library(shiny)
library(DT)
library(ggplot2)
library(reshape2)
library(bruceR)
library(shinyjs)

# Helper Functions ------------------------------------------------------

create_variable_matrix <- function(levels, pattern, num_trials, num_arms) {
  if (pattern == "Loop") {
    values <- rep(1:levels, length.out = num_trials)
    matrix <- matrix(0, nrow = num_trials, ncol = levels)
    for (i in 1:num_trials) {
      matrix[i, values[i]] <- 1
    }
  } else {
    values <- sample(1:levels, num_trials, replace = TRUE)
    matrix <- matrix(0, nrow = num_trials, ncol = levels)
    for (i in 1:num_trials) {
      matrix[i, values[i]] <- 1
    }
  }
  return(matrix)
}

generate_sequence <- function(n_levels, distribution_type) {
  if (distribution_type == "Identical") {
    rep(runif(1, 0, 100), n_levels)
  } else if (distribution_type == "Independent") {
    runif(n_levels, 0, 100)
  } else if (distribution_type == "Monotonic") {
    bounds <- sort(runif(2, 0, 100))
    seq(bounds[1], bounds[2], length.out = n_levels)
  }
}

create_distribution_matrix <- function(state_levels, arm_levels, num_trials, num_arms,
                                       state_dist_type, arm_dist_type) {
  state_seq <- generate_sequence(state_levels, state_dist_type)
  arm_seq <- generate_sequence(arm_levels, arm_dist_type)
  dist_matrix <- outer(state_seq, arm_seq, "+")
  dist_matrix <- (dist_matrix - min(dist_matrix)) / (max(dist_matrix) - min(dist_matrix)) * 100
  padded_matrix <- matrix(0, nrow = num_trials, ncol = num_arms)
  padded_matrix[1:state_levels, 1:arm_levels] <- dist_matrix
  return(padded_matrix)
}

summary_reward_distribution <- function(state_matrices, distribution_matrices, arm_matrices, 
                                        link_data, num_trials, num_arms) {
  if (length(distribution_matrices) == 0) return(matrix(0, nrow = num_trials, ncol = num_arms))
  
  final_matrix <- matrix(0, nrow = num_trials, ncol = num_arms)
  for (i in 1:nrow(link_data)) {
    state_var <- link_data$State_Variable[i]
    arm_var <- link_data$Arm_Variable[i]
    state_matrix <- state_matrices[[state_var]]
    dist_matrix <- distribution_matrices[[paste(state_var, arm_var, sep = "_")]]
    arm_matrix <- arm_matrices[[arm_var]]
    temp_matrix <- state_matrix %*% dist_matrix
    pair_matrix <- temp_matrix %*% t(arm_matrix)
    final_matrix <- final_matrix + pair_matrix
  }
  
  final_matrix <- (final_matrix - min(final_matrix)) / (max(final_matrix) - min(final_matrix)) * 100
  return(final_matrix)
}

# UI --------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Integrative Bandit Parameterization Interface"),
  
  # Initialization of NUM_TRIALS, NUM_ARMS, and SEED
  fluidRow(
    column(4, numericInput("num_trials", "Number of Trials", value = 10, min = 1)),
    column(4, numericInput("num_arms", "Number of Arms", value = 5, min = 1)),
    column(4, numericInput("seed", "Random Seed", value = 42, min = 1))
  ),
  
  # Top: Reward Matrix Visualization
  fluidRow(
    column(12, 
           h3("Demo Reward Matrix Heatmap"),
           plotOutput("reward_matrix", height = "300px")
    )
  ),
  
  # State and Arm Variables Configuration (side by side)
  fluidRow(
    # State Variables Configuration
    column(6,
           h4("State Variables Configuration"),
           fluidRow(
             column(3, textInput("state_name", "Name")),
             column(3, numericInput("state_levels", "# Levels", value = 1, min = 1)),
             column(3, selectInput("state_pattern", "Pattern", choices = c("Loop", "Random"))),
             column(3, div(style = "margin-top: 25px;", 
                           actionButton("add_state", "Add", class = "btn-info")))
           ),
           DTOutput("state_table")
    ),
    
    # Arm Variables Configuration
    column(6,
           h4("Arm Variables Configuration"),
           fluidRow(
             column(3, textInput("arm_name", "Name")),
             column(3, numericInput("arm_levels", "# Levels", value = 1, min = 1)),
             column(3, selectInput("arm_pattern", "Pattern", choices = c("Loop", "Random"))),
             column(3, div(style = "margin-top: 25px;",
                           actionButton("add_arm", "Add", class = "btn-info")))
           ),
           DTOutput("arm_table")
    )
  ),
  
  # Reward Distribution Configuration
  fluidRow(
    column(12,
           h4("Reward Distribution Configuration"),
           fluidRow(
             column(3, 
                    selectInput("link_state", "State Variable", choices = NULL),
                    style = "padding-right: 5px; width: 21%;"),
             column(3, 
                    selectInput("link_state_function", "State Distribution", 
                                choices = c("Identical", "Independent", "Monotonic")),
                    style = "padding-right: 5px; width: 21%;"),
             column(1,
                    div(style = "margin-top: 25px; width: 100%; text-align: center;",
                        actionButton("link_distributions", "",
                                     icon = icon("link"),
                                     class = "btn-sm",
                                     style = "padding: 6px 8px;")),
                    style = "width: 8%;"),
             column(3, 
                    selectInput("link_arm_function", "Arm Distribution", 
                                choices = c("Identical", "Independent", "Monotonic")),
                    style = "padding-left: 5px; width: 21%;"),
             column(2, 
                    selectInput("link_arm", "Arm Variable", choices = NULL),
                    style = "padding-left: 5px; width: 21%;"),
             column(1, 
                    div(style = "margin-top: 25px;", 
                        actionButton("add_link", "Add", 
                                     class = "btn-info btn-sm",
                                     style = "width: 50px;")),
                    style = "width: 8%;")
           ),
           div(style = "padding: 0 15px;",
               DTOutput("link_table"))
    )
  ),
  
  # Bottom buttons
  fluidRow(
    column(12,
           div(style = "margin-top: 20px; text-align: center;",
               actionButton("update_demo", "Update Demo", 
                            class = "btn-primary",
                            style = "margin-right: 10px"),
               downloadButton("save_config", "Save Configuration",
                              class = "btn-success")
           )
    )
  )
)


# Server ----------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive values to store matrices and data
  state_matrices <- reactiveVal(list())
  arm_matrices <- reactiveVal(list())
  distribution_matrices <- reactiveVal(list())
  link_data <- reactiveVal(data.frame(
    State_Variable = character(),
    State_Distribution = character(),
    Interaction = character(),
    Arm_Distribution = character(),
    Arm_Variable = character(),
    stringsAsFactors = FALSE
  ))
  
  # Initialize default state and arm data
  state_data <- reactiveVal(data.frame(
    Name = c("Overall", "Time"),
    Levels = c(1, input$num_trials),
    Pattern = c("Loop", "Loop"),
    stringsAsFactors = FALSE
  ))
  
  arm_data <- reactiveVal(data.frame(
    Name = c("Index"),
    Levels = c(input$num_arms),
    Pattern = c("Loop"),
    stringsAsFactors = FALSE
  ))
  
  # Update "Time" row levels when num_trials changes
  observeEvent(input$num_trials, {
    current_data <- state_data()
    time_row <- which(current_data$Name == "Time")
    if (length(time_row) > 0) {
      current_data$Levels[time_row] <- input$num_trials
      state_data(current_data)
    }
  })
  
  # Update "Index" row levels when num_arms changes
  observeEvent(input$num_arms, {
    current_data <- arm_data()
    index_row <- which(current_data$Name == "Index")
    if (length(index_row) > 0) {
      current_data$Levels[index_row] <- input$num_arms
      arm_data(current_data)
    }
  })
  
  # Add or update state variable
  observeEvent(input$add_state, {
    new_state <- data.frame(
      Name = input$state_name,
      Levels = input$state_levels,
      Pattern = input$state_pattern,
      stringsAsFactors = FALSE
    )
    current_data <- state_data()
    duplicate_idx <- which(current_data$Name == input$state_name)
    if (length(duplicate_idx) > 0) {
      current_data[duplicate_idx, ] <- new_state
    } else {
      current_data <- rbind(current_data, new_state)
    }
    state_data(current_data)
    updateSelectInput(session, "link_state", choices = state_data()$Name)
    
    # Generate and store matrix
    mat <- create_variable_matrix(input$state_levels, input$state_pattern, input$num_trials, input$num_arms)
    state_matrices(modifyList(state_matrices(), setNames(list(mat), input$state_name)))
  })
  
  # Add or update arm variable
  observeEvent(input$add_arm, {
    new_arm <- data.frame(
      Name = input$arm_name,
      Levels = input$arm_levels,
      Pattern = input$arm_pattern,
      stringsAsFactors = FALSE
    )
    current_data <- arm_data()
    duplicate_idx <- which(current_data$Name == input$arm_name)
    if (length(duplicate_idx) > 0) {
      current_data[duplicate_idx, ] <- new_arm
    } else {
      current_data <- rbind(current_data, new_arm)
    }
    arm_data(current_data)
    updateSelectInput(session, "link_arm", choices = arm_data()$Name)
    
    # Generate and store matrix
    mat <- create_variable_matrix(input$arm_levels, input$arm_pattern, input$num_trials, input$num_arms)
    arm_matrices(modifyList(arm_matrices(), setNames(list(mat), input$arm_name)))
  })
  
  # Add reward distribution link
  observeEvent(input$add_link, {
    new_link <- data.frame(
      State_Variable = input$link_state,
      State_Distribution = input$link_state_function,
      Interaction = "+",  # Default interaction type, extendable
      Arm_Distribution = input$link_arm_function,
      Arm_Variable = input$link_arm,
      stringsAsFactors = FALSE
    )
    current_links <- link_data()
    link_data(rbind(current_links, new_link))
    
    # Generate and store distribution matrix
    state_levels <- state_data()$Levels[state_data()$Name == input$link_state]
    arm_levels <- arm_data()$Levels[arm_data()$Name == input$link_arm]
    dist_mat <- create_distribution_matrix(
      state_levels = state_levels,
      arm_levels = arm_levels,
      num_trials = input$num_trials,
      num_arms = input$num_arms,
      state_dist_type = input$link_state_function,
      arm_dist_type = input$link_arm_function
    )
    key <- paste(input$link_state, input$link_arm, sep = "_")
    distribution_matrices(modifyList(distribution_matrices(), setNames(list(dist_mat), key)))
  })
  
  # Remove state variable and related links
  observeEvent(input$state_name, {
    state_name <- input$state_name
    if (!state_name %in% c("Overall", "Time")) {
      current_data <- state_data()
      state_data(current_data[current_data$Name != state_name, ])
      state_matrices(state_matrices()[!names(state_matrices()) %in% state_name])
      
      # Remove related links and matrices
      link_data(link_data()[link_data()$State_Variable != state_name, ])
      dist_keys <- grep(state_name, names(distribution_matrices()), value = TRUE)
      distribution_matrices(distribution_matrices()[!names(distribution_matrices()) %in% dist_keys])
    }
  })
  
  # Remove arm variable and related links
  observeEvent(input$arm_name, {
    arm_name <- input$arm_name
    if (arm_name != "Index") {
      current_data <- arm_data()
      arm_data(current_data[current_data$Name != arm_name, ])
      arm_matrices(arm_matrices()[!names(arm_matrices()) %in% arm_name])
      
      # Remove related links and matrices
      link_data(link_data()[link_data()$Arm_Variable != arm_name, ])
      dist_keys <- grep(arm_name, names(distribution_matrices()), value = TRUE)
      distribution_matrices(distribution_matrices()[!names(distribution_matrices()) %in% dist_keys])
    }
  })
  
  # Render state table
  output$state_table <- renderDT({
    data <- state_data()
    data$Operation <- sapply(1:nrow(data), function(i) {
      if (data$Name[i] %in% c("Overall", "Time")) "" else 
        sprintf('<button onclick="Shiny.setInputValue(\'state_name\', \'%s\')" class="btn btn-danger btn-sm">Remove</button>', data$Name[i])
    })
    datatable(data, escape = FALSE, selection = "none", rownames = FALSE, options = list(dom = 't', paging = FALSE))
  })
  
  # Render arm table
  output$arm_table <- renderDT({
    data <- arm_data()
    data$Operation <- sapply(1:nrow(data), function(i) {
      if (data$Name[i] == "Index") "" else 
        sprintf('<button onclick="Shiny.setInputValue(\'arm_name\', \'%s\')" class="btn btn-danger btn-sm">Remove</button>', data$Name[i])
    })
    datatable(data, escape = FALSE, selection = "none", rownames = FALSE, options = list(dom = 't', paging = FALSE))
  })
  
  # Render link table
  output$link_table <- renderDT({
    data <- link_data()
    if (nrow(data) == 0) return(NULL)
    data$Operation <- sapply(1:nrow(data), function(i) {
      sprintf('<button onclick="Shiny.setInputValue(\'remove_link\', \'%s|%s\')" class="btn btn-danger btn-sm">Remove</button>',
              data$State_Variable[i], data$Arm_Variable[i])
    })
    datatable(data, escape = FALSE, selection = "none", rownames = FALSE, options = list(dom = 't', paging = FALSE))
  })
  
  # Render reward matrix heatmap
  output$reward_matrix <- renderPlot({
    final_matrix <- summary_reward_distribution(state_matrices(), distribution_matrices(), arm_matrices(), link_data(), input$num_trials, input$num_arms)
    heatmap_data <- data.frame(
      Trial = rep(1:nrow(final_matrix), ncol(final_matrix)),
      Arm = rep(1:ncol(final_matrix), each = nrow(final_matrix)),
      Value = as.vector(final_matrix)
    )
    ggplot(heatmap_data, aes(x = Trial, y = Arm, fill = Value)) +
      geom_tile() +
      scale_fill_viridis_c(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "Trial", y = "Arm", fill = "Reward")
  })
}
# Run App ---------------------------------------------------------------

shinyApp(ui, server)