library(shiny)
library(DT)
library(ggplot2)
library(reshape2)
library(bruceR)
library(shinyjs)

# Helper Functions
generate_variable_matrix <- function(levels, pattern, num_trials, num_arms) {
  mat <- matrix(0, nrow = num_trials, ncol = num_trials)
  valid_size <- min(levels, num_trials)
  
  if (pattern == "Loop") {
    for (i in 1:num_trials) {
      row_idx <- ((i - 1) %% valid_size) + 1
      mat[row_idx, i] <- 1
    }
  } else if (pattern == "Random") {
    for (i in 1:num_trials) {
      row_idx <- sample(1:valid_size, 1)
      mat[row_idx, i] <- 1
    }
  }
  
  return(mat)
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

generate_distribution_matrix <- function(state_levels, arm_levels, num_trials, num_arms,
                                         state_dist_type, arm_dist_type) {
  state_seq <- generate_sequence(state_levels, state_dist_type)
  arm_seq <- generate_sequence(arm_levels, arm_dist_type)
  
  dist_matrix <- outer(state_seq, arm_seq, "+")
  dist_matrix <- (dist_matrix - min(dist_matrix)) / (max(dist_matrix) - min(dist_matrix)) * 100
  padded_matrix <- matrix(0, nrow = num_trials, ncol = num_arms)
  padded_matrix[1:state_levels, 1:arm_levels] <- dist_matrix
  
  return(padded_matrix)
}

summary_reward_distribution <- function(state_matrices, distribution_matrices, arm_matrices, link_data, num_trials, num_arms) {
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

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Integrative Bandit Parameterization Interface"),
  
  fluidRow(
    column(4, numericInput("num_trials", "Number of Trials", value = 10, min = 1)),
    column(4, numericInput("num_arms", "Number of Arms", value = 5, min = 1)),
    column(4, numericInput("seed", "Random Seed", value = 42, min = 1))
  ),
  
  fluidRow(
    column(12, h3("Demo Reward Matrix Heatmap"), plotOutput("reward_matrix", height = "300px"))
  ),
  
  fluidRow(
    column(6, h4("State Variables Configuration"),
           fluidRow(
             column(3, textInput("state_name", "Name")),
             column(3, numericInput("state_levels", "# Levels", value = 1, min = 1)),
             column(3, selectInput("state_pattern", "Pattern", choices = c("Loop", "Random"))),
             column(3, actionButton("add_state", "Add", class = "btn-info"))
           ),
           DTOutput("state_table")),
    column(6, h4("Arm Variables Configuration"),
           fluidRow(
             column(3, textInput("arm_name", "Name")),
             column(3, numericInput("arm_levels", "# Levels", value = 1, min = 1)),
             column(3, selectInput("arm_pattern", "Pattern", choices = c("Loop", "Random"))),
             column(3, actionButton("add_arm", "Add", class = "btn-info"))
           ),
           DTOutput("arm_table"))
  ),
  
  fluidRow(
    column(12, h4("Reward Distribution Configuration"),
           fluidRow(
             column(3, selectInput("link_state", "State Variable", choices = NULL)),
             column(3, selectInput("link_state_function", "State Distribution", choices = c("Identical", "Independent", "Monotonic"))),
             column(3, selectInput("link_arm_function", "Arm Distribution", choices = c("Identical", "Independent", "Monotonic"))),
             column(3, selectInput("link_arm", "Arm Variable", choices = NULL)),
             column(3, actionButton("add_link", "Add", class = "btn-info"))
           ),
           DTOutput("link_table"))
  ),
  
  fluidRow(
    column(12, actionButton("update_demo", "Update Demo", class = "btn-primary"))
  )
)

# Server
server <- function(input, output, session) {
  state_matrices <- reactiveVal(list())
  arm_matrices <- reactiveVal(list())
  distribution_matrices <- reactiveVal(list())
  link_data <- reactiveVal(data.frame(State_Variable = character(), Arm_Variable = character(),
                                      stringsAsFactors = FALSE))
  
  observeEvent(input$add_state, {
    new_state <- list(
      Name = input$state_name,
      Levels = input$state_levels,
      Pattern = input$state_pattern
    )
    mat <- generate_variable_matrix(input$state_levels, input$state_pattern, input$num_trials, input$num_arms)
    state_matrices(modifyList(state_matrices(), setNames(list(mat), input$state_name)))
  })
  
  observeEvent(input$add_arm, {
    new_arm <- list(
      Name = input$arm_name,
      Levels = input$arm_levels,
      Pattern = input$arm_pattern
    )
    mat <- generate_variable_matrix(input$arm_levels, input$arm_pattern, input$num_trials, input$num_arms)
    arm_matrices(modifyList(arm_matrices(), setNames(list(mat), input$arm_name)))
  })
  
  observeEvent(input$add_link, {
    new_link <- data.frame(
      State_Variable = input$link_state,
      Arm_Variable = input$link_arm,
      stringsAsFactors = FALSE
    )
    dist_mat <- generate_distribution_matrix(
      state_levels = input$state_levels,
      arm_levels = input$arm_levels,
      num_trials = input$num_trials,
      num_arms = input$num_arms,
      state_dist_type = input$link_state_function,
      arm_dist_type = input$link_arm_function
    )
    distribution_matrices(modifyList(distribution_matrices(), setNames(list(dist_mat), paste(input$link_state, input$link_arm, sep = "_"))))
    link_data(rbind(link_data(), new_link))
  })
  
  output$reward_matrix <- renderPlot({
    final_mat <- summary_reward_distribution(state_matrices(), distribution_matrices(), arm_matrices(), link_data(), input$num_trials, input$num_arms)
    heatmap_data <- data.frame(
      Trial = rep(1:nrow(final_mat), ncol(final_mat)),
      Arm = rep(1:ncol(final_mat), each = nrow(final_mat)),
      Value = as.vector(final_mat)
    )
    ggplot(heatmap_data, aes(x = Trial, y = Arm, fill = Value)) +
      geom_tile() +
      scale_fill_viridis_c(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "Trial", y = "Arm", fill = "Reward")
  })
}

shinyApp(ui, server)