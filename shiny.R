library(shiny)
library(DT)
library(ggplot2)
library(reshape2)
library(bruceR)

# Helper function: Generate one-hot encoded matrix
generate_matrix <- function(num_levels, num_trials, pattern) {
  if (pattern == "Loop") {
    mat <- matrix(0, nrow = num_trials, ncol = num_levels)
    for (i in 1:num_trials) mat[i, (i - 1) %% num_levels + 1] <- 1
  } else if (pattern == "Random") {
    mat <- matrix(0, nrow = num_trials, ncol = num_levels)
    random_indices <- sample(1:num_trials, size = num_levels, replace = TRUE)
    mat[cbind(random_indices, 1:num_levels)] <- 1
  }
  return(mat)
}

# UI
ui <- fluidPage(
  titlePanel("Experimental Parameterization Framework"),
  
  # Initialization of NUM_TRIALS and NUM_ARMS
  fluidRow(
    column(6, numericInput("num_trials", "Number of Trials (NUM_TRIALS):", value = 10, min = 1)),
    column(6, numericInput("num_arms", "Number of Arms (NUM_ARMS):", value = 5, min = 1))
  ),
  
  # Top: Reward Matrix Visualization
  fluidRow(
    column(12, 
           h3("Reward Matrix Heatmap"),
           plotOutput("reward_matrix", height = "300px")
    )
  ),
  
  # Middle: State and Arm Features Configuration
  fluidRow(
    column(6, 
           h4("State Features"),
           actionButton("add_state", "Add State Feature"),
           actionButton("remove_state", "Remove State Feature"),
           tags$br(), tags$br(),
           DTOutput("state_table")
    ),
    column(6, 
           h4("Arm Features"),
           actionButton("add_arm", "Add Arm Feature"),
           actionButton("remove_arm", "Remove Arm Feature"),
           tags$br(), tags$br(),
           DTOutput("arm_table")
    )
  ),
  
  # Bottom: Link Matrix and Submit
  fluidRow(
    column(12, 
           h4("Link Matrix Configuration"),
           actionButton("add_link", "Add Link"),
           actionButton("remove_link", "Remove Link"),
           tags$br(), tags$br(),
           DTOutput("link_table"),
           tags$hr(),
           actionButton("submit", "Submit"),
           downloadButton("save", "Save Configuration")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Add pattern choices constant
  pattern_choices <- c("Loop", "Random")
  link_function_choices <- c("Linear", "Random", "Inverse")  # Expandable link functions
  
  # Reactive: Initialize State and Arm Features
  state_data <- reactiveVal(data.frame(
    Name = c("Game", "Time"),
    Levels = c(1, isolate(input$num_trials)),  # Use isolate to avoid direct reactive access
    Pattern = c("Loop", "Loop"),
    stringsAsFactors = FALSE
  ))
  
  arm_data <- reactiveVal(data.frame(
    Name = c("Position"),
    Levels = c(isolate(input$num_arms)),  # Use isolate to avoid direct reactive access
    Pattern = c("Loop"),
    stringsAsFactors = FALSE
  ))
  
  link_data <- reactiveVal(data.frame(
    State_Feature = c("Time"),
    Link_Function = c("Linear"),
    Arm_Feature = c("Position"),
    stringsAsFactors = FALSE
  ))
  
  # Observe changes to NUM_TRIALS and NUM_ARMS, and update State/Arm data
  observeEvent(input$num_trials, {
    state_data(data.frame(
      Name = c("Game", "Time"),
      Levels = c(1, input$num_trials),
      Pattern = c("Loop", "Loop"),
      stringsAsFactors = FALSE
    ))
  })
  
  observeEvent(input$num_arms, {
    arm_data(data.frame(
      Name = c("Position"),
      Levels = c(input$num_arms),
      Pattern = c("Loop"),
      stringsAsFactors = FALSE
    ))
  })
  
  # Generate State and Arm Matrices
  state_matrices <- reactive({
    lapply(1:nrow(state_data()), function(i) {
      generate_matrix(as.numeric(state_data()$Levels[i]), input$num_trials, state_data()$Pattern[i])
    })
  })
  
  arm_matrices <- reactive({
    lapply(1:nrow(arm_data()), function(i) {
      generate_matrix(as.numeric(arm_data()$Levels[i]), input$num_arms, arm_data()$Pattern[i])
    })
  })
  
  # Generate Reward Matrix based on Link Matrix
  reward_matrix <- eventReactive(input$submit, {
    links <- link_data()
    final_matrix <- matrix(0, nrow = input$num_trials, ncol = input$num_arms)
    
    for (i in 1:nrow(links)) {
      state_idx <- which(state_data()$Name == links$State_Feature[i])
      arm_idx <- which(arm_data()$Name == links$Arm_Feature[i])
      
      if (length(state_idx) == 0 || length(arm_idx) == 0) next
      
      state_matrix <- state_matrices()[[state_idx]]
      arm_matrix <- arm_matrices()[[arm_idx]]
      
      # Apply link function
      if (links$Link_Function[i] == "Linear") {
        link_matrix <- outer(seq(1, 0, length.out = input$num_trials), seq(0, 1, length.out = input$num_arms))
      } else if (links$Link_Function[i] == "Random") {
        link_matrix <- matrix(runif(input$num_trials * input$num_arms), nrow = input$num_trials, ncol = input$num_arms)
      } else if (links$Link_Function[i] == "Inverse") {
        link_matrix <- outer(1 / (1:input$num_trials), 1 / (1:input$num_arms))
      } else {
        link_matrix <- matrix(0, nrow = input$num_trials, ncol = input$num_arms)
      }
      
      # Calculate partial reward matrix
      partial_matrix <- state_matrix %*% link_matrix %*% t(arm_matrix)
      final_matrix <- final_matrix + partial_matrix
    }
    
    # Normalize to 0-100 range
    if (max(final_matrix) > 0) {
      final_matrix <- 100 * (final_matrix - min(final_matrix)) / (max(final_matrix) - min(final_matrix))
    }
    return(final_matrix)
  })
  
  # Render Reward Matrix Heatmap
  output$reward_matrix <- renderPlot({
    mat <- reward_matrix()
    if (is.null(mat)) return()
    heatmap_data <- melt(mat)
    colnames(heatmap_data) <- c("Trial", "Arm", "Value")
    ggplot(heatmap_data, aes(x = Trial, y = Arm, fill = Value)) +
      geom_tile() +
      scale_fill_viridis_c() +
      scale_x_continuous(breaks = if (input$num_trials > 20) seq(1, input$num_trials, length.out = 20) else 1:input$num_trials,
                         labels = if (input$num_trials > 20) round(seq(1, input$num_trials, length.out = 20)) else 1:input$num_trials) +
      scale_y_continuous(breaks = 1:input$num_arms, labels = 1:input$num_arms) +
      theme_minimal() +
      labs(x = "Trial", y = "Arm", fill = "Reward") +
      theme_bruce()
  })
  
  # Render Tables
  output$state_table <- renderDT({datatable(state_data(), editable = TRUE, options = list(dom = 't', paging = FALSE))})
  output$arm_table <- renderDT({datatable(arm_data(), editable = TRUE, options = list(dom = 't', paging = FALSE))})
  output$link_table <- renderDT({datatable(link_data(), editable = TRUE, options = list(dom = 't', paging = FALSE))})
}

# Run App
shinyApp(ui, server)