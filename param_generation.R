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
  
  # State Features Table
  observeEvent(input$add_state, {
    state_data(rbind(state_data(), data.frame(Name = "", Levels = "", Pattern = "")))
  })
  
  observeEvent(input$remove_state, {
    data <- state_data()
    if (nrow(data) > 2) state_data(data[-nrow(data), ])
  })
  
  output$state_table <- renderDT({
    datatable(state_data(), editable = TRUE, rownames = FALSE, options = list(dom = 't', paging = FALSE))
  }, server = FALSE)
  
  # Arm Features Table
  observeEvent(input$add_arm, {
    arm_data(rbind(arm_data(), data.frame(Name = "", Levels = "", Pattern = "")))
  })
  
  observeEvent(input$remove_arm, {
    data <- arm_data()
    if (nrow(data) > 1) arm_data(data[-nrow(data), ])
  })
  
  output$arm_table <- renderDT({
    datatable(arm_data(), editable = TRUE, rownames = FALSE, options = list(dom = 't', paging = FALSE))
  }, server = FALSE)
  
  # Link Matrix Table
  link_data <- reactiveVal(data.frame(
    State_Feature = character(),
    Link_Function = character(),
    Arm_Feature = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$add_link, {
    link_data(rbind(link_data(), data.frame(
      State_Feature = "", Link_Function = "", Arm_Feature = ""
    )))
  })
  
  observeEvent(input$remove_link, {
    data <- link_data()
    if (nrow(data) > 0) link_data(data[-nrow(data), ])
  })
  
  output$link_table <- renderDT({
    datatable(link_data(), editable = TRUE, rownames = FALSE, options = list(dom = 't', paging = FALSE))
  }, server = FALSE)
  
  # Reward Matrix Heatmap
  reward_matrix <- reactive({
    # Dummy reward matrix for demonstration
    matrix(runif(input$num_trials * input$num_arms, 0, 100), nrow = input$num_trials, ncol = input$num_arms)
  })
  
  output$reward_matrix <- renderPlot({
    mat <- reward_matrix()
    heatmap_data <- melt(mat)
    colnames(heatmap_data) <- c("Trial", "Arm", "Value")
    ggplot(heatmap_data, aes(x = Trial, y = Arm, fill = Value)) +
      geom_tile() +
      scale_fill_viridis_c() +
      scale_x_continuous(breaks = if(input$num_trials > 20) seq(1, input$num_trials, length.out = 20) else 1:input$num_trials,
                        labels = if(input$num_trials > 20) round(seq(1, input$num_trials, length.out = 20)) else 1:input$num_trials) +
      scale_y_continuous(breaks = 1:input$num_arms, labels = 1:input$num_arms) +
      theme_minimal() +
      labs(x = "Trial", y = "Arm", fill = "Reward") +
      theme_bruce()
  })
  
  # Save Configuration
  output$save <- downloadHandler(
    filename = function() {
      paste("configuration", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(
        list(
          State_Features = state_data(),
          Arm_Features = arm_data(),
          Link_Matrix = link_data()
        ),
        file
      )
    }
  )
}

# Run App
shinyApp(ui, server)