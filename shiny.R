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
    Levels = c(isolate(input$num_trials)),  # Use isolate to avoid direct reactive access
    Pattern = c("Loop", "Loop"),
    stringsAsFactors = FALSE
  ))
  
  arm_data <- reactiveVal(data.frame(
    Name = c("Position"),
    Levels = c(isolate(input$num_arms)),  # Use isolate to avoid direct reactive access
    Pattern = c("Loop"),
    stringsAsFactors = FALSE
  ))
  
  # State Features Table
  observeEvent(input$add_state, {
    current_data <- state_data()
    new_row <- data.frame(Name = "", Levels = "", Pattern = "Loop")
    updated_data <- rbind(current_data, new_row)
    state_data(updated_data)
  })
  
  observeEvent(input$remove_state, {
    current_data <- state_data()
    if (nrow(current_data) > 2) {
      updated_data <- current_data[-nrow(current_data), ]
      state_data(updated_data)
    }
  })
  
  output$state_table <- renderDT({
    datatable(
      state_data(),
      editable = TRUE,
      rownames = FALSE,
      options = list(dom = 't', paging = FALSE)
    )
  }, server = FALSE)
  
  proxy_state <- dataTableProxy("state_table")
  observeEvent(input$state_table_cell_edit, {
    info <- input$state_table_cell_edit
    current_data <- state_data()
    current_data[info$row, info$col + 1] <- info$value  # Adjust column index for editing
    state_data(current_data)
  })
  
  # Arm Features Table
  observeEvent(input$add_arm, {
    current_data <- arm_data()
    new_row <- data.frame(Name = "", Levels = "", Pattern = "Loop")
    updated_data <- rbind(current_data, new_row)
    arm_data(updated_data)
  })
  
  observeEvent(input$remove_arm, {
    current_data <- arm_data()
    if (nrow(current_data) > 1) {
      updated_data <- current_data[-nrow(current_data), ]
      arm_data(updated_data)
    }
  })
  
  output$arm_table <- renderDT({
    datatable(
      arm_data(),
      editable = TRUE,
      rownames = FALSE,
      options = list(dom = 't', paging = FALSE)
    )
  }, server = FALSE)
  
  proxy_arm <- dataTableProxy("arm_table")
  observeEvent(input$arm_table_cell_edit, {
    info <- input$arm_table_cell_edit
    current_data <- arm_data()
    current_data[info$row, info$col + 1] <- info$value  # Adjust column index for editing
    arm_data(current_data)
  })
  
  # Link Matrix Table
  link_data <- reactiveVal(data.frame(
    State_Feature = character(),
    Link_Function = character(),
    Arm_Feature = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$add_link, {
    current_data <- link_data()
    new_row <- data.frame(State_Feature = "", Link_Function = "", Arm_Feature = "")
    updated_data <- rbind(current_data, new_row)
    link_data(updated_data)
  })
  
  observeEvent(input$remove_link, {
    current_data <- link_data()
    if (nrow(current_data) > 0) {
      updated_data <- current_data[-nrow(current_data), ]
      link_data(updated_data)
    }
  })
  
  output$link_table <- renderDT({
    datatable(
      link_data(),
      editable = TRUE,
      rownames = FALSE,
      options = list(dom = 't', paging = FALSE)
    )
  }, server = FALSE)
  
  # Reward Matrix Heatmap
  reward_matrix <- reactive({
    matrix(runif(input$num_trials * input$num_arms, 0, 100), nrow = input$num_trials, ncol = input$num_arms)
  })
  
  output$reward_matrix <- renderPlot({
    mat <- reward_matrix()
    heatmap_data <- melt(mat)
    colnames(heatmap_data) <- c("Trial", "Arm", "Value")
    ggplot(heatmap_data, aes(x = Trial, y = Arm, fill = Value)) +
      geom_tile() +
      scale_fill_gradient(low = "yellow", high = "red") +
      theme_minimal() +
      labs(x = "Trial", y = "Arm", fill = "Reward")
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