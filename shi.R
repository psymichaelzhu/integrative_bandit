library(shiny)
library(DT)
library(ggplot2)
library(reshape2)
library(bruceR)

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
           h4("State Features Configuration"),
           textInput("state_name", "State Name:"),
           numericInput("state_levels", "Number of Levels:", value = 1, min = 1),
           selectInput("state_pattern", "Pattern:", choices = c("Loop", "Random")),
           actionButton("add_state", "Add State Feature"),
           DTOutput("state_table"),
           actionButton("remove_state", "Remove Selected State")
    ),
    column(6, 
           h4("Arm Features Configuration"),
           textInput("arm_name", "Arm Name:"),
           numericInput("arm_levels", "Number of Levels:", value = 1, min = 1),
           selectInput("arm_pattern", "Pattern:", choices = c("Loop", "Random")),
           actionButton("add_arm", "Add Arm Feature"),
           DTOutput("arm_table"),
           actionButton("remove_arm", "Remove Selected Arm")
    )
  ),
  
  # Bottom: Link Matrix and Submit
  fluidRow(
    column(12, 
           h4("Link Matrix Configuration"),
           selectInput("link_state", "State Feature:", choices = NULL),
           textInput("link_function", "Link Function:"),
           selectInput("link_arm", "Arm Feature:", choices = NULL),
           actionButton("add_link", "Add Link"),
           DTOutput("link_table"),
           actionButton("remove_link", "Remove Selected Link"),
           tags$hr(),
           actionButton("submit", "Submit"),
           downloadButton("save", "Save Configuration")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive: Initialize State, Arm, and Link Data
  state_data <- reactiveVal(data.frame(
    Name = character(),
    Levels = numeric(),
    Pattern = character(),
    stringsAsFactors = FALSE
  ))
  
  arm_data <- reactiveVal(data.frame(
    Name = character(),
    Levels = numeric(),
    Pattern = character(),
    stringsAsFactors = FALSE
  ))
  
  link_data <- reactiveVal(data.frame(
    State_Feature = character(),
    Link_Function = character(),
    Arm_Feature = character(),
    stringsAsFactors = FALSE
  ))
  
  # Add State Feature
  observeEvent(input$add_state, {
    new_state <- data.frame(
      Name = input$state_name,
      Levels = input$state_levels,
      Pattern = input$state_pattern,
      stringsAsFactors = FALSE
    )
    state_data(rbind(state_data(), new_state))
    updateSelectInput(session, "link_state", choices = state_data()$Name)
  })
  
  # Add Arm Feature
  observeEvent(input$add_arm, {
    new_arm <- data.frame(
      Name = input$arm_name,
      Levels = input$arm_levels,
      Pattern = input$arm_pattern,
      stringsAsFactors = FALSE
    )
    arm_data(rbind(arm_data(), new_arm))
    updateSelectInput(session, "link_arm", choices = arm_data()$Name)
  })
  
  # Add Link
  observeEvent(input$add_link, {
    new_link <- data.frame(
      State_Feature = input$link_state,
      Link_Function = input$link_function,
      Arm_Feature = input$link_arm,
      stringsAsFactors = FALSE
    )
    link_data(rbind(link_data(), new_link))
  })
  
  # Remove Selected State
  observeEvent(input$remove_state, {
    selected <- input$state_table_rows_selected
    if (length(selected) > 0) {
      state_data(state_data()[-selected, ])
      updateSelectInput(session, "link_state", choices = state_data()$Name)
    }
  })
  
  # Remove Selected Arm
  observeEvent(input$remove_arm, {
    selected <- input$arm_table_rows_selected
    if (length(selected) > 0) {
      arm_data(arm_data()[-selected, ])
      updateSelectInput(session, "link_arm", choices = arm_data()$Name)
    }
  })
  
  # Remove Selected Link
  observeEvent(input$remove_link, {
    selected <- input$link_table_rows_selected
    if (length(selected) > 0) {
      link_data(link_data()[-selected, ])
    }
  })
  
  # Render Tables
  output$state_table <- renderDT({
    datatable(state_data(), selection = "single", rownames = FALSE, options = list(dom = 't', paging = FALSE))
  })
  
  output$arm_table <- renderDT({
    datatable(arm_data(), selection = "single", rownames = FALSE, options = list(dom = 't', paging = FALSE))
  })
  
  output$link_table <- renderDT({
    datatable(link_data(), selection = "single", rownames = FALSE, options = list(dom = 't', paging = FALSE))
  })
  
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
      scale_fill_viridis_c() +
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