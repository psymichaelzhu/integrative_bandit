library(shiny)
library(DT)
library(ggplot2)
library(reshape2)
library(bruceR)

# UI
ui <- fluidPage(
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
           h3("Reward Matrix Heatmap"),
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
             column(3, div(style = "margin-top: 25px;", actionButton("add_state", "Add")))
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
             column(3, div(style = "margin-top: 25px;", actionButton("add_arm", "Add")))
           ),
           DTOutput("arm_table")
    )
  ),
  
  # Link Matrix Configuration
  fluidRow(
    column(12,
           h4("Link Matrix Configuration"),
           fluidRow(
             column(3, selectInput("link_state", "State Feature", choices = NULL)),
             column(3, selectInput("link_function", "Link Function", 
                                 choices = c("linear", "random", "correlation"))),
             column(3, selectInput("link_arm", "Arm Feature", choices = NULL)),
             column(3, div(style = "margin-top: 25px;", actionButton("add_link", "Add")))
           ),
           DTOutput("link_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Update default rows when num_trials or num_arms changes
  observeEvent(input$num_trials, {
    current_data <- state_data()
    time_row <- which(current_data$Name == "Time")
    if (length(time_row) > 0) {
      current_data$Levels[time_row] <- input$num_trials
      state_data(current_data)
    }
  })

  observeEvent(input$num_arms, {
    current_data <- arm_data()
    position_row <- which(current_data$Name == "Position")
    if (length(position_row) > 0) {
      current_data$Levels[position_row] <- input$num_arms
      arm_data(current_data)
    }
  })

  # Initialize State and Arm Data with Default Rows
  state_data <- reactiveVal(data.frame(
    Name = c("Game", "Time"),
    Levels = c(1, 10),  # Time will be updated by num_trials
    Pattern = c("Loop", "Loop"),
    stringsAsFactors = FALSE
  ))
  
  arm_data <- reactiveVal(data.frame(
    Name = c("Position"),
    Levels = c(5),  # Position will be updated by num_arms
    Pattern = c("Loop"),
    stringsAsFactors = FALSE
  ))
  
  link_data <- reactiveVal(data.frame(
    State_Feature = character(),
    Link_Function = character(),
    Arm_Feature = character(),
    stringsAsFactors = FALSE
  ))
  
  # Initialize choices for link dropdowns
  observe({
    updateSelectInput(session, "link_state", 
                     choices = state_data()$Name,
                     selected = state_data()$Name[1])
    updateSelectInput(session, "link_arm", 
                     choices = arm_data()$Name,
                     selected = arm_data()$Name[1])
  }, priority = 1000)  # High priority to ensure it runs early
  
  # Initialize the default values to match input values
  observe({
    # Update initial values when the app starts
    isolate({
      # Update Time levels
      current_state_data <- state_data()
      time_row <- which(current_state_data$Name == "Time")
      if (length(time_row) > 0) {
        current_state_data$Levels[time_row] <- input$num_trials
        state_data(current_state_data)
      }
      
      # Update Position levels
      current_arm_data <- arm_data()
      position_row <- which(current_arm_data$Name == "Position")
      if (length(position_row) > 0) {
        current_arm_data$Levels[position_row] <- input$num_arms
        arm_data(current_arm_data)
      }
    })
  }, priority = 1000)  # High priority to ensure it runs at startup
  
  # Add State Variable with duplicate check
  observeEvent(input$add_state, {
    new_state <- data.frame(
      Name = input$state_name,
      Levels = input$state_levels,
      Pattern = input$state_pattern,
      stringsAsFactors = FALSE
    )
    
    # Check for duplicate
    current_data <- state_data()
    duplicate_idx <- which(current_data$Name == input$state_name)
    
    if (length(duplicate_idx) > 0) {
      # Replace existing row
      current_data[duplicate_idx, ] <- new_state
      state_data(current_data)
    } else {
      # Add new row
      state_data(rbind(current_data, new_state))
    }
    updateSelectInput(session, "link_state", choices = state_data()$Name)
  })
  
  # Add Arm Variable with duplicate check
  observeEvent(input$add_arm, {
    new_arm <- data.frame(
      Name = input$arm_name,
      Levels = input$arm_levels,
      Pattern = input$arm_pattern,
      stringsAsFactors = FALSE
    )
    
    # Check for duplicate
    current_data <- arm_data()
    duplicate_idx <- which(current_data$Name == input$arm_name)
    
    if (length(duplicate_idx) > 0) {
      # Replace existing row
      current_data[duplicate_idx, ] <- new_arm
      arm_data(current_data)
    } else {
      # Add new row
      arm_data(rbind(current_data, new_arm))
    }
    updateSelectInput(session, "link_arm", choices = arm_data()$Name)
  })
  
  # Add Link with improved duplicate check
  observeEvent(input$add_link, {
    new_link <- data.frame(
      State_Feature = input$link_state,
      Link_Function = input$link_function,
      Arm_Feature = input$link_arm,
      stringsAsFactors = FALSE
    )
    
    # Check for duplicate
    current_data <- link_data()
    duplicate_idx <- which(current_data$State_Feature == input$link_state & 
                          current_data$Arm_Feature == input$link_arm)
    
    if (length(duplicate_idx) > 0) {
      # Replace existing row
      current_data[duplicate_idx, ] <- new_link
      link_data(current_data)
    } else {
      # Add new row
      if (nrow(current_data) == 0) {
        link_data(new_link)
      } else {
        link_data(rbind(current_data, new_link))
      }
    }
  })
  
  # Link table rendering with improved remove button
  output$link_table <- renderDT({
    df <- link_data()
    if (nrow(df) == 0) {
      # 创建一个空数据框，但包含所有必要的列
      df <- data.frame(
        State_Feature = character(),
        Link_Function = character(),
        Arm_Feature = character(),
        Operation = character(),
        stringsAsFactors = FALSE
      )
    } else {
      # 为现有数据添加 Operation 列
      df$Operation <- sapply(1:nrow(df), function(i) {
        sprintf('<button onclick="Shiny.setInputValue(\'remove_link_key\', \'%s|%s\')" class="btn btn-danger btn-sm">Remove</button>', 
                df$State_Feature[i], df$Arm_Feature[i])
      })
    }
    
    datatable(
      df,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        columnDefs = list(
          list(
            targets = ncol(df) - 1,  # Operation column
            className = 'dt-center'
          )
        )
      )
    )
  })
  
  # Remove link by composite key (State_Feature|Arm_Feature)
  observeEvent(input$remove_link_key, {
    if (!is.null(input$remove_link_key)) {
      # Split the composite key
      key_parts <- strsplit(input$remove_link_key, "\\|")[[1]]
      state_feature <- key_parts[1]
      arm_feature <- key_parts[2]
      
      # Remove the link
      current_links <- link_data()
      links_to_keep <- !(current_links$State_Feature == state_feature & 
                        current_links$Arm_Feature == arm_feature)
      
      # Update with remaining links
      link_data(current_links[links_to_keep, , drop = FALSE])
    }
  })
  
  # Remove State by Name with link cleanup
  observeEvent(input$remove_state_name, {
    name_to_remove <- input$remove_state_name
    if (!is.null(name_to_remove) && !(name_to_remove %in% c("Game", "Time"))) {  # Protect default rows
      # First remove any links that reference this state
      current_links <- link_data()
      links_to_keep <- current_links$State_Feature != name_to_remove
      link_data(current_links[links_to_keep, , drop = FALSE])
      
      # Then remove the state
      current_data <- state_data()
      current_data <- current_data[current_data$Name != name_to_remove, ]
      state_data(current_data)
      
      # Update the state feature dropdown
      updateSelectInput(session, "link_state", choices = state_data()$Name)
    }
  })
  
  # Remove Arm by Name with link cleanup
  observeEvent(input$remove_arm_name, {
    name_to_remove <- input$remove_arm_name
    if (!is.null(name_to_remove) && name_to_remove != "Position") {  # Protect default row
      # First remove any links that reference this arm
      current_links <- link_data()
      links_to_keep <- current_links$Arm_Feature != name_to_remove
      link_data(current_links[links_to_keep, , drop = FALSE])
      
      # Then remove the arm
      current_data <- arm_data()
      current_data <- current_data[current_data$Name != name_to_remove, ]
      arm_data(current_data)
      
      # Update the arm feature dropdown
      updateSelectInput(session, "link_arm", choices = arm_data()$Name)
    }
  })
  
  # Render Tables with correct remove button functionality
  output$state_table <- renderDT({
    df <- state_data()
    # Add operation column (empty for default rows)
    df$Operation <- sapply(1:nrow(df), function(i) {
      if (i <= 2) return("") # Default rows (Game and Time)
      sprintf('<button onclick="Shiny.setInputValue(\'remove_state_name\', \'%s\')" class="btn btn-danger btn-sm">Remove</button>', 
              df$Name[i])
    })
    
    datatable(
      df,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        columnDefs = list(
          list(
            targets = ncol(df) - 1,  # Operation column
            className = 'dt-center'
          )
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().rows().nodes()).css({'background-color': '#ffffff'});",
          "  $(this.api().rows([0,1]).nodes()).css({'background-color': '#f5f5f5'});",
          "}")
      )
    )
  })
  
  output$arm_table <- renderDT({
    df <- arm_data()
    # Add operation column (empty for default row)
    df$Operation <- sapply(1:nrow(df), function(i) {
      if (i <= 1) return("") # Default row (Position)
      sprintf('<button onclick="Shiny.setInputValue(\'remove_arm_name\', \'%s\')" class="btn btn-danger btn-sm">Remove</button>', 
              df$Name[i])
    })
    
    datatable(
      df,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        columnDefs = list(
          list(
            targets = ncol(df) - 1,  # Operation column
            className = 'dt-center'
          )
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().rows().nodes()).css({'background-color': '#ffffff'});",
          "  $(this.api().rows([0]).nodes()).css({'background-color': '#f5f5f5'});",
          "}")
      )
    )
  })
  
  # Reward Matrix Heatmap with seed control
  reward_matrix <- reactive({
    set.seed(input$seed)  # Set random seed
    matrix(runif(input$num_trials * input$num_arms, 0, 100), 
           nrow = input$num_trials, 
           ncol = input$num_arms)
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
          State_Variables = state_data(),
          Arm_Variables = arm_data(),
          Link_Matrix = link_data()
        ),
        file
      )
    }
  )
}

# Run App
shinyApp(ui, server)