library(shiny)
library(DT)
library(ggplot2)
library(reshape2)
library(bruceR)
library(shinyjs)

# UI
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
                              choices = c("Uniform", "Linear", "Asymmetric", "Correlation")),
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
                              choices = c("Uniform", "Linear", "Asymmetric", "Correlation")),
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

# 添加矩阵生成函数
generate_variable_matrix <- function(variable_type, name, levels, pattern, total_size, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # 创建全0矩阵
  if (variable_type == "state") {
    mat <- matrix(0, nrow = total_size, ncol = total_size)
  } else { # arm
    mat <- matrix(0, nrow = total_size, ncol = total_size)
  }
  
  # 确定有效的列数/行数（不超过总大小）
  valid_size <- min(levels, total_size)
  
  if (pattern == "Loop") {
    # 循环模式
    if (variable_type == "state") {
      for (i in 1:total_size) {
        col_idx <- ((i-1) %% valid_size) + 1
        mat[i, col_idx] <- 1
      }
    } else { # arm
      for (i in 1:total_size) {
        row_idx <- ((i-1) %% valid_size) + 1
        mat[row_idx, i] <- 1
      }
    }
  } else { # Random
    # 随机模式
    if (variable_type == "state") {
      for (i in 1:total_size) {
        col_idx <- sample(1:valid_size, 1)
        mat[i, col_idx] <- 1
      }
    } else { # arm
      for (i in 1:total_size) {
        row_idx <- sample(1:valid_size, 1)
        mat[row_idx, i] <- 1
      }
    }
  }
  
  # 打印日志
  cat(sprintf("\nGenerated matrix for %s variable '%s':\n", variable_type, name))
  print(mat)
  
  return(mat)
}

# Server
server <- function(input, output, session) {
  # 存储矩阵的 reactive values
  state_matrices <- reactiveVal(list())
  arm_matrices <- reactiveVal(list())
  
  # 初始化默认矩阵
  observe({
    # 仅在启动时运行一次
    isolate({
      # 为默认的 state variables 生成矩阵
      initial_state_matrices <- list()
      initial_state_data <- state_data()
      for (i in 1:nrow(initial_state_data)) {
        name <- initial_state_data$Name[i]
        matrix <- generate_variable_matrix(
          "state",
          name,
          initial_state_data$Levels[i],
          initial_state_data$Pattern[i],
          input$num_trials,
          input$seed
        )
        initial_state_matrices[[name]] <- matrix
      }
      state_matrices(initial_state_matrices)
      
      # 为默认的 arm variables 生成矩阵
      initial_arm_matrices <- list()
      initial_arm_data <- arm_data()
      for (i in 1:nrow(initial_arm_data)) {
        name <- initial_arm_data$Name[i]
        matrix <- generate_variable_matrix(
          "arm",
          name,
          initial_arm_data$Levels[i],
          initial_arm_data$Pattern[i],
          input$num_arms,
          input$seed
        )
        initial_arm_matrices[[name]] <- matrix
      }
      arm_matrices(initial_arm_matrices)
    })
  }, priority = 1000)
  
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
    position_row <- which(current_data$Name == "Index")
    if (length(position_row) > 0) {
      current_data$Levels[position_row] <- input$num_arms
      arm_data(current_data)
    }
  })

  # Initialize State and Arm Data with Default Rows
  state_data <- reactiveVal(data.frame(
    Name = c("Overall", "Time"),
    Levels = c(1, 10),  # Time will be updated by num_trials
    Pattern = c("Loop", "Loop"),
    stringsAsFactors = FALSE
  ))
  
  arm_data <- reactiveVal(data.frame(
    Name = c("Index"),
    Levels = c(5),  # Index will be updated by num_arms
    Pattern = c("Loop"),
    stringsAsFactors = FALSE
  ))
  
  # Initialize link data with interaction type
  link_data <- reactiveVal(data.frame(
    State_Variable = character(),
    State_Distribution = character(),
    Interaction = character(),      # Add interaction type
    Arm_Distribution = character(),
    Arm_Variable = character(),
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
      
      # Update Index levels
      current_arm_data <- arm_data()
      position_row <- which(current_arm_data$Name == "Index")
      if (length(position_row) > 0) {
        current_arm_data$Levels[position_row] <- input$num_arms
        arm_data(current_arm_data)
      }
    })
  }, priority = 1000)  # High priority to ensure it runs at startup
  
  # Add State Variable with duplicate check (update matrix if exists)
  observeEvent(input$add_state, {
    # 生成新矩阵
    new_matrix <- generate_variable_matrix(
      "state",
      input$state_name,
      input$state_levels,
      input$state_pattern,
      input$num_trials,
      input$seed
    )
    
    # 更新矩阵列表
    current_matrices <- state_matrices()
    current_matrices[[input$state_name]] <- new_matrix  # This will update if exists or add if new
    state_matrices(current_matrices)
    
    # 表格更新逻辑
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
  })
  
  # Add Arm Variable with duplicate check (update matrix if exists)
  observeEvent(input$add_arm, {
    # 生成新矩阵
    new_matrix <- generate_variable_matrix(
      "arm",
      input$arm_name,
      input$arm_levels,
      input$arm_pattern,
      input$num_arms,
      input$seed
    )
    
    # 更新矩阵列表
    current_matrices <- arm_matrices()
    current_matrices[[input$arm_name]] <- new_matrix  # This will update if exists or add if new
    arm_matrices(current_matrices)
    
    # 表格更新逻辑
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
  })
  
  # Add Link with interaction type
  observeEvent(input$add_link, {
    new_link <- data.frame(
      State_Variable = input$link_state,
      State_Distribution = input$link_state_function,
      Interaction = ifelse(input$link_distributions %% 2 == 1, "×", "+"),  # Set interaction based on current state
      Arm_Distribution = input$link_arm_function,
      Arm_Variable = input$link_arm,
      stringsAsFactors = FALSE
    )
    
    # Check for duplicate
    current_data <- link_data()
    duplicate_idx <- which(current_data$State_Variable == input$link_state & 
                          current_data$Arm_Variable == input$link_arm)
    
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
      df <- data.frame(
        State_Variable = character(),
        State_Distribution = character(),
        Interaction = character(),
        Arm_Distribution = character(),
        Arm_Variable = character(),
        Operation = character(),
        stringsAsFactors = FALSE
      )
    } else {
      df$Operation <- sapply(1:nrow(df), function(i) {
        sprintf('<button onclick="Shiny.setInputValue(\'remove_link_key\', \'%s|%s\')" class="btn btn-danger btn-sm">Remove</button>', 
                df$State_Variable[i], df$Arm_Variable[i])
      })
    }
    
    colnames(df) <- c("State Variable", "State Distribution", 
                      "Interaction", "Arm Distribution", 
                      "Arm Variable", "Operation")

    datatable(
      df,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        scrollX = FALSE,
        stripeClasses = FALSE,  # 禁用条纹样式
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-center'
          ),
          list(
            targets = c(0, 1, 3, 4),  # State and Arm columns
            width = "20%"
          ),
          list(
            targets = 2,    # Interaction column
            width = "8%"
          ),
          list(
            targets = 5,    # Operation column
            width = "12%"
          ),
          list(
            targets = "_all",
            headerClassName = 'dt-left'
          )
        )
      )
    ) %>% 
      formatStyle(
        columns = 1:6,
        backgroundColor = 'white'  # 设置所有单元格背景为白色
      )
  })
  
  # Remove link by composite key (State_Variable|Arm_Variable)
  observeEvent(input$remove_link_key, {
    if (!is.null(input$remove_link_key)) {
      # Split the composite key
      key_parts <- strsplit(input$remove_link_key, "\\|")[[1]]
      state_feature <- key_parts[1]
      arm_feature <- key_parts[2]
      
      # Remove the link
      current_links <- link_data()
      links_to_keep <- !(current_links$State_Variable == state_feature & 
                        current_links$Arm_Variable == arm_feature)
      
      # Update with remaining links
      link_data(current_links[links_to_keep, , drop = FALSE])
    }
  })
  
  # Remove State by Name with link cleanup and matrix removal
  observeEvent(input$remove_state_name, {
    name_to_remove <- input$remove_state_name
    if (!is.null(name_to_remove) && !(name_to_remove %in% c("Overall", "Time"))) {  # Protect default rows
      # First remove any links that reference this state
      current_links <- link_data()
      links_to_keep <- current_links$State_Variable != name_to_remove
      link_data(current_links[links_to_keep, , drop = FALSE])
      
      # Remove the matrix
      current_matrices <- state_matrices()
      current_matrices[[name_to_remove]] <- NULL  # Remove the matrix
      state_matrices(current_matrices)
      
      # Then remove the state
      current_data <- state_data()
      current_data <- current_data[current_data$Name != name_to_remove, ]
      state_data(current_data)
      
      # Update the state feature dropdown
      updateSelectInput(session, "link_state", choices = state_data()$Name)
    }
  })
  
  # Remove Arm by Name with link cleanup and matrix removal
  observeEvent(input$remove_arm_name, {
    name_to_remove <- input$remove_arm_name
    if (!is.null(name_to_remove) && name_to_remove != "Index") {  # Protect default row
      # First check if this arm is the only one referenced in any link
      current_links <- link_data()
      
      # Count how many unique arm variables are used in links
      unique_arms_in_links <- unique(current_links$Arm_Variable)
      
      # If this arm is in use and it's the only arm variable being used
      if (name_to_remove %in% unique_arms_in_links && length(unique_arms_in_links) == 1) {
        # Clear all links first
        link_data(data.frame(
          State_Variable = character(),
          State_Distribution = character(),
          Arm_Variable = character(),
          Arm_Distribution = character(),
          stringsAsFactors = FALSE
        ))
      } else {
        # Remove only links that reference this arm
        links_to_keep <- current_links$Arm_Variable != name_to_remove
        link_data(current_links[links_to_keep, , drop = FALSE])
      }
      
      # Remove the matrix
      current_matrices <- arm_matrices()
      current_matrices[[name_to_remove]] <- NULL  # Remove the matrix
      arm_matrices(current_matrices)
      
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
    df$Operation <- sapply(1:nrow(df), function(i) {
      if (i <= 2) return("") # Default rows (Overall and Time)
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
        stripeClasses = FALSE,
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-left'
          ),
          list(
            targets = ncol(df) - 1,
            className = 'dt-center'
          )
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().rows().nodes()).css({'background-color': '#ffffff'});",  # Set all rows to white first
          "  $(this.api().rows([0,1]).nodes()).css({'background-color': '#f5f5f5'});",  # Then set default rows to gray
          "}")
      )
    )
  })
  
  output$arm_table <- renderDT({
    df <- arm_data()
    df$Operation <- sapply(1:nrow(df), function(i) {
      if (i <= 1) return("") # Default row (Index)
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
        stripeClasses = FALSE,
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-left'
          ),
          list(
            targets = ncol(df) - 1,
            className = 'dt-center'
          )
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().rows().nodes()).css({'background-color': '#ffffff'});",  # Set all rows to white first
          "  $(this.api().rows([0]).nodes()).css({'background-color': '#f5f5f5'});",  # Then set default row to gray
          "}")
      )
    )
  })
  
  # Reward Matrix Heatmap with seed control
  reward_matrix <- reactive({
    # Instead of random values, create a matrix of zeros
    matrix(0, 
           nrow = input$num_trials, 
           ncol = input$num_arms)
  })
  
  output$reward_matrix <- renderPlot({
    mat <- reward_matrix()
    heatmap_data <- data.frame(
      Trial = rep(1:nrow(mat), ncol(mat)),
      Arm = rep(1:ncol(mat), each = nrow(mat)),
      Value = as.vector(mat)
    )
    
    ggplot(heatmap_data, aes(x = Trial, y = Arm, fill = Value)) +
      geom_tile() +
      scale_fill_viridis_c(limits = c(0, 100)) +  # Set fixed scale limits
      theme_minimal() +
      labs(x = "Trial", y = "Arm", fill = "Reward") +
      theme_bruce()
  })
  
  # Update Demo (renamed from Generate Demo)
  observeEvent(input$update_demo, {
    # Add your demo update logic here
    # This is a placeholder for the demo update functionality
  })
  
  # Save Configuration
  output$save_config <- downloadHandler(
    filename = function() {
      paste("bandit_configuration_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      # Prepare configuration data
      config <- list(
        state_variables = state_data(),
        arm_variables = arm_data(),
        link_matrix = link_data(),
        parameters = list(
          num_trials = input$num_trials,
          num_arms = input$num_arms,
          seed = input$seed
        )
      )
      
      # Save to file
      saveRDS(config, file)
    }
  )
  
    # Keep only this part
  observeEvent(input$link_distributions, {
    # Toggle button appearance only
    shinyjs::toggleClass("link_distributions", "btn-primary")
  })
}

# Run App
shinyApp(ui, server)