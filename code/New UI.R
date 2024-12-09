library(shiny)
library(DT)
library(ggplot2)
library(reshape2)
library(bruceR)
library(shinyjs)
library(rclipboard)
# UI ==========================================================================

ui <- fluidPage(
  useShinyjs(),
  rclipboardSetup(),
  tags$head(
    tags$style(HTML("
      .text-muted {
        color: #6c757d !important;
        opacity: 0.8;
      }
    "))
  ),
  titlePanel("Integrative Bandit Parameterization Interface"),
  
  # Basic Configuration
  h4("Basic Configuration"),
  
  # Initialization of NUM_TRIALS, NUM_ARMS, and SEED
  fluidRow(
    column(4, numericInput("num_trials", "Number of Trials", value = 10, min = 1)),
    column(4, numericInput("num_arms", "Number of Arms", value = 5, min = 1)),
    column(4, selectInput("feedback_version", "Feedback Version",
                         choices = c("full", "contingent"),
                         selected = "contingent"))
  ),

  # Additional Configuration Options
  fluidRow(
    column(4, selectInput("reward_type", "Reward Type", 
                         choices = c("numeric", "binary"),
                         selected = "numeric")),
    column(4, selectInput("cover_story", "Cover Story",
                         choices = c("social", "non-social"), 
                         selected = "non-social")),
    column(4, numericInput("seed", "Seed (Only for UI Illustration)", value = 42, min = 1))
  ),


  # Trial and Arm Variables Configuration (side by side)
  fluidRow(
    h4("Feature Configuration"),
    # Trial Variables Configuration
    column(6,
           h5("Trial-based Variables Configuration"),
           fluidRow(
             column(3, 
                    textInput("state_name", "Name")),
             column(3, 
                    numericInput("state_levels", "Levels", 
                               value = 1, min = 1)),
             column(3, 
                    selectInput("state_pattern", "Pattern", 
                              choices = c("Shuffle", "Loop", "Random"))),
             column(3, 
                    div(style = "margin-top: 25px;", 
                        actionButton("add_state", "Add", 
                                   class = "btn-info btn-sm",
                                   style = "width: 60px;")))
           ),
           DTOutput("state_table")
    ),
    
    # Arm Variables Configuration
    column(6,
           h5("Arm-based Variables Configuration"),
           fluidRow(
             column(3, 
                    textInput("arm_name", "Name")),
             column(3, 
                    numericInput("arm_levels", "Levels", 
                               value = 1, min = 1)),
             column(3, 
                    selectInput("arm_pattern", "Pattern", 
                              choices = c("Shuffle", "Loop", "Random"))),
             column(3, 
                    div(style = "margin-top: 25px;", 
                        actionButton("add_arm", "Add", 
                                   class = "btn-info btn-sm",
                                   style = "width: 60px;")))
           ),
           DTOutput("arm_table")
    )
  ),
  
  # Reward Distribution Configuration
  fluidRow(
    column(12,
           h4("Reward Configuration"),
           fluidRow(
             column(2, 
                    selectInput("reward_function", "Function", 
                              choices = c("Identical", "Independent", "Monotonic", "Random Walk"),
                              selected = "Identical"),
                    style = "padding-right: 5px;"),
             column(2, 
                    selectInput("type1", "Type", 
                              choices = c("Trial", "Arm"),
                              selected = "Trial"),
                    style = "padding-right: 5px;"),
             column(2, 
                    selectInput("name1", "Name", 
                              choices = NULL),
                    style = "padding-right: 5px;"),
             column(1,
                    div(style = "margin-top: 5px; width: 100%; text-align: left;",
                        h6(style = "margin: 0;", "On "),
                        checkboxInput("conditional", "", value = FALSE)),
                    style = "width: 8%;"),
             column(2, 
                    textInput("type2", "Type", 
                             value = "Arm") %>% 
                      tagAppendChild(
                        tags$script("$('#type2').prop('readonly', true);")
                      ),
                    style = "padding-left: 5px;"),
             column(2, 
                    selectInput("name2", "Name", 
                              choices = NULL),
                    style = "padding-left: 5px;"),
             column(1, 
                    div(style = "margin-top: 25px;", 
                        actionButton("add_reward", "Add", 
                                   class = "btn-info btn-sm",
                                   style = "width: 50px;")),
                    style = "width: 8%;")
           ),
           div(style = "padding: 0 15px;",
               DTOutput("reward_table"))
    )
  ),
  

  # Asymmetric Section
  fluidRow(
    column(12,
           h4("Asymmetry Configuration")
    )
  ),
  fluidRow(
    # Information Column
    column(4,
           h5("Information"),
           selectInput("forced_pattern", "Pattern",
                      choices = c("Equal", "Unequal"),
                      selected = "Equal"),
           numericInput("num_forced_choice", "# Forced Choice",
                       value = 0, min = 0, max = 100)
    ),
    
    # Noise Column
    column(4,
           h5("Noise"),
           selectInput("noise_pattern", "Pattern",
                      choices = c("Equal", "Unequal"),
                      selected = "Equal"),
           selectInput("noise_level", "Noise Level",
                      choices = c("None", "Low", "Median", "High"),
                      selected = "None"),
           selectInput("noise_informed", "Explicitly Informed",
                      choices = c("Yes", "No"),
                      selected = "No")
    ),
    
    # Cost Column
    column(4,
           h5("Cost"),
           selectInput("cost_pattern", "Pattern",
                      choices = c("Equal", "Unequal"),
                      selected = "Equal"),
           selectInput("cost_level", "Cost Level",
                      choices = c("None", "Low", "Median", "High"),
                      selected = "None"),
           selectInput("cost_informed", "Explicitly Informed",
                      choices = c("Yes", "No"),
                      selected = "Yes")
    )
  ),

  # Bottom buttons
  fluidRow(
    column(12,
           div(style = "display: flex; justify-content: center; gap: 10px;",
               downloadButton("save_config", "Save Configuration", class = "btn-success"),
               uiOutput("clip")
           )
    )
  )
)

# Helper Functions =============================================================
# Create a new function to generate configuration lines
generate_config_lines <- function(basic_parameters, input, state_data, arm_data, link_data) {
  lines <- c()
  
  # Add basic_parameters section
  lines <- c(lines,
            "// Basic Parameters",
            paste0("const NUM_TRIALS = ", basic_parameters$num_trials, ";"),
            paste0("const NUM_ARMS = ", basic_parameters$num_arms, ";"),
            paste0("const REWARD_TYPE = '", input$reward_type, "';"),
            paste0("const FEEDBACK_VERSION = '", input$feedback_version, "';"),
            paste0("const COVER_STORY = '", input$cover_story, "';"),
            "")
  
  # Add Asymmetric Configuration section
  lines <- c(lines,
            "// Asymmetry Configuration", 
            "const ASYMMETRY_CONFIG = {",
            "  information: {",
            paste0("    pattern: '", input$forced_pattern, "',"),
            paste0("    numForcedChoice: ", input$num_forced_choice),
            "  },",
            "  noise: {",
            paste0("    pattern: '", input$noise_pattern, "',"),
            paste0("    level: '", input$noise_level, "',"),
            "  },",
            "  cost: {",
            paste0("    pattern: '", input$cost_pattern, "',"),
            paste0("    level: '", input$cost_level, "',"),
            "  }",
            "};",
            "")
  
  # Add trial variables section
  state_df <- state_data()
  lines <- c(lines,
            "// Trial Variables",
            "const STATE_VARIABLES = {")
  
  state_lines <- apply(state_df, 1, function(row) {
    paste0("  ", row["Name"], ": {",
          "levels: ", row["Levels"], ", ",
          "pattern: '", row["Pattern"], "'",
          "}")
  })
  lines <- c(lines, paste0(paste(state_lines, collapse = ",\n"), "\n};"), "")
  
  # Add arm variables section
  arm_df <- arm_data()
  lines <- c(lines,
            "// Arm Variables",
            "const ARM_VARIABLES = {")
  
  arm_lines <- apply(arm_df, 1, function(row) {
    paste0("  ", row["Name"], ": {",
          "levels: ", row["Levels"], ", ",
          "pattern: '", row["Pattern"], "'",
          "}")
  })
  lines <- c(lines, paste0(paste(arm_lines, collapse = ",\n"), "\n};"), "")
  
  # Add link matrix section
  link_df <- link_data()
  if (nrow(link_df) > 0) {
    lines <- c(lines,
              "// Distribution Links",
              "const DISTRIBUTION_LINKS = [")
    
    link_lines <- apply(link_df, 1, function(row) {
      paste0("  {",
            "stateVariable: '", row["State_Variable"], "', ",
            "stateDistribution: '", row["State_Distribution"], "', ",
            "interaction: '", row["Interaction"], "', ",
            "armDistribution: '", row["Arm_Distribution"], "', ",
            "armVariable: '", row["Arm_Variable"], "'",
            "}")
    })
    lines <- c(lines, paste0(paste(link_lines, collapse = ",\n"), "\n];"))
  } else {
    lines <- c(lines,
              "// Distribution Links",
              "const DISTRIBUTION_LINKS = [];")
  }
  
  return(lines)
}
# Server =====================================================================

server <- function(input, output, session) {
  # 0 Reactive Variables
  # Define basic_parameters reactiveValues first
  basic_parameters <- reactiveValues(
    num_trials = 10,
    num_arms = 5,
    seed = 42
  )
  state_data <- reactiveVal(data.frame(
    Name = c("Index", "Planet"),
    Levels = c(10, 1),  # Index will be updated by num_trials
    Pattern = c("Loop", "Shuffle"),
    stringsAsFactors = FALSE
  ))
  arm_data <- reactiveVal(data.frame(
    Name = c("Index", "Color", "Shape"),
    Levels = c(5, 1, 1),  # Index will be updated by num_arms
    Pattern = c("Loop", "Shuffle", "Shuffle"),
    stringsAsFactors = FALSE
  ))
  reward_data <- reactiveVal(data.frame(
    Function = character(),
    Type1 = character(),
    Name1 = character(),
    Conditional = logical(),
    Type2 = character(),
    Name2 = character(),
    stringsAsFactors = FALSE
  ))



  # 1 Basic Section
  # Valid update
  observeEvent(input$num_trials, {
    new_value <- input$num_trials
    if (is.null(new_value) || is.na(new_value) || new_value < 1) {
      updateNumericInput(session, "num_trials", value = basic_parameters$num_trials)
    } else {
      basic_parameters$num_trials <- new_value
    }
  })

  observeEvent(input$num_arms, {
    new_value <- input$num_arms
    if (is.null(new_value) || is.na(new_value) || new_value < 1) {
      updateNumericInput(session, "num_arms", value = basic_parameters$num_arms)
    } else {
      basic_parameters$num_arms <- new_value
    }
  })

  observeEvent(input$seed, {
    new_value <- input$seed
    if (is.null(new_value) || is.na(new_value) || new_value < 1) {
      updateNumericInput(session, "seed", value = basic_parameters$seed)
    } else {
      basic_parameters$seed <- new_value
    }
  })

  # Chain Reaction
  observeEvent(basic_parameters$num_arms, {
    # If basic_parameters$num_arms change, valid num_arms
    # Update arm_data (Arm Levels) when num_arms changes
    current_data <- arm_data()
    position_row <- which(current_data$Name == "Index")
    if (length(position_row) > 0) {
      current_data$Levels[position_row] <- basic_parameters$num_arms
      arm_data(current_data)
    }
  })

  observeEvent(basic_parameters$num_trials, {
    # Update state_data (Trial Levels) when num_trials changes
    current_data <- state_data()
    time_row <- which(current_data$Name == "Index")
    if (length(time_row) > 0) {
      current_data$Levels[time_row] <- basic_parameters$num_trials
      state_data(current_data)
    }
    # check forced choice validity
    max_allowed <- basic_parameters$num_trials
    # Update num_forced_choice limit
    updateNumericInput(session, "num_forced_choice",
                      max = max_allowed,
                      value = isolate({
                        # Only update if current value exceeds new maximum
                        current_value <- input$num_forced_choice
                        if (is.null(current_value) || current_value > max_allowed) {
                          max_allowed
                        } else {
                          current_value
                        }
                      }))
  })

  # Asymmetry Section
  # UI display
  #Unequal in Noise and Cost, UI
  observe({
    if (input$noise_pattern == "Unequal") {
      shinyjs::addCssClass("noise_level", "text-muted")
      shinyjs::disable("noise_level")
      updateSelectInput(session, "noise_level", selected = "None")
    } else {
      shinyjs::removeCssClass("noise_level", "text-muted")
      shinyjs::enable("noise_level")
    }
  })

  observe({
    if (input$cost_pattern == "Unequal") {
      shinyjs::addCssClass("cost_level", "text-muted")
      shinyjs::disable("cost_level")
      updateSelectInput(session, "cost_level", selected = "None")
    } else {
      shinyjs::removeCssClass("cost_level", "text-muted")
      shinyjs::enable("cost_level")
    }
  })

  # Forced Choice field, safety check
  observeEvent(input$num_forced_choice, {
    # Validate input
    new_value <- input$num_forced_choice
    if (is.null(new_value) || is.na(new_value) || new_value < 0 ) { #valid value
      updateNumericInput(session, "num_forced_choice", value = 0)
    } else if (new_value > basic_parameters$num_trials) {
      updateNumericInput(session, "num_forced_choice", value = basic_parameters$num_trials)
    } else {
      basic_parameters$num_forced_choice <- new_value
    }
  }, priority = 1000)

  # Feature Section

  # Table display
  output$state_table <- renderDT({
    df <- state_data()
    df$Operation <- sapply(1:nrow(df), function(i) {
      if (df$Name[i] == "Index") {
        # Index row is uneditable and not deletable
        return("")
      } else if (df$Name[i] == "Planet") {
        # Planet row is editable but not deletable
        return("")
      } else {
        # Other rows are deletable
        return(sprintf('<button onclick="Shiny.setInputValue(\'remove_state_name\', \'%s\')" class="btn btn-danger btn-sm">Remove</button>', 
                      df$Name[i]))
      }
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
          ),
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "  if (row[0] === 'Index') {",
              "    return '<span class=\"text-muted\">' + data + '</span>';",
              "  }",
              "  return data;",
              "}"
            )
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
  output$arm_table <- renderDT({
    df <- arm_data()
    df$Operation <- sapply(1:nrow(df), function(i) {
      if (df$Name[i] == "Index") {
        # Index row is uneditable and not deletable
        return("")
      } else if (df$Name[i] %in% c("Color", "Shape")) {
        # Color and Shape rows are editable but not deletable
        return("")
      } else {
        # Other rows are deletable
        return(sprintf('<button onclick="Shiny.setInputValue(\'remove_arm_name\', \'%s\')" class="btn btn-danger btn-sm">Remove</button>', 
                      df$Name[i]))
      }
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
          ),
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "  if (row[0] === 'Index') {",
              "    return '<span class=\"text-muted\">' + data + '</span>';",
              "  }",
              "  return data;",
              "}"
            )
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

  # Add/Update Feature
  observeEvent(input$add_state, {
    if (input$state_name == "Index") { #Index is not editable
      return()
    }
    
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
  observeEvent(input$add_arm, {
    if (input$arm_name == "Index") { #Index is not editable
      return()
    }
    
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

  # Delete Feature
  observeEvent(input$remove_state_name, {
    name_to_remove <- input$remove_state_name
    if (!is.null(name_to_remove) && !name_to_remove %in% c("Index", "Planet")) {  # Index and Planet are not deletable
        # Remove any rewards associated with this trial
        current_rewards <- reward_data()
        rewards_to_keep <- !(
            (current_rewards$Type1 == "Trial" & current_rewards$Name1 == name_to_remove) |
            (current_rewards$Type2 == "Trial" & current_rewards$Name2 == name_to_remove)
        )
        reward_data(current_rewards[rewards_to_keep, , drop = FALSE])
        
        # Then remove the trial
        current_data <- state_data()
        current_data <- current_data[current_data$Name != name_to_remove, ]
        state_data(current_data)
        
        # Update the trial feature dropdown
        updateSelectInput(session, "link_state", choices = state_data()$Name)
    }
  })
  observeEvent(input$remove_arm_name, {
    name_to_remove <- input$remove_arm_name
    if (!is.null(name_to_remove) && !name_to_remove %in% c("Index", "Color", "Shape")) {  # Index, Color, and Shape are not deletable
        # Remove any rewards associated with this arm
        current_rewards <- reward_data()
        rewards_to_keep <- !(
            (current_rewards$Type1 == "Arm" & current_rewards$Name1 == name_to_remove) |
            (current_rewards$Type2 == "Arm" & current_rewards$Name2 == name_to_remove)
        )
        reward_data(current_rewards[rewards_to_keep, , drop = FALSE])
        
        # Then remove the arm
        current_data <- arm_data()
        current_data <- current_data[current_data$Name != name_to_remove, ]
        arm_data(current_data)
        
        # Update the arm feature dropdown
        updateSelectInput(session, "link_arm", choices = arm_data()$Name)
    }
  })



  # Reward Section
  # Reward UI

  observeEvent(input$type1, {
    # Update name1 choices based on type1
    if (input$type1 == "Trial") {
      updateSelectInput(session, "name1", choices = state_data()$Name)
    } else {
      updateSelectInput(session, "name1", choices = arm_data()$Name)
    }
  })
  observe({
    if (!input$conditional) {
      # Disable and clear Type2 and Name2 when conditional is unchecked
      shinyjs::addCssClass("type2", "text-muted")
      shinyjs::addCssClass("name2", "text-muted")
      shinyjs::disable("type2")
      shinyjs::disable("name2")
      # Set empty value for Type2 and clear Name2 choices
      updateTextInput(session, "type2", value = "")
      updateSelectInput(session, "name2", choices = "")
    } else {
      # Enable and update when conditional is checked
      shinyjs::removeCssClass("type2", "text-muted")
      shinyjs::removeCssClass("name2", "text-muted")
      shinyjs::enable("type2")
      shinyjs::enable("name2")
      
      # Set opposite type for type2 based on type1
      if (input$type1 == "Trial") {
        updateTextInput(session, "type2", value = "Arm")
        updateSelectInput(session, "name2", choices = arm_data()$Name)
      } else {
        updateTextInput(session, "type2", value = "Trial")
        updateSelectInput(session, "name2", choices = state_data()$Name)
      }
    }
  })

  # Update name1 choices when state_data or arm_data changes
  observe({
    if (input$type1 == "Trial") {
      updateSelectInput(session, "name1", choices = state_data()$Name)
    }
  })
  observe({
    if (input$type1 == "Arm") {
      updateSelectInput(session, "name1", choices = arm_data()$Name)
    }
  })
  
  # Add Reward
  observeEvent(input$add_reward, {
    new_reward <- data.frame(
      Function = input$reward_function,
      Type1 = input$type1,
      Name1 = input$name1,
      Conditional = input$conditional,
      Type2 = ifelse(input$conditional, input$type2, ""),
      Name2 = ifelse(input$conditional, input$name2, ""),
      Operation = "",  # Placeholder for the Operation column
      stringsAsFactors = FALSE
    )
    
    # Check for duplicate
    current_data <- reward_data()
    duplicate_idx <- which(current_data$Type1 == new_reward$Type1 & 
                           current_data$Name1 == new_reward$Name1 & 
                           current_data$Type2 == new_reward$Type2 & 
                           current_data$Name2 == new_reward$Name2)
    
    if (length(duplicate_idx) > 0) {
      # Replace existing row
      current_data[duplicate_idx, ] <- new_reward
    } else {
      # Add new row
      current_data <- rbind(current_data, new_reward)
    }
    
    # Update Operation column with delete button
    current_data$Operation <- sapply(1:nrow(current_data), function(i) {
      sprintf('<button onclick="Shiny.setInputValue(\'remove_reward\', %d)" class="btn btn-danger btn-sm">Delete</button>', i)
    })
    
    reward_data(current_data)
  })

  # Delete Reward
  observeEvent(input$remove_reward, {
    index_to_remove <- input$remove_reward
    current_data <- reward_data()
    if (!is.null(index_to_remove) && index_to_remove <= nrow(current_data)) {
      current_data <- current_data[-index_to_remove, ]
      # Ensure Operation column is still present
      if (nrow(current_data) == 0) {
        current_data <- data.frame(
          Function = character(),
          Type1 = character(),
          Name1 = character(),
          Conditional = logical(),
          Type2 = character(),
          Name2 = character(),
          Operation = character(),  # Keep Operation column
          stringsAsFactors = FALSE
        )
      }
      reward_data(current_data)
    }
  })

  # Display Reward Table
  output$reward_table <- renderDT({
    datatable(
      reward_data(),
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
            targets = ncol(reward_data()) - 1,
            className = 'dt-center'
          )
        )
      )
    )
  })
  

  # Generate config text
  config_text <- reactiveVal("")

  # Save Configuration
  output$save_config <- downloadHandler(
    filename = function() {
      paste("Bandit_config.js", sep = "")
    },
    content = function(file) {
      lines <- config_text()
      writeLines(lines, file)
    }
  )

  # Copy to clipboard
  output$clip <- renderUI({
    rclipButton(
      inputId = "copy_config",
      label = "Copy to Clipboard",
      clipText = config_text(),
      icon = icon("clipboard"),
      class = "btn-info"
    )
  })
  observeEvent(input$copy_config, {
    showNotification("Configuration copied to clipboard!", type = "message")
  })


}

# Run App =====================================================================

shinyApp(ui, server)