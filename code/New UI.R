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

  
  
  # State and Arm Variables Configuration (side by side)
  fluidRow(
    # State Variables Configuration
    column(6,
           h4("State Variables Configuration"),
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
                        actionButton("add_state", "Update", 
                                   class = "btn-info btn-sm",
                                   style = "width: 60px;")))
           ),
           DTOutput("state_table")
    ),
    
    # Arm Variables Configuration
    column(6,
           h4("Arm Variables Configuration"),
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
                        actionButton("add_arm", "Update", 
                                   class = "btn-info btn-sm",
                                   style = "width: 60px;")))
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
                    selectInput("link_state", "State Variable", 
                            choices = "Time",  # 初始只有Time选项
                            selected = "Time"),
                    style = "padding-right: 5px; width: 21%;"),
             column(3, 
                    selectInput("link_state_function", "State Distribution", 
                            choices = "Identical",  # 初始只有Identical选项
                            selected = "Identical"),
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
                              choices = c("Identical", "Independent", "Monotonic", "Random Walk")),
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
                      selected = "None")
    ),
    
    # Cost Column
    column(4,
           h5("Cost"),
           selectInput("cost_pattern", "Pattern",
                      choices = c("Equal", "Unequal"),
                      selected = "Equal"),
           selectInput("cost_level", "Cost Level",
                      choices = c("None", "Low", "Median", "High"),
                      selected = "None")
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
  
  # Add state variables section
  state_df <- state_data()
  lines <- c(lines,
            "// State Variables",
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
    Name = c("Time", "Planet"),
    Levels = c(10, 1),  # Time will be updated by num_trials
    Pattern = c("Loop", "Shuffle"),
    stringsAsFactors = FALSE
  ))
  arm_data <- reactiveVal(data.frame(
    Name = c("Index", "Color", "Shape"),
    Levels = c(5, 1, 1),  # Index will be updated by num_arms
    Pattern = c("Loop", "Shuffle", "Shuffle"),
    stringsAsFactors = FALSE
  ))
  link_data <- reactiveVal(data.frame(
    State_Variable = character(),
    State_Distribution = character(),
    Interaction = character(),
    Arm_Distribution = character(),
    Arm_Variable = character(),
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
    time_row <- which(current_data$Name == "Time")
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
      if (df$Name[i] == "Time") {
        # Time row is uneditable and not deletable
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
              "  if (row[0] === 'Time') {",
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
    if (input$state_name == "Time") { #Time is not editable
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
    if (!is.null(name_to_remove) && !name_to_remove %in% c("Time", "Planet")) {  # Time and Planet are not deletable
        # First remove any links that reference this state
        current_links <- link_data()
        links_to_keep <- current_links$State_Variable != name_to_remove
        link_data(current_links[links_to_keep, , drop = FALSE])
        
        # Then remove the state
        current_data <- state_data()
        current_data <- current_data[current_data$Name != name_to_remove, ]
        state_data(current_data)
        
        # Update the state feature dropdown
        updateSelectInput(session, "link_state", choices = state_data()$Name)
    }
  })
  observeEvent(input$remove_arm_name, {
    name_to_remove <- input$remove_arm_name
    if (!is.null(name_to_remove) && !name_to_remove %in% c("Index", "Color", "Shape")) {  # Index, Color, and Shape are not deletable
      current_links <- link_data()
      unique_arms_in_links <- unique(current_links$Arm_Variable)
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
      
      # Then remove the arm
      current_data <- arm_data()
      current_data <- current_data[current_data$Name != name_to_remove, ]
      arm_data(current_data)
      
      # Update the arm feature dropdown
      updateSelectInput(session, "link_arm", choices = arm_data()$Name)
    }
  })



  # Reward Section
  #Update field
  observe({
    if (is.null(input$link_distributions)) {
      updateActionButton(session, "link_distributions", value = 0)
    }
    current_state <- input$link_state
    current_arm <- input$link_arm
    state_choices <- state_data()$Name
    arm_choices <- arm_data()$Name
    if (input$link_distributions %% 2 == 1) {
      updateSelectInput(session, "link_state", 
                       choices = state_choices,
                       selected = if (current_state %in% state_choices) current_state else state_choices[1])
      updateSelectInput(session, "link_state_function",
                       choices = c("Independent", "Monotonic", "Random Walk"),
                       selected = "Independent")
    } else {
      updateSelectInput(session, "link_state",
                       choices = " ",
                       selected = " ")
      updateSelectInput(session, "link_state_function",
                       choices = " ",
                       selected = " ")
      
      shinyjs::addCssClass("link_state", "text-muted")
      shinyjs::addCssClass("link_state_function", "text-muted")
      shinyjs::disable("link_state")
      shinyjs::disable("link_state_function")
    }
   
    updateSelectInput(session, "link_arm", 
                     choices = arm_choices,
                     selected = if (current_arm %in% arm_choices) current_arm else arm_choices[1])
  }, priority = 1000)  
  
  
  # Table display
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
        # Include interaction type in the composite key
        sprintf('<button onclick="Shiny.setInputValue(\'remove_link_key\', \'%s|%s|%s\')" class="btn btn-danger btn-sm">Remove</button>', 
                df$State_Variable[i], df$Arm_Variable[i], df$Interaction[i])
      })
    }
    
    colnames(df) <- c("State Variable", "State Distribution", 
                      "Interaction", "Arm Distribution", 
                      "Arm Variable", "Operation")
    datatable(
      df,
      escape = FALSE,
      selection = 'single',  #  Allow single row selection
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        scrollX = FALSE,
        stripeClasses = FALSE,  #  Disable striped style
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
        backgroundColor = 'white'  #  Set all cell backgrounds to white
      )
  })
  # Add/Update Link
  observeEvent(input$add_link, {
    # Create new link with current interaction state
    current_interaction <- ifelse(input$link_distributions %% 2 == 1, "on", " ")
    
    new_link <- data.frame(
      State_Variable = input$link_state,
      State_Distribution = input$link_state_function,
      Interaction = current_interaction,
      Arm_Distribution = input$link_arm_function,
      Arm_Variable = input$link_arm,
      stringsAsFactors = FALSE
    )
    
    # Check for duplicate considering interaction type
    current_data <- link_data()
    duplicate_idx <- which(
      current_data$State_Variable == input$link_state & 
      current_data$Arm_Variable == input$link_arm &
      current_data$Interaction == current_interaction  
    )
    
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
  # Delete link
  observeEvent(input$remove_link_key, {
    if (!is.null(input$remove_link_key)) {
      # Split the composite key (now includes interaction)
      key_parts <- strsplit(input$remove_link_key, "\\|")[[1]]
      state_feature <- key_parts[1]
      arm_feature <- key_parts[2]
      interaction_type <- key_parts[3]
      
      # Remove the link considering all three components
      current_links <- link_data()
      links_to_keep <- !(current_links$State_Variable == state_feature & 
                        current_links$Arm_Variable == arm_feature &
                        current_links$Interaction == interaction_type)
      
      # Update with remaining links
      link_data(current_links[links_to_keep, , drop = FALSE])
    }
  })

  
  # UI display
  # link button
  observeEvent(input$link_distributions, {
    # Toggle button appearance
    shinyjs::toggleClass("link_distributions", "btn-primary")
    
    # Add disabled state to inputs when unlinked
    if (input$link_distributions %% 2 == 0) {
      shinyjs::addCssClass("link_state", "text-muted")
      shinyjs::addCssClass("link_state_function", "text-muted")
      shinyjs::disable("link_state")
      shinyjs::disable("link_state_function")
    } else {
      shinyjs::removeCssClass("link_state", "text-muted")
      shinyjs::removeCssClass("link_state_function", "text-muted")
      shinyjs::enable("link_state")
      shinyjs::enable("link_state_function")
    }
  })

  # Generate config text
  config_text <- reactiveVal("")
  observe({
    lines <- generate_config_lines(basic_parameters, input, state_data, arm_data, link_data)
    config_text(paste(lines, collapse = "\n"))
  })

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