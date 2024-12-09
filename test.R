

  observe({
    lines <- generate_config_lines(basic_parameters, input, state_data, arm_data, link_data)
    config_text(paste(lines, collapse = "\n"))
  })
  
  
  
  observe({
    if (!input$conditional) {
      # Disable and clear Type2 and Name2 when conditional is unchecked
      shinyjs::addCssClass("type2", "text-muted")
      shinyjs::addCssClass("name2", "text-muted")
      shinyjs::disable("type2")
      shinyjs::disable("name2")
    } else {
      # Enable and update when conditional is checked
      shinyjs::removeCssClass("type2", "text-muted")
      shinyjs::removeCssClass("name2", "text-muted")
      shinyjs::enable("type2")
      shinyjs::enable("name2")
    }
  })

  
  # Reward Distribution Configuration
  fluidRow(
    column(12,
           h4("Reward Distribution Configuration"),
           fluidRow(
             column(2, 
                    selectInput("function", "Function", 
                              choices = c("Identical", "Independent", "Monotonic", "Random Walk"),
                              selected = "Identical"),
                    style = "padding-right: 5px;"),
             column(2, 
                    selectInput("type1", "Type", 
                              choices = c("State", "Arm"),
                              selected = "State"),
                    style = "padding-right: 5px;"),
             column(2, 
                    selectInput("name1", "Name", 
                              choices = NULL),
                    style = "padding-right: 5px;"),
             column(1,
                    div(style = "margin-top: 25px; width: 100%; text-align: center;",
                        checkboxInput("conditional", "", value = FALSE)),
                    style = "width: 8%;"),
             column(2, 
                    selectInput("type2", "Type", 
                              choices = c("State", "Arm"),
                              selected = "Arm"),
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
               DTOutput("dist_table"))
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



数量问题

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

    # Update noise sequence
    new_noise <- generate_sequence(
      input$noise_pattern,
      input$noise_level,
      basic_parameters$num_arms
    )
    noise_sequence(new_noise)
    
    # Update cost sequence
    new_cost <- generate_sequence(
      input$cost_pattern,
      input$cost_level,
      basic_parameters$num_arms
    )
    cost_sequence(new_cost)
  })







  
  
  # Observe changes in Noise settings
  observe({
    # Generate new noise sequence
    new_noise <- generate_sequence(
      input$noise_pattern,
      input$noise_level,
      parameters$num_arms
    )
    
    # Update noise sequence
    noise_sequence(new_noise)
    
    # Print results
    cat("Noise sequence updated:\n")
    cat("Pattern:", input$noise_pattern, "\n")
    cat("Level:", input$noise_level, "\n")
    cat("Values:", paste(new_noise, collapse = ", "), "\n\n")
  })

  # Observe changes in Cost settings
  observe({
    # Generate new cost sequence
    new_cost <- generate_sequence(
      input$cost_pattern,
      input$cost_level,
      parameters$num_arms
    )
    
    # Update cost sequence
    cost_sequence(new_cost)
    
    # Print results
    cat("Cost sequence updated:\n")
    cat("Pattern:", input$cost_pattern, "\n")
    cat("Level:", input$cost_level, "\n")
    cat("Values:", paste(new_cost, collapse = ", "), "\n\n")
  })

  



  # Ensure forced_choice does not exceed NUM_TRIALS
  observe({
    # Get current num_trials value
    
  })




observeEvent(input$update_demo, {
    # Set seed before generating new matrix
    set.seed(basic_parameters$seed)
    new_matrix <- summary_reward_distribution(link_data(), state_data(), arm_data(), basic_parameters$num_trials, basic_parameters$num_arms)
    reward_matrix(new_matrix)
  })


  # Modify Update Demo button processing logic
  observeEvent(input$update_demo, {
    # Set random seed
    set.seed(parameters$seed)
    
    # Generate base reward matrix
    base_matrix <- summary_reward_distribution(
      link_data(), 
      state_data(), 
      arm_data(), 
      parameters$num_trials, 
      parameters$num_arms
    )
    
    # Get current noise and cost sequences
    current_noise <- noise_sequence()
    current_cost <- cost_sequence()
    
    # Generate final reward matrix
    final_matrix <- generate_final_reward_matrix(
      base_matrix,
      current_noise,
      current_cost,
      input$reward_type
    )
    
    # Update reward matrix
    reward_matrix(final_matrix)
    
    # Print debug information
    cat("Final Reward Matrix generated:\n")
    cat("Applied Noise:", paste(current_noise, collapse = ", "), "\n")
    cat("Applied Cost:", paste(current_cost, collapse = ", "), "\n")
  })