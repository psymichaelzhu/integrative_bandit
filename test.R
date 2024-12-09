

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