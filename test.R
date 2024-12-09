








  
  
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
    max_allowed <- parameters$num_trials
    
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