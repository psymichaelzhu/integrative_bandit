const NAME = "Celestara Universe";
//const NUM_TRIALS = 100;
//const NUM_OPTIONS = 4;
const REWARD_TYPE = "numeric";
const SOCIAL_VERSION = "social";
const FEEDBACK_VERSION = "full";
const IMAGE_INDICES = [
    [1,1],
    [1,2], 
    [1,3],
    [1,4]
];
const NUM_FORCED_TRIALS = 0;
const FORCED_CHOICE_SEQUENCE = [0, 1, 0];


# Keep existing update_demo button for manual updates
  observeEvent(input$update_demo, {
    # Set seed before generating new matrix
    set.seed(parameters$seed)
    new_matrix <- summary_reward_distribution(link_data(), state_data(), arm_data(), parameters$num_trials, parameters$num_arms)
    reward_matrix(new_matrix)
  })


  seed


  重复


  绘图

  noise


  bug




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