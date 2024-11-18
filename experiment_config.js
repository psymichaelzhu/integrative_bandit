const experiment_setup={
      "NUM_TRIALS": 5,
      "NUM_OPTIONS": 4,
      "NAME": "Integrative Bandit",
      "SOCIAL_VERSION": "social",
      "REWARD_PROBABILITIES": [0.5, 0.5, 0.5, 0.5],
      "DISTRIBUTION": "bernoulli",
      "REWARD_MATRIX": [
        [0, 0, 0, 0],
        [1, 0, 0, 0],
        [0, 0, 0, 0],
        [1, 0, 0, 0],
        [0, 0, 0, 0]
    ],
    // Add this to your experiment configuration
    "FULL_FEEDBACK": false, // or false for contingent feedback
    "IMAGE_INDICES": [
      [1,1], 
      [1,4], 
      [2,1], 
      [2,3]
  ],
  "NUM_CONTEXTS": 2,
  "CONTEXT_INDICES": [1, 0, 1, 0, 1]
}
