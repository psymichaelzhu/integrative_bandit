const experiment_setup={
      "NUM_TRIALS": 4,
      "NUM_OPTIONS": 2,
      "NAME": "Celestara Universe",
      "SOCIAL_VERSION": "social",
      "REWARD_PROBABILITIES": [0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0.5,0.5],
      "DISTRIBUTION": "bernoulli",
      "REWARD_MATRIX": [
        [1, 0, 0, 0, 0, 0, 0, 0],
        [1, 1, 0, 0, 2, 0, 0, 0],
        [0, 0, 1, 0, 0, 0, 0, 0],
        [1, 0, 0, 1, 2, 0, 0, 0],
        [0, 0, 0, 0, 1, 0, 0, 0],
        [1, 0, 0, 0, 2, 1, 0, 0],
        [0, 0, 0, 0, 0, 0, 1, 0],
        [1, 0, 0, 0, 2, 0, 0, 1],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [1, 1, 1, 1, 1, 1, 1, 1]
    ],
    "FULL_FEEDBACK": true,
    "IMAGE_INDICES": [
      [1,1], 
      [1,4], 
      [2,1], 
      [2,3],
      [3,5],
      [3,2],
      [2,2],
      [3,3]
  ],
  "NUM_CONTEXTS": 3,
  "CONTEXT_INDICES": [0, 1, 2, 0, 1, 2, 0, 1, 2, 0],
  "NUM_FORCED_TRIALS": 3,
  "FORCED_CHOICE_SEQUENCE": [0, 1, 0]
}
