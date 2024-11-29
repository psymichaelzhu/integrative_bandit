const experiment_setup={
  "NAME": "Celestara Universe",  
  "NUM_TRIALS": 100,
  "NUM_OPTIONS": 4,
  "REWARD_TYPE": "bernoulli",
  "REWARD_PARAMETERS": [0, 0.3, 0.7, 1, 0.5, 0.9, 0.8, 1],
  "SOCIAL_VERSION": "social",
  "FEEDBACK_VERSION": "full",
  "IMAGE_INDICES": [
      [1,1], 
      [1,2], 
      [1,3], 
      [1,4]
  ],
  "NUM_CONTEXTS": 1,
  "CONTEXT_INDICES": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],//随机循环 functioN
  "NUM_FORCED_TRIALS": 0,
  "FORCED_CHOICE_SEQUENCE": [0, 1, 0]
}
//选择，自动生成，可视化界面