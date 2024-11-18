experiment_config <- list(
  NUM_TRIALS = 5,
  NUM_OPTIONS = 2,
  NAME = "Integrative Bandit",
  SOCIAL_VERSION = "social",
  REWARD_PROBABILITIES = c(0.5, 0.5),
  DISTRIBUTION = "bernoulli",
  DYNAMICS = "random walk",
  GENERALIZATION = "structural",
  FULL_FEEDBACK = TRUE
)

reward_matrix <- matrix(
  c(0, 0,
    1, 0,
    0, 0,
    1, 0,
    0, 0),
  nrow = experiment_config$NUM_TRIALS,  # NUM_TRIALS
  ncol = experiment_config$NUM_OPTIONS,  # NUM_OPTIONS
  byrow = TRUE
)
reward_matrix_json <- jsonlite::toJSON(reward_matrix, matrix = "columnmajor")

experiment_config$REWARD_MATRIX <- reward_matrix


# 将配置转换为JSON并保存
json_config <- jsonlite::toJSON(experiment_config, auto_unbox = TRUE, pretty = TRUE)
writeLines(json_config, "experiment_config2.js")