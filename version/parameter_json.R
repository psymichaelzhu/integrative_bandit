# My data.frame with all experimental variables
# Create experiment setup data frame
df <- data.frame(
  NUM_TRIALS = 5,
  NUM_OPTIONS = 2,
  NAME = "Integrative Bandit",
  SOCIAL_VERSION = "non_social",
  REWARD_PROBABILITIES = I(list(c(0.5, 0.5))),
  DISTRIBUTION = "bernoulli",
  REWARD_MATRIX = I(list(matrix(c(0,1, 1,2, 0,3, 1,4, 0,5), nrow=5, byrow=TRUE)))
)


create_json_variable_str <- function(var_name, list_var, extraString){
  require(rjson)
  json <- toJSON(list_var)
  Json_string <- gsub('\"', '\\\\"', json) # adding \ so that " are printed right
  return(paste('var ', var_name, extraString,' = "', Json_string, '";', sep = ''))
}

json_strings_from_df <- function(df, fileName, extraString = ""){
  # Functions takes data.frame and creates json_strings with it
  
  # Get all variable names
  var_nam <- names(df)
  
  # Create Javascript file to write to
  sink(fileName)
  
  # Loop to create json strings and assign to .GlobalEnv
  for(i in 1:length(var_nam)){
    assign(paste0(var_nam[i], '_string'),
           create_json_variable_str(var_nam[i], df[, var_nam[i]], extraString),
           envir = .GlobalEnv)
    
    # Write variable
    cat(get(paste0(var_nam[i], '_string')))
    
    # Line break
    cat('\n\n')
    
    # Write JSON.paste
    cat(paste0(var_nam[i], extraString, " = JSON.parse(",var_nam[i], extraString, ");"))
    
    # Line break
    cat('\n\n')
  }
  
  # Close file
  sink()
  
  # Report back
  cat(paste('Created', length(var_nam), '_string variables.\n'))
  cat(paste('Javascript file', fileName, 'has been generated.\n'))
}

# Creating a .js file from this
json_strings_from_df(df, 'example.js') 