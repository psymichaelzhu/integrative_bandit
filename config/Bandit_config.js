// Basic Parameters
const NUM_TRIALS = 10;
const NUM_ARMS = 5;
const RANDOM_SEED = 42;

// State Variables
const STATE_VARIABLES = {
  Time: {levels: 10, pattern: 'Loop'}
};

// Arm Variables
const ARM_VARIABLES = {
  Index: {levels: 5, pattern: 'Loop'},
  Color: {levels: 5, pattern: 'Loop'},
  Shape: {levels: 3, pattern: 'Loop'}
};

// Distribution Links
const DISTRIBUTION_LINKS = [
  {stateVariable: 'Time', stateDistribution: 'Random Walk', interaction: '×', armDistribution: 'Identical', armVariable: 'Index'},
  {stateVariable: 'Time', stateDistribution: 'Identical', interaction: '+', armDistribution: 'Independent', armVariable: 'Index'}
];
