// Basic Parameters
const NUM_TRIALS = 5;
const NUM_OPTIONS = 4;
//const RANDOM_SEED = 42;

// State Variables
const STATE_VARIABLES = {
  Time: {levels: 10, pattern: 'Loop'},
  Planet: {levels: 3, pattern: 'Loop'}
};

// Arm Variables
const ARM_VARIABLES = {
  Index: {levels: 5, pattern: 'Loop'},
  Color: {levels: 2, pattern: 'Shuffle'},
  Shape: {levels: 2, pattern: 'Shuffle'}
};

// Distribution Links
const DISTRIBUTION_LINKS = [
  {stateVariable: 'Time', stateDistribution: 'Identical', interaction: '+', armDistribution: 'Independent', armVariable: 'Index'},
  {stateVariable: 'Time', stateDistribution: 'Monotonic', interaction: '×', armDistribution: 'Independent', armVariable: 'Index'}
];