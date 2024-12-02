// Basic Parameters
const NUM_TRIALS = 10;
const NUM_OPTIONS = 5;
//const RANDOM_SEED = 42;

// State Variables
const STATE_VARIABLES = {
  Time: {levels: 10, pattern: 'Loop'},
  Planet: {levels: 3, pattern: 'Shuffle'}
};

// Arm Variables
const ARM_VARIABLES = {
  Index: {levels: 5, pattern: 'Loop'},
  Color: {levels: 3, pattern: 'Shuffle'},
  Shape: {levels: 3, pattern: 'Shuffle'}
};

// Distribution Links
const DISTRIBUTION_LINKS = [
  {stateVariable: 'Time', stateDistribution: 'Identical', interaction: '+', armDistribution: 'Independent', armVariable: 'Index'},
  {stateVariable: 'Time', stateDistribution: 'Random Walk', interaction: '×', armDistribution: 'Independent', armVariable: 'Index'}
];
