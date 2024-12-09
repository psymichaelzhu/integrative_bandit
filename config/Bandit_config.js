// Basic Parameters
const NUM_TRIALS = 30;
const NUM_ARMS = 8;
const REWARD_TYPE = 'numeric';
const FEEDBACK_VERSION = 'contingent';
const COVER_STORY = 'non-social';

// Asymmetry Configuration
const ASYMMETRY_CONFIG = {
  information: {
    pattern: 'Unequal',
    numForcedChoice: 10
  },
  noise: {
    pattern: 'Equal',
    level: 'High',
  },
  cost: {
    pattern: 'Unequal',
    level: 'None',
  }
};

// State Variables
const STATE_VARIABLES = {
  Time: {levels: 30, pattern: 'Loop'},
  Planet: {levels:  1, pattern: 'Shuffle'},
  Season: {levels:  4, pattern: 'Loop'}
};

// Arm Variables
const ARM_VARIABLES = {
  Index: {levels: 8, pattern: 'Loop'},
  Color: {levels: 1, pattern: 'Shuffle'},
  Shape: {levels: 1, pattern: 'Shuffle'},
  Name: {levels: 4, pattern: 'Shuffle'}
};

// Distribution Links
const DISTRIBUTION_LINKS = [
  {stateVariable: ' ', stateDistribution: ' ', interaction: ' ', armDistribution: 'Identical', armVariable: 'Index'},
  {stateVariable: 'Time', stateDistribution: 'Independent', interaction: 'on', armDistribution: 'Identical', armVariable: 'Index'}
];