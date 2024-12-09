// Basic Parameters
const NUM_TRIALS = 11;
const NUM_ARMS = 4;
const REWARD_TYPE = 'binary';
const FEEDBACK_VERSION = 'full';
const COVER_STORY = 'social';

// Asymmetry Configuration
const ASYMMETRY_CONFIG = {
  information: {
    pattern: 'Equal',
    numForcedChoice: 3
  },
  noise: {
    pattern: 'Equal',
    level: 'Low',
  },
  cost: {
    pattern: 'Equal',
    level: 'Median',
  }
};

// State Variables
const STATE_VARIABLES = {
  Time: {levels: 11, pattern: 'Loop'},
  Planet: {levels:  1, pattern: 'Shuffle'},
  Season: {levels:  3, pattern: 'Loop'}
};

// Arm Variables
const ARM_VARIABLES = {
  Index: {levels: 4, pattern: 'Loop'},
  Color: {levels: 3, pattern: 'Shuffle'},
  Shape: {levels: 1, pattern: 'Shuffle'},
  Name: {levels: 2, pattern: 'Shuffle'}
};

// Distribution Links
const DISTRIBUTION_LINKS = [
  {stateVariable: ' ', stateDistribution: ' ', interaction: ' ', armDistribution: 'Identical', armVariable: 'Index'},
  {stateVariable: 'Time', stateDistribution: 'Independent', interaction: 'on', armDistribution: 'Independent', armVariable: 'Color'}
];
