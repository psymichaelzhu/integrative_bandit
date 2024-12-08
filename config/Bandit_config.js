// Basic Parameters
const NUM_TRIALS = 10;
const NUM_ARMS = 5;
const REWARD_TYPE = 'numeric';
const FEEDBACK_VERSION = 'contingent';
const COVER_STORY = 'non-social';

// Asymmetry Configuration
const ASYMMETRY_CONFIG = {
  information: {
    pattern: 'Unequal',
    numForcedChoice: 5
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
  Time: {levels: 10, pattern: 'Loop'},
  Planet: {levels:  1, pattern: 'Shuffle'},
  Season: {levels:  4, pattern: 'Shuffle'}
};

// Arm Variables
const ARM_VARIABLES = {
  Index: {levels: 5, pattern: 'Loop'},
  Color: {levels: 1, pattern: 'Shuffle'},
  Shape: {levels: 1, pattern: 'Shuffle'}
};

// Distribution Links
const DISTRIBUTION_LINKS = [
  {stateVariable: 'Time', stateDistribution: 'Identical', interaction: '+', armDistribution: 'Identical', armVariable: 'Index'}
];

const NUM_FORCED_TRIALS = 0;
