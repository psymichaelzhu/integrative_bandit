// Basic Parameters
const NUM_TRIALS = 10;
const NUM_ARMS = 5;
const REWARD_TYPE = 'numeric';
const FEEDBACK_VERSION = 'contingent';
const COVER_STORY = 'non-social';

// Asymmetry Configuration
const ASYMMETRY_CONFIG = {
  information: {
    pattern: 'Equal',
    numForcedChoice: 0
  },
  noise: {
    pattern: 'Equal',
    level: 'None',
  },
  cost: {
    pattern: 'Equal',
    level: 'None',
  }
};

// State Variables
const STATE_VARIABLES = {
  Time: {levels: 10, pattern: 'Loop'}
};

// Arm Variables
const ARM_VARIABLES = {
  Index: {levels: 5, pattern: 'Loop'}
};

// Distribution Links
const DISTRIBUTION_LINKS = [];
