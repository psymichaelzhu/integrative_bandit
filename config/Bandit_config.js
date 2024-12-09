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
    informed: false
  },
  cost: {
    pattern: 'Equal',
    level: 'None',
    informed: true
  }
};

// Trial Features
const STATE_VARIABLES = {
  Index: {levels: 10, pattern: 'Loop'},
  Planet: {levels:  1, pattern: 'Shuffle'}
};

// Arm Features
const ARM_VARIABLES = {
  Index: {levels: 5, pattern: 'Loop'},
  Color: {levels: 1, pattern: 'Shuffle'},
  Shape: {levels: 1, pattern: 'Shuffle'}
};

// Reward Configuration
const REWARD_CONFIG = [
  {function: 'Identical', type1: 'Trial', name1: 'Index'},
  {function: 'Identical', type1: 'Trial', name1: 'Index', type2: 'Arm', name2: 'Color'},
  {function: 'Monotonic', type1: 'Arm', name1: 'Color', type2: 'Trial', name2: 'Planet'}
];