# Preface



# Basic configuration
## Overview
In this section, we will go through some basic setup of bandit task.

## Conceptual Introduction
- `NUM_TRIALS`: Number of trials in the task
- `NUM_ARMS`: Number of arms in the task
- `REWARD_TYPE`: Type of reward in the task
   - "binary": Reward is either 0 or 1, following a Bernoulli distribution. Reward probability is given by the reward matrix.
   - "numerical": Reward is a integer between 0 and 100, drawn from a Gaussian distribution, whose mean is given by the reward matrix. 
- `FEEDBACK_VERSION`: Version of feedback in the task
   - "full": Feedback is given for every arm whether it is chosen or not
   - "contingent": Feedback is given only for the chosen arm
- `COVER_STORY`: Cover story of the task
   - "social": Trading with aliens
   - "non-social": Mining at different ores

## Setup with UI
![[Pasted image 20241207234217.png]]
Every field shown above can be set in UI.

> [!NOTE]
> `SEED` is also in UI, but it is only for illustration purpose (To ensure the reproducibility of the reward matrix). It does not affect the actual task in any way.


# Manipulation configuration
## Overview
In this section, we will go through setup of three manipulations that will influence the bandit task performance, but not directly through the reward matrix.

## Conceptual Introduction
#### Manipulation Aspects
- **Information**: (Horizontal task, Wilson et al., 2014) Information is manipulated through chosen count in the forced choice phase. Being chosen more brings more information.
- **Noise**: (Two-armed bandit task, Gershman, 2018) Noise is manipulated through the variance level of the reward generation process. Larger variance means more noise.
- **Cost**: (Search task, Bhatia et al., 2021) The cost is manipulated through the price of sampling an arm (can be regarded as a discount of reward mean). Higher price means higher cost.

> [!NOTE]
> Noise and cost manipulation are only available for numerical reward type.
#### Manipulation Levels
There are four possible levels of each manipulation. 
   - "None": N = 0
   - "Low": N = 1
   - "Medium": N = 5
   - "High": N = 10
For Information, N is the number of forced choices; for Noise and Cost, N is the variance and price of the reward generation process.
#### Two Asymmetries
Those manipulations can be asymmetric between sessions, being **identical among arms** but different across sessions (e.g. Session1 has high noise than Session2). In this case, `Level` field needs to be specified. The manipulation for each arm will be set to the `Level` value.
Or they can be asymmetric within session, being **different among arms** (e.g. Arm1 has high information than Arm2).  In this case, `Level` field is not needed. The manipulation for each arm is sampled from the manipulation level list.
> [!NOTE]
> The manipulations are always consistent within a session, across different trials.

#### Availability to Participants
All of those manipulations can be explicitly informed to participants. For example, by showing participants the variance of each arm, they will know the noise levels.
Or manipulations can be hidden from participants, in which case participants can only infer the levels based on their own experience. 
> [!NOTE]
> Information manipulation (forced choice) can only be explicit.
> Noise is usually hidden; while cost is usually explicit.

## Setup with UI
![[Pasted image 20241208004007.png]]
- `Pattern` specifies whether the manipulation is the same or different among arms.
   - "Equal": Identical among arms, equal to `Level`.
   - "Unequal": Different among arms, sampled from the manipulation level list (0, 1, 5, 10)
- `Level` field is only available when `Pattern` = "Equal". It determines the level of the manipulation. 
- `Explicitly Provided`: Only for noise and cost manipulation. Whether the manipulation is explicitly informed to participants. 
- `# Forced Choice`: Only for information manipulation with `Pattern` = "Unequal". It determines the total number of forced choices in the forced choice phase. The samples will be scaled to avoid undesired excessive forced choices. For example, if there are only 10 trials, it's easy for the sum of unequal samples to exceed the total trial number.


# Feature configuration
## Overview
In this section, we will go through the setup of the features.
This setup will determine the sequence of the features across trials or arms, which is crucial for the reward generation (next chapter `Reward Configuration`) and stimulus presentation.

## Conceptual Introduction
Every feature needs two properties to determine its sequence across trials or arms:
- Levels: How many levels that feature has
- Pattern: How to allocate its levels across its dimension (trials/arms)
   - `Shuffle`: Randomly assign levels with guaranteed similar number of each level
   - `Loop`: Repeating sequence with ascending index
   - `Random`: (less frequently used) Random assignment with equal probability
The resulting allocation matrix is a one-hot encoding matrix, with shape `total_length x levels`. One represents for that specific trial/arm, this feature takes this level.

Every feature belongs to one of the two categories: **trial-based** or **arm-based**.
For each category, there can be multiple features. The allocation matrix of each feature is independent.
By default, trial-based category has one basic feature, `Trial`,indicating the trial index, with levels of `NUM_TRIALS`, following the "Loop" pattern; arm-based category has one basic feature, `Arm`, indicating the arm index, with levels of `NUM_ARMS`, following the "Loop" pattern.
> [!EXAMPLE]
> Let's say there are five trials (`NUM_TRIALS`=5), the allocation matrix of `Trial` is:
> $$
> \begin{bmatrix}
> 1 & 0 & 0 & 0 & 0 \\
> 0 & 1 & 0 & 0 & 0 \\
> 0 & 0 & 1 & 0 & 0 \\
> 0 & 0 & 0 & 1 & 0 \\
> 0 & 0 & 0 & 0 & 1 \\
> \end{bmatrix}
> $$
> This means for every trial, it has a unique ordered index, from 1 to `NUM_TRIALS`.
> Similarly, there are four arms (`NUM_ARMS`=4), the allocation matrix of `Arm` is:
> $$
> \begin{bmatrix}
> 1 & 0 & 0 & 0 \\
> 0 & 1 & 0 & 0 \\
> 0 & 0 & 1 & 0 \\
> 0 & 0 & 0 & 1 \\
> \end{bmatrix}
> $$
> This means for every arm, it has a unique ordered index, from 1 to `NUM_ARMS`.

In our setting, we have one trial-based feature for stimuli, `Planet`, and two arm-based features for stimuli, `Color` and `Shape`. Those features shall determine the stimulus presentation.

> [!EXAMPLE]
> `Planet` is a trial-based feature, as it might change across trials.
>  Let's say we want there to be three planets, and planets changes in a certain order (e.g. Trial1-Earth, Trial2-Venus, Trial3-Mercury, Trial4-Earth, Trial5-Venus). 
>  Then `Planet` should be put in the `Trial-Based Feature` tab, with `levels` = 3, and allocation `pattern` = "Loop".
>  The resulting allocation matrix of `Planet` is:
>  $$
>  \begin{bmatrix}
>  1 & 0 & 0 \\
>  0 & 1 & 0 \\
>  0 & 0 & 1 \\
>  1 & 0 & 0 \\
>  0 & 1 & 0 \\
>  \end{bmatrix}
>  $$
> Row are trials, and columns are possible planets. $M_{1,1}=1$ means on the first trial, we are on the first planet, Earth.
>
> `Color` is an arm-based feature, as it might be different among arms.
>  Let's say we want there to be two colors, and colors are randomly assigned to 4 different arms, with similar number of each color (e.g. Arm1-Orange, Arm2-Red, Arm3-Red, Arm4-Orange).
>  Then `Color` should be put in the `Arm-Based Feature` tab, with `levels` = 2, and allocation `pattern` = "Shuffle".
>  The resulting allocation matrix of `Color` is:
>  $$
>  \begin{bmatrix}
>  0 & 1 \\
>  1 & 0 \\
>  1 & 0 \\
>  0 & 1 \\
>  \end{bmatrix}
>  $$
> Row are arms, and columns are possible colors. $M_{3,1}=1$ means the third arm takes the first color, Red.
> Similarly for arm-based feature `Shape`, the following matrix represents two shapes randomly assigned to 4 different arms (e.g. Arm1-Square, Arm2-Droplike, Arm3-Square, Arm4-Square):
>  $$
>  \begin{bmatrix}
>  0 & 1 \\
>  1 & 0 \\
>  0 & 1 \\
>  1 & 0 \\
>  \end{bmatrix}
>  $$
> Together, our stimulus presentation on the first trial looks like this:
> ![[Pasted image 20241207225220.png]]

Besides those basic or stimulus-related features, you can also add your own features. 
For example, `season` can be a trial-based feature, with levels of `4` (e.g. Spring, Summer, Autumn, Winter), following the "Loop" pattern. By doing so, you might be able to create a seasonality in reward generation (see `Reward Configuration` chapter). 
Although those self-defined features can influence the reward generation, they are not gonna be explicitly presented in the task, unless you modify the stimulus presentation part of the experiment script.

## Setup with UI
![[Pasted image 20241207231819.png]]
To **add** a feature:
1. Select the corresponding panel for trial-based or arm-based feature.
> If the feature changes across trials, go to `Trial-Based Feature`.
> If the feature changes across arms, go to `Arm-Based Feature`.
2. Type the name of the feature in the `Name` field.
3. Set the number of levels in the `Levels` field.
> How many different possible values that this feature might take.
4. Select the pattern of the feature in the `Pattern` field.
> How to allocate its levels across its dimension (trials/arms).
5. Click `Update` to add the feature to the list.

To **delete** a feature, click the Delete button on the right of the feature.
To **edit** a feature, declare and update it like "add". It will automatically overwrite the existing record.

>[!NOTE]
> Stimulus-related feature (`Planet`, `Color`, `Shape`) are not deletable but editable.
> Basic features (`Trial`, `Arm`) are neither editable or deletable.
> 
> If you change the number of trials (`NUM_TRIALS`) or arms (`NUM_ARMS`) from the previous setup (`Basic Configuration`), the levels of basic features will be automatically updated.



# Reward configuration
## Overview
The reward matrix is the core of the bandit task. Simply put, the reward matrix specifies how rewarding a arm on a given trial is.
The `Feature Configuration` chapter specifies how different features are located across trials or arms. In this chapter, we will specify how the features influence the reward.

## Conceptual Introduction
#### Decomposition into feature mappings
As mentioned in the previous chapter `Feature Configuration`, each arm has a set of arm-based features associated with it, and each trial has a set of trial-based features associated with it.
Essentially, the mapping from the arm or trial to the reward can actually be decomposed into mappings from different features (or their interactions) to reward.
> [!EXAMPLE]
> In the structural bandit (Wu et al., 2018), arms with close locations have similar rewards, therefore the location (arm-based) determines the reward.
> ![[Pasted image 20241208111755.png]]
> In the contextual bandit task (Schulz et al., 2018), for a single arm, the rewards are different in different contexts, therefore the interaction between the context (trial-based) and arm index (arm-based) determines the reward.
> ![[Pasted image 20241208111511.png]]

#### Mapping forms
##### Single feature
The mapping from a single feature to reward can take the following forms:
- Identical: every levels of the feature has the identical reward
- Linear: reward is correlated linearly with the feature level
- Independent: different levels of the feature has independent different rewards
- Random walk: reward is a random walk of the feature level

##### Interaction of two features
The mapping from the interaction of two features to reward can be understood as conditional mapping.
> [!EXAMPLE]
> In the restless bandit task (Daw et al., 2006), the rewards change across trials, while the change pattern is inconsistent among arms, therefore we can say the mapping from trial index to reward is conditional on arm index. 
> ![[Pasted image 20241208111643.png]]
> For each arm, the mapping from trial index to reward is a random walk process, while the random walk seed is determined by the arm index.

For the interaction, the conditioned feature (e.g. trial index in the restless bandit example) is the feature that directly determines the mapping, taking the forms like single feature (Identical, Linear, Independent, Randomwalk). While the conditioning feature (e.g. arm index) determines the seed of the mappings.

> [!NOTE]
> To avoid complexity, for single feature, we only consider the arm-based features; for interaction of multiple features, we only consider the interaction between arm-based features and trial-based features.


#### Generate reward matrix
Previously, we have specified how the features are allocated across trials or arms (`Feature Configuration` chapter). Feature allocation can be formally represented as a one-hot encoding matrix.
In this chapter, we've also specified how the features influence the reward (`Mapping Forms` part). Mapping forms can be formally represented as a matrices.
Combining the feature allocation matrix and mapping matrix, we can generate the reward matrix of this specific feature mapping.
##### Single feature
For a single feature reward matrix, we only consider the allocation and mapping of one arm-based feature:
- The allocation matrix is a one-hot encoding matrix, with shape `NUM_ARMS` x `levels`. One represents for that specific trial/arm, this feature takes this level.
- The mapping matrix is a matrix with shape `levels` x 1. Each row represents for a level of the feature, and each value represents for a reward.
Multiplying the allocation matrix and mapping matrix, we can generate the reward matrix of this specific feature mapping, with shape `NUM_ARMS` x 1.
> [!EXAMPLE]
> #### Structural Bandit
> Let's consider a structural bandit task where larger arms are more rewarding. (Assuming `NUM_ARMS` = 4, `NUM_TRIALS` = 5)
> 1. Specify the feature: `Color` is an arm-based feature , with levels of `3` (Red, Green, Blue).
> 2. Specify the allocation pattern: The allocation pattern is `Shuffle`, which means the feature is allocated randomly with roughly equal number of each level.
> ![[Pasted image 20241208123521.png]]
> $$
> \begin{bmatrix}
> 1 & 0 & 0 \\
> 0 & 1 & 0 \\
> 0 & 0 & 1 \\
> 0 & 1 & 0 \\
> \end{bmatrix}
> $$
> Rows are arm indices, and columns are possible colors. There are 4 arms, and 3 possible colors. The first arm is Red. The second and forth arms are Green. The third arm is Blue.
> 3. Specify the mapping form: The reward is linearly correlated with the color level. Let's say warm colors are more rewarding, while cool colors are less rewarding.
> ![[Pasted image 20241208123824.png]]
> $$
> \begin{bmatrix}
> 3 \\
> 2 \\
> 1 \\
> \end{bmatrix}
> $$
> Rows are color levels, and columns are arm indices.
> 4. Generate the reward matrix: Multiplying the feature allocation matrix and mapping matrix, we can generate the reward matrix for each arm.
> $$
> \begin{bmatrix}
> 1 & 0 & 0 \\
> 0 & 1 & 0 \\
> 0 & 0 & 1 \\
> 0 & 1 & 0 \\
> \end{bmatrix} \times
> \begin{bmatrix}
> 3 \\
> 2 \\
> 1 \\
> \end{bmatrix} =
> \begin{bmatrix}
> 1 \times 3 + 0 \times 2 + 0 \times 1 \\
> 0 \times 3 + 1 \times 2 + 0 \times 1 \\
> 0 \times 3 + 0 \times 2 + 1 \times 1 \\
> 0 \times 3 + 1 \times 2 + 0 \times 1 \\
> \end{bmatrix} =
> \begin{bmatrix}
> 3 \\
> 2 \\
> 1 \\
> 2 \\
> \end{bmatrix}
> $$
> Then we know that on this feature dimension (color), arm 1 is the best, arm 2 and 4 are of medium reward, arm 3 is the worst.
> ![[Pasted image 20241208124540.png]]

##### Interaction of two features
For the interaction of two features, we consider the allocation and mapping of two features: Trial-based feature conditional on arm-based feature.
For the trial-based feature, the allocation matrix is a one-hot encoding matrix, with shape `NUM_TRIALS` x `levels`. One represents for that specific trial, this feature takes this level.
For the arm-based feature, the allocation matrix is a one-hot encoding matrix, with shape `NUM_ARMS` x `levels`. One represents for that specific arm, this feature takes this level.
The mapping matrix is a matrix with shape `trial-based levels` x `arm-based levels`. Each row represents for a level of the trial-based feature, and each column represents for a level of the arm-based feature. Each value represents for a reward.
As mentioned before, the mapping from trial-based feature to reward is conditional on the arm-based feature. That means each column of the mapping matrix follows the same form like single feature mapping, but with different seed across columns.
Multiplying the two allocation matrices and the mapping matrix, we can generate the reward matrix of this specific interaction mapping, with shape `NUM_ARMS` x `NUM_TRIALS`.


## Setup with UI
