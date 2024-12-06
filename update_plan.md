UI上的其他内容！！



## function
通过UI进行设置众多参数 一起保存

incentive (paper)
instruction


UI
不确定性
forced choice



**不对称性*
---

**document**


---
**服务器**



复用
复用：图性质

# 检查 /next 3
- Jspsych的内容，学习 修改
- 随机化
- 服务器


保存参数: 提前存
id 不同的csv

---
# UI

## bug
shiny闪退








问卷





- shuffle的话：随机之后选择



# trial: /next 1
- 不对称性的补充 295 363
    不确定性
        对应的参数
    forced choice: 数量bug
    成本
    奖励
新增加两个内容：
1. uncertainty: uncertainty是按照variable和distribution生成一个uncertainty序列 比如variable=index distribution=independent
就是说uncertainty在index之间不相同

uncertainty反映的是一个选项的噪音程度(numeric reward的奖励会反映在两个方面：
1. 对reward_matrix每一个arm的均值进行一定的substraction
2. 在trial阶段





扩展不对称性：
trial:
    不确定性
    成本
    奖励
feedback:
    信息
    成本
forced choice



# 可有可无 精简
- 指导语和界面文字复用
- 结构优化
- incentive  配置：
    sensitive to num 等
- 删除遗留的名字
- 修改变量：Context-State








// Generate context image sequence based on Planet matrix
function generateContextImageSequence(stateMatrices) {
  // Get Planet matrix
  const planetMatrix = stateMatrices.Planet;
  if (!planetMatrix) {
      console.error("Planet matrix not found in stateMatrices");
      return [];
  }

  // Convert one-hot encoding to planet indices
  const contextSequence = planetMatrix.map(trial => {
      // Find the index of 1 in the trial array (convert to 1-based index)
      const planetIndex = trial.indexOf(1) + 1;
      // Generate image path
      return `img/Planet/${planetIndex}.png`;
  });

  return contextSequence;
}
const IMG_CONTEXT = generateContextImageSequence(stateMatrices);
const NAME_PLANET = [
"Nova",
"Solis",
"Aether",
"Zephyr",
"Lumis",
"Orion",
"Astra",
"Vora",
"Kryos",
"Pyra"
];
const NAME_CONTEXT = IMG_CONTEXT.map((_, index) => NAME_PLANET[index]);
console.log(NAME_CONTEXT);
console.log(IMG_CONTEXT);


const contextNames = [
"Nova",
"Solis",
"Aether",
"Zephyr",
"Lumis",
"Orion",
"Astra",
"Vora",
"Kryos",
"Pyra"
];





//load images (context and arm)
function generateImagePath(imageType, shapeColorPair) {
  const [shape, color] = shapeColorPair;
  const folder = imageType === 'arm' ? `${SOCIAL_VERSION}/arm` : 'context';
  return `img/${folder}/${shape}-${color}.png`;
}
function shuffleArray(array) {
  for (let i = array.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [array[i], array[j]] = [array[j], array[i]];
  }
  return array;
}

// Generate random context sequence
function generateContextSequence(numTrials, numContexts) {
  let sequence = [];
  for (let i = 0; i < numTrials; i++) {
      sequence.push(Math.floor(Math.random() * numContexts));
  }
  return sequence;
}
// Generate context sequence for all trials
const CONTEXT_INDICES = generateContextSequence(NUM_TRIALS + NUM_FORCED_TRIALS, NUM_CONTEXTS);

