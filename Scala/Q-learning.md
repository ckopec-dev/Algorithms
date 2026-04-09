# Q-Learning Algorithm in Scala

Here's a complete implementation of the Q-learning algorithm in Scala:

```scala
import scala.collection.mutable
import scala.util.Random

class QLearning(
    val states: Set[Int],
    val actions: Set[Int],
    val discountFactor: Double = 0.9,
    val learningRate: Double = 0.1,
    val epsilon: Double = 0.1
) {
  
  // Q-table: Map from (state, action) to Q-value
  private val qTable = mutable.Map[(Int, Int), Double]()
  
  // Initialize Q-values to zero for all state-action pairs
  for (state <- states; action <- actions) {
    qTable((state, action)) = 0.0
  }
  
  // Get the maximum Q-value for a given state
  def getMaxQValue(state: Int): Double = {
    actions.map(action => qTable((state, action))).max
  }
  
  // Get the action with the highest Q-value for a given state
  def getBestAction(state: Int): Int = {
    actions.maxBy(action => qTable((state, action)))
  }
  
  // Choose action using epsilon-greedy policy
  def chooseAction(state: Int): Int = {
    if (Random.nextDouble() < epsilon) {
      // Exploration: choose random action
      actions.toSeq(Random.nextInt(actions.size))
    } else {
      // Exploitation: choose best action
      getBestAction(state)
    }
  }
  
  // Update Q-value using the Q-learning update rule
  def updateQValue(
      state: Int, 
      action: Int, 
      reward: Double, 
      nextState: Int
  ): Unit = {
    val currentQ = qTable((state, action))
    val maxNextQ = getMaxQValue(nextState)
    val newQ = currentQ + learningRate * (reward + discountFactor * maxNextQ - currentQ)
    qTable((state, action)) = newQ
  }
  
  // Get Q-value for a specific state-action pair
  def getQValue(state: Int, action: Int): Double = {
    qTable((state, action))
  }
  
  // Print the Q-table (for debugging)
  def printQTable(): Unit = {
    println("Q-Table:")
    for (state <- states.sorted) {
      print(s"State $state: ")
      for (action <- actions.sorted) {
        print(f"Action $action: ${qTable((state, action))}%.2f ")
      }
      println()
    }
  }
}

// Example usage with a simple grid world
object QLearningExample extends App {
  
  // Define states (grid positions)
  val states = Set(0, 1, 2, 3, 4, 5, 6, 7, 8)
  
  // Define actions (0=up, 1=right, 2=down, 3=left)
  val actions = Set(0, 1, 2, 3)
  
  // Create Q-learning agent
  val agent = new QLearning(states, actions, discountFactor = 0.9, learningRate = 0.1, epsilon = 0.1)
  
  // Simple reward function (simplified for example)
  def getReward(state: Int, action: Int, nextState: Int): Double = {
    // Terminal states
    if (state == 8) 10.0  // Goal state
    else if (state == 7) -10.0  // Penalty state
    else -0.1  // Small negative reward for each step
  }
  
  // Simple environment transition function
  def getNextState(state: Int, action: Int): Int = {
    val row = state / 3
    val col = state % 3
    
    val newRow = action match {
      case 0 => math.max(0, row - 1)  // up
      case 2 => math.min(2, row + 1)  // down
      case _ => row  // no vertical change
    }
    
    val newCol = action match {
      case 1 => math.min(2, col + 1)  // right
      case 3 => math.max(0, col - 1)  // left
      case _ => col  // no horizontal change
    }
    
    newRow * 3 + newCol
  }
  
  // Training loop
  println("Training Q-Learning agent...")
  
  val episodes = 1000
  for (episode <- 1 to episodes) {
    var currentState = 0  // Start at state 0
    
    // Run episode until reaching terminal state
    while (currentState != 8 && currentState != 7) {
      val action = agent.chooseAction(currentState)
      val nextState = getNextState(currentState, action)
      val reward = getReward(currentState, action, nextState)
      
      agent.updateQValue(currentState, action, reward, nextState)
      currentState = nextState
    }
  }
  
  // Print final Q-table
  agent.printQTable()
  
  // Print optimal policy
  println("\nOptimal Policy:")
  for (state <- states) {
    val bestAction = agent.getBestAction(state)
    val actionNames = Map(0 -> "Up", 1 -> "Right", 2 -> "Down", 3 -> "Left")
    println(s"State $state: Action ${actionNames(bestAction)}")
  }
  
  // Test the learned policy
  println("\nTesting learned policy:")
  var currentState = 0
  var steps = 0
  val maxSteps = 20
  
  while (currentState != 8 && currentState != 7 && steps < maxSteps) {
    val action = agent.getBestAction(currentState)
    val nextState = getNextState(currentState, action)
    println(s"Step $steps: State $currentState -> Action $action -> State $nextState")
    currentState = nextState
    steps += 1
  }
  
  if (currentState == 8) println("Goal reached!")
  else if (currentState == 7) println("Penalty state reached!")
}
```

## Key Components Explained:

### 1. **QTable Structure**
- Uses a `mutable.Map[(Int, Int), Double]` to store Q-values
- Keys are tuples of (state, action) pairs
- Values are the corresponding Q-values

### 2. **Core Methods**
- `chooseAction()`: Implements epsilon-greedy policy
- `updateQValue()`: Applies the Q-learning update rule
- `getMaxQValue()`: Finds the maximum Q-value for a state
- `getBestAction()`: Returns the action with highest Q-value

### 3. **Q-Learning Update Rule**
```
Q(s,a) ← Q(s,a) + α[r + γ max Q(s',a') - Q(s,a)]
```

### 4. **Training Process**
- Episodes of interaction with environment
- Exploration vs exploitation trade-off
- Convergence to optimal policy over time

### 5. **Example Environment**
- 3x3 grid world with:
  - State 8: Goal (reward = +10)
  - State 7: Penalty (reward = -10)
  - Other states: Small negative reward (-0.1)

This implementation demonstrates the fundamental concepts of Q-learning in a practical Scala context, showing how an agent learns optimal behavior through trial and error.

