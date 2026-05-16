# Q-Learning Algorithm in Ruby

Here's a complete implementation of the Q-learning algorithm in Ruby:

```ruby
class QLearningAgent
  def initialize(actions, learning_rate: 0.1, discount_factor: 0.9, exploration_rate: 0.1)
    @actions = actions
    @learning_rate = learning_rate
    @discount_factor = discount_factor
    @exploration_rate = exploration_rate
    @q_table = Hash.new { |hash, key| hash[key] = Hash.new(0) }
  end

  def get_q_value(state, action)
    @q_table[state][action]
  end

  def choose_action(state)
    if rand < @exploration_rate
      # Exploration: choose random action
      @actions.sample
    else
      # Exploitation: choose best action
      best_action = @actions.max_by { |action| @q_table[state][action] }
      best_action
    end
  end

  def update_q_value(state, action, reward, next_state)
    current_q = @q_table[state][action]
    max_next_q = @actions.map { |a| @q_table[next_state][a] }.max || 0
    
    # Q-learning update rule
    new_q = current_q + @learning_rate * (reward + @discount_factor * max_next_q - current_q)
    
    @q_table[state][action] = new_q
  end

  def q_table
    @q_table
  end
end

# Example usage: Simple grid world
class GridWorld
  def initialize
    @states = []
    @actions = [:up, :down, :left, :right]
    @agent = QLearningAgent.new(@actions, learning_rate: 0.1, discount_factor: 0.9, exploration_rate: 0.1)
    
    # Create grid states (0,0) to (2,2)
    (0..2).each do |i|
      (0..2).each do |j|
        @states << [i, j]
      end
    end
  end

  def get_reward(state)
    # Reward for reaching goal state (2,2)
    if state == [2, 2]
      10
    elsif state == [1, 1]  # Penalty for obstacle
      -10
    else
      -1  # Small penalty for each step
    end
  end

  def get_next_state(current_state, action)
    x, y = current_state
    
    case action
    when :up
      x = [0, x - 1].max
    when :down
      x = [2, x + 1].min
    when :left
      y = [0, y - 1].max
    when :right
      y = [2, y + 1].min
    end
    
    # Check for obstacle
    if [x, y] == [1, 1]
      current_state  # Stay in place if obstacle
    else
      [x, y]
    end
  end

  def train(episodes: 1000)
    episodes.times do |episode|
      current_state = [0, 0]  # Start at top-left corner
      
      # Run episode until reaching goal
      100.times do
        action = @agent.choose_action(current_state)
        next_state = get_next_state(current_state, action)
        reward = get_reward(next_state)
        
        @agent.update_q_value(current_state, action, reward, next_state)
        
        current_state = next_state
        
        # Break if reached goal
        break if current_state == [2, 2]
      end
    end
  end

  def print_q_table
    puts "Q-Table:"
    @states.each do |state|
      print "State #{state}: "
      @actions.each do |action|
        print "#{action}:#{@agent.get_q_value(state, action).round(2)} "
      end
      puts
    end
  end

  def print_policy
    puts "\nPolicy (best action for each state):"
    @states.each do |state|
      best_action = @actions.max_by { |action| @agent.get_q_value(state, action) }
      puts "State #{state}: #{best_action}"
    end
  end
end

# Run the example
puts "Q-Learning Example - Grid World"
puts "=" * 30

# Create and train the agent
world = GridWorld.new
world.train(episodes: 1000)

# Display results
world.print_q_table
world.print_policy

# Test a few specific paths
puts "\nTesting specific state transitions:"
agent = world.instance_variable_get(:@agent)
puts "Q(0,0):up = #{agent.get_q_value([0,0], :up).round(2)}"
puts "Q(0,0):right = #{agent.get_q_value([0,0], :right).round(2)}"
puts "Q(1,0):down = #{agent.get_q_value([1,0], :down).round(2)}"
```

## Key Components Explained:

### 1. **QLearningAgent Class**
- **Q-Table**: Hash-based storage of state-action values
- **choose_action()**: Implements epsilon-greedy policy
- **update_q_value()**: Applies the Q-learning update rule
- **get_q_value()**: Retrieves current Q-values

### 2. **Q-Learning Update Rule**
```
Q(s,a) ← Q(s,a) + α[r + γ max Q(s',a') - Q(s,a)]
```

### 3. **Key Parameters**
- **α (learning_rate)**: How quickly to update Q-values
- **γ (discount_factor)**: Importance of future rewards
- **ε (exploration_rate)**: Probability of exploring vs exploiting

### 4. **Example Output**
The algorithm will learn to navigate from (0,0) to (2,2) while avoiding the obstacle at (1,1), demonstrating how Q-learning discovers optimal policies through trial and error.

This implementation shows how Q-learning can be applied to simple environments and can be extended to more complex problems by modifying the state space, actions, and reward structure.

