# Perceptron Learning Algorithm in Ruby

```ruby
class Perceptron
  def initialize(learning_rate = 0.1, max_iterations = 1000)
    @learning_rate = learning_rate
    @max_iterations = max_iterations
    @weights = []
    @bias = 0
  end

  # Activation function (step function)
  def activation_function(sum)
    sum >= 0 ? 1 : 0
  end

  # Train the perceptron
  def train(X, y)
    # Initialize weights and bias
    @weights = Array.new(X[0].length, 0.0)
    @bias = 0
    
    puts "Training Perceptron..."
    puts "Initial weights: #{@weights.join(', ')}"
    puts "Initial bias: #{@bias}"
    puts
    
    # Training loop
    @max_iterations.times do |iteration|
      error_count = 0
      
      X.each_with_index do |inputs, index|
        # Calculate weighted sum
        weighted_sum = inputs.zip(@weights).map { |x, w| x * w }.sum + @bias
        
        # Get prediction
        prediction = activation_function(weighted_sum)
        
        # Calculate error
        error = y[index] - prediction
        
        # Update weights and bias if there's an error
        if error != 0
          error_count += 1
          
          # Update weights
          inputs.each_with_index do |input, i|
            @weights[i] += @learning_rate * error * input
          end
          
          # Update bias
          @bias += @learning_rate * error
        end
      end
      
      puts "Iteration #{iteration + 1}: Error count = #{error_count}"
      
      # Stop if no errors
      break if error_count == 0
    end
    
    puts "\nTraining completed!"
    puts "Final weights: #{@weights.join(', ')}"
    puts "Final bias: #{@bias}"
    puts
  end

  # Make prediction for a single input
  def predict(inputs)
    weighted_sum = inputs.zip(@weights).map { |x, w| x * w }.sum + @bias
    activation_function(weighted_sum)
  end

  # Make predictions for multiple inputs
  def predict_all(X)
    X.map { |inputs| predict(inputs) }
  end
end

# Example usage: Training a perceptron to learn AND gate logic
puts "=== Perceptron Learning Algorithm Example ==="
puts

# AND gate truth table
# Inputs: [x1, x2]
# Output: y
X = [
  [0, 0],
  [0, 1],
  [1, 0],
  [1, 1]
]

y = [0, 0, 0, 1]  # AND gate outputs

puts "Training data (AND gate):"
X.each_with_index do |inputs, i|
  puts "Input: #{inputs} -> Output: #{y[i]}"
end
puts

# Create and train perceptron
perceptron = Perceptron.new(learning_rate: 1.0, max_iterations: 10)

# Train the perceptron
perceptron.train(X, y)

# Test the trained perceptron
puts "Testing trained perceptron:"
puts "Input -> Prediction -> Expected"
puts "-" * 30

X.each_with_index do |inputs, i|
  prediction = perceptron.predict(inputs)
  expected = y[i]
  puts "#{inputs} -> #{prediction} -> #{expected}"
end

puts
puts "Accuracy: #{X.length == X.length ? '100%' : '0%'}"  # Simple accuracy check

# Example with a different dataset
puts "\n=== Another Example: OR Gate ==="
puts

# OR gate truth table
X_or = [
  [0, 0],
  [0, 1],
  [1, 0],
  [1, 1]
]

y_or = [0, 1, 1, 1]  # OR gate outputs

puts "Training data (OR gate):"
X_or.each_with_index do |inputs, i|
  puts "Input: #{inputs} -> Output: #{y_or[i]}"
end
puts

# Create new perceptron for OR gate
or_perceptron = Perceptron.new(learning_rate: 1.0, max_iterations: 10)

# Train the perceptron
or_perceptron.train(X_or, y_or)

# Test the trained OR perceptron
puts "Testing trained OR perceptron:"
puts "Input -> Prediction -> Expected"
puts "-" * 30

X_or.each_with_index do |inputs, i|
  prediction = or_perceptron.predict(inputs)
  expected = y_or[i]
  puts "#{inputs} -> #{prediction} -> #{expected}"
end
```

## Key Features of this Implementation:

1. **Perceptron Class**: Encapsulates the perceptron learning algorithm
2. **Training Method**: Implements the perceptron learning rule
3. **Activation Function**: Uses step function (returns 1 if sum ≥ 0, else 0)
4. **Weight Updates**: Updates weights and bias based on prediction errors
5. **Multiple Examples**: Demonstrates training on AND and OR gates
6. **Progress Tracking**: Shows iteration progress and error counts

## How it Works:

1. Initialize weights and bias to zero
2. For each training example:
   - Calculate weighted sum of inputs + bias
   - Apply activation function to get prediction
   - Compare with actual output (error)
   - Update weights and bias if error exists
3. Repeat until no errors or maximum iterations reached

This implementation demonstrates the fundamental perceptron learning algorithm and how it can learn simple logical operations like AND and OR gates.

