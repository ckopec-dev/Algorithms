# Backpropagation Algorithm in Ruby

Here's a complete implementation of a simple neural network with backpropagation using Ruby:

```ruby
class NeuralNetwork
  def initialize(input_size, hidden_size, output_size, learning_rate = 0.1)
    @input_size = input_size
    @hidden_size = hidden_size
    @output_size = output_size
    @learning_rate = learning_rate
    
    # Initialize weights with random values
    @weights_input_hidden = Array.new(hidden_size) { Array.new(input_size) { rand * 2 - 1 } }
    @weights_hidden_output = Array.new(output_size) { Array.new(hidden_size) { rand * 2 - 1 } }
    
    # Initialize biases
    @bias_hidden = Array.new(hidden_size) { rand * 2 - 1 }
    @bias_output = Array.new(output_size) { rand * 2 - 1 }
  end
  
  # Sigmoid activation function
  def sigmoid(x)
    1.0 / (1.0 + Math.exp(-x))
  end
  
  # Derivative of sigmoid
  def sigmoid_derivative(x)
    x * (1.0 - x)
  end
  
  # Forward propagation
  def forward(input)
    # Hidden layer
    @hidden_layer = []
    @weights_input_hidden.each_with_index do |weights, i|
      sum = weights.each_with_index.reduce(@bias_hidden[i]) do |acc, (weight, j)|
        acc + weight * input[j]
      end
      @hidden_layer[i] = sigmoid(sum)
    end
    
    # Output layer
    @output_layer = []
    @weights_hidden_output.each_with_index do |weights, i|
      sum = weights.each_with_index.reduce(@bias_output[i]) do |acc, (weight, j)|
        acc + weight * @hidden_layer[j]
      end
      @output_layer[i] = sigmoid(sum)
    end
    
    @output_layer
  end
  
  # Backpropagation
  def backpropagate(input, target)
    # Forward pass (already computed in forward method)
    
    # Calculate output layer errors
    output_errors = []
    @output_layer.each_with_index do |output, i|
      output_errors[i] = target[i] - output
    end
    
    # Calculate output layer deltas
    output_deltas = []
    output_errors.each_with_index do |error, i|
      output_deltas[i] = error * sigmoid_derivative(@output_layer[i])
    end
    
    # Calculate hidden layer errors
    hidden_errors = []
    @weights_hidden_output.each_with_index do |weights, i|
      error = 0
      weights.each_with_index do |weight, j|
        error += weight * output_deltas[i]
      end
      hidden_errors[i] = error
    end
    
    # Calculate hidden layer deltas
    hidden_deltas = []
    hidden_errors.each_with_index do |error, i|
      hidden_deltas[i] = error * sigmoid_derivative(@hidden_layer[i])
    end
    
    # Update weights and biases (output layer)
    @weights_hidden_output.each_with_index do |weights, i|
      weights.each_with_index do |_, j|
        # Update weight
        @weights_hidden_output[i][j] += @learning_rate * output_deltas[i] * @hidden_layer[j]
      end
      # Update bias
      @bias_output[i] += @learning_rate * output_deltas[i]
    end
    
    # Update weights and biases (hidden layer)
    @weights_input_hidden.each_with_index do |weights, i|
      weights.each_with_index do |_, j|
        # Update weight
        @weights_input_hidden[i][j] += @learning_rate * hidden_deltas[i] * input[j]
      end
      # Update bias
      @bias_hidden[i] += @learning_rate * hidden_deltas[i]
    end
  end
  
  # Train the network
  def train(input_data, target_data, epochs)
    epochs.times do |epoch|
      total_error = 0
      input_data.each_with_index do |input, i|
        target = target_data[i]
        
        # Forward propagation
        output = forward(input)
        
        # Calculate error
        error = target.each_with_index.reduce(0) do |acc, (t, j)|
          acc + (t - output[j]) ** 2
        end
        total_error += error
        
        # Backpropagation
        backpropagate(input, target)
      end
      
      puts "Epoch #{epoch + 1}/#{epochs}, Error: #{total_error}" if epoch % 100 == 0
    end
  end
  
  # Predict
  def predict(input)
    forward(input)
  end
end

# Example usage: XOR problem
puts "Training Neural Network for XOR problem..."

# XOR input data
input_data = [
  [0, 0],
  [0, 1],
  [1, 0],
  [1, 1]
]

# XOR target data
target_data = [
  [0],
  [1],
  [1],
  [0]
]

# Create neural network (2 inputs, 4 hidden neurons, 1 output)
network = NeuralNetwork.new(2, 4, 1, 0.5)

# Train the network
network.train(input_data, target_data, 1000)

# Test the network
puts "\nTesting the trained network:"
input_data.each_with_index do |input, i|
  output = network.predict(input)
  puts "Input: #{input} -> Output: #{output[0].round(4)} (Target: #{target_data[i][0]})"
end
```

## Key Components Explained:

### 1. **Neural Network Structure**
- Input layer: 2 neurons
- Hidden layer: 4 neurons  
- Output layer: 1 neuron

### 2. **Forward Propagation**
- Input values are multiplied by weights and added to biases
- Sigmoid activation function is applied
- Values flow from input to hidden to output layer

### 3. **Backpropagation Algorithm**
- Calculate output layer errors (difference between predicted and actual)
- Calculate output layer deltas (errors multiplied by derivative of activation)
- Calculate hidden layer errors (backpropagated from output layer)
- Calculate hidden layer deltas
- Update weights and biases using gradient descent

### 4. **Training Process**
- Forward pass to get predictions
- Backward pass to calculate errors
- Weight updates based on learning rate and errors
- Repeat for multiple epochs

This implementation demonstrates the core concepts of backpropagation: forward propagation to make predictions, backward propagation to calculate errors, and weight updates to minimize prediction errors.

