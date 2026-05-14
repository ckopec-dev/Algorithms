# Backpropagation Algorithm in Lua

Here's a complete implementation of a simple neural network with backpropagation in Lua:

```lua
-- Simple Neural Network with Backpropagation in Lua

-- Sigmoid activation function
function sigmoid(x)
    return 1 / (1 + math.exp(-x))
end

-- Derivative of sigmoid function
function sigmoid_derivative(x)
    return x * (1 - x)
end

-- Neural Network class
NeuralNetwork = {}
NeuralNetwork.__index = NeuralNetwork

function NeuralNetwork:new(input_nodes, hidden_nodes, output_nodes)
    local obj = {
        input_nodes = input_nodes,
        hidden_nodes = hidden_nodes,
        output_nodes = output_nodes,
        
        -- Initialize weights with random values
        weights_ih = {} -- input to hidden
        weights_ho = {} -- hidden to output
        
        -- Initialize biases
        bias_h = {}
        bias_o = {}
    }
    
    setmetatable(obj, NeuralNetwork)
    
    -- Initialize weights
    for i = 1, obj.hidden_nodes do
        obj.weights_ih[i] = {}
        for j = 1, obj.input_nodes do
            obj.weights_ih[i][j] = math.random() * 2 - 1  -- Random between -1 and 1
        end
    end
    
    for i = 1, obj.output_nodes do
        obj.weights_ho[i] = {}
        for j = 1, obj.hidden_nodes do
            obj.weights_ho[i][j] = math.random() * 2 - 1
        end
    end
    
    -- Initialize biases
    for i = 1, obj.hidden_nodes do
        obj.bias_h[i] = math.random() * 2 - 1
    end
    
    for i = 1, obj.output_nodes do
        obj.bias_o[i] = math.random() * 2 - 1
    end
    
    return obj
end

-- Forward propagation
function NeuralNetwork:forward(input_array)
    -- Convert input to matrix
    local inputs = {}
    for i = 1, #input_array do
        inputs[i] = {input_array[i]}
    end
    
    -- Calculate hidden layer
    local hidden = {}
    for i = 1, self.hidden_nodes do
        hidden[i] = {0}
        for j = 1, self.input_nodes do
            hidden[i][1] = hidden[i][1] + self.weights_ih[i][j] * inputs[j][1]
        end
        hidden[i][1] = hidden[i][1] + self.bias_h[i]
        hidden[i][1] = sigmoid(hidden[i][1])
    end
    
    -- Calculate output layer
    local outputs = {}
    for i = 1, self.output_nodes do
        outputs[i] = {0}
        for j = 1, self.hidden_nodes do
            outputs[i][1] = outputs[i][1] + self.weights_ho[i][j] * hidden[j][1]
        end
        outputs[i][1] = outputs[i][1] + self.bias_o[i]
        outputs[i][1] = sigmoid(outputs[i][1])
    end
    
    return outputs, hidden
end

-- Backpropagation
function NeuralNetwork:train(input_array, target_array, learning_rate)
    -- Forward propagation
    local outputs, hidden = self:forward(input_array)
    
    -- Convert targets to matrix
    local targets = {}
    for i = 1, #target_array do
        targets[i] = {target_array[i]}
    end
    
    -- Calculate output layer errors
    local output_errors = {}
    for i = 1, self.output_nodes do
        output_errors[i] = {targets[i][1] - outputs[i][1]}
    end
    
    -- Calculate output layer gradients
    local output_gradients = {}
    for i = 1, self.output_nodes do
        output_gradients[i] = {output_errors[i][1] * sigmoid_derivative(outputs[i][1])}
    end
    
    -- Calculate hidden layer errors
    local hidden_errors = {}
    for i = 1, self.hidden_nodes do
        hidden_errors[i] = {0}
        for j = 1, self.output_nodes do
            hidden_errors[i][1] = hidden_errors[i][1] + self.weights_ho[j][i] * output_errors[j][1]
        end
    end
    
    -- Calculate hidden layer gradients
    local hidden_gradients = {}
    for i = 1, self.hidden_nodes do
        hidden_gradients[i] = {hidden_errors[i][1] * sigmoid_derivative(hidden[i][1])}
    end
    
    -- Update weights and biases (output to hidden)
    for i = 1, self.output_nodes do
        for j = 1, self.hidden_nodes do
            self.weights_ho[i][j] = self.weights_ho[i][j] + 
                learning_rate * output_gradients[i][1] * hidden[j][1]
        end
        self.bias_o[i] = self.bias_o[i] + learning_rate * output_gradients[i][1]
    end
    
    -- Update weights and biases (input to hidden)
    for i = 1, self.hidden_nodes do
        for j = 1, self.input_nodes do
            self.weights_ih[i][j] = self.weights_ih[i][j] + 
                learning_rate * hidden_gradients[i][1] * input_array[j]
        end
        self.bias_h[i] = self.bias_h[i] + learning_rate * hidden_gradients[i][1]
    end
end

-- Example usage
print("Training Neural Network with Backpropagation")

-- Create a neural network: 2 inputs, 4 hidden, 1 output
local nn = NeuralNetwork:new(2, 4, 1)

-- Training data (XOR problem)
local training_data = {
    {0, 0, 0},
    {0, 1, 1},
    {1, 0, 1},
    {1, 1, 0}
}

-- Train the network
local epochs = 10000
local learning_rate = 1.0

print("Training...")
for epoch = 1, epochs do
    for i = 1, #training_data do
        local input = {training_data[i][1], training_data[i][2]}
        local target = {training_data[i][3]}
        nn:train(input, target, learning_rate)
    end
    
    -- Print progress
    if epoch % 2000 == 0 then
        print("Epoch: " .. epoch)
    end
end

-- Test the network
print("\nTesting the trained network:")
print("Input -> Expected -> Actual")

for i = 1, #training_data do
    local input = {training_data[i][1], training_data[i][2]}
    local outputs, _ = nn:forward(input)
    print(input[1] .. " " .. input[2] .. " -> " .. training_data[i][3] .. " -> " .. string.format("%.4f", outputs[1][1]))
end
```

## Key Components Explained

### 1. **Activation Functions**
- `sigmoid()`: Non-linear activation function
- `sigmoid_derivative()`: Derivative used in backpropagation

### 2. **Forward Propagation**
- Processes input through the network
- Applies weights and biases
- Uses sigmoid activation function

### 3. **Backpropagation**
- Calculates errors at each layer
- Computes gradients using chain rule
- Updates weights and biases using gradient descent

### 4. **Training Process**
- Iterates through training data multiple times
- Adjusts weights based on prediction errors
- Uses learning rate to control update magnitude

## Usage Example

This implementation demonstrates training a neural network to solve the XOR problem, which is a classic example where simple linear models fail but neural networks with backpropagation succeed.

The network learns to approximate the XOR function by adjusting its weights and biases through the backpropagation algorithm during training.

