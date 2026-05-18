# Backpropagation Algorithm in Pascal

Here's a complete implementation of a simple neural network with backpropagation in Pascal:

```pascal
program BackpropagationExample;

uses
  crt, math;

const
  MAX_LAYERS = 4;
  MAX_NEURONS = 10;
  LEARNING_RATE = 0.5;
  MAX_ITERATIONS = 10000;

type
  TMatrix = array[1..MAX_NEURONS, 1..MAX_NEURONS] of real;
  TVector = array[1..MAX_NEURONS] of real;
  TNetwork = record
    layers: array[1..MAX_LAYERS] of integer;
    weights: array[1..MAX_LAYERS-1] of TMatrix;
    biases: array[1..MAX_LAYERS-1] of TVector;
    activations: array[1..MAX_LAYERS] of TVector;
    deltas: array[1..MAX_LAYERS] of TVector;
    num_layers: integer;
  end;

var
  network: TNetwork;
  training_data: array[1..4, 1..2] of real;  // XOR training data
  expected_output: array[1..4] of real;

// Sigmoid activation function
function sigmoid(x: real): real;
begin
  sigmoid := 1.0 / (1.0 + exp(-x));
end;

// Derivative of sigmoid
function sigmoid_derivative(x: real): real;
begin
  sigmoid_derivative := x * (1.0 - x);
end;

// Initialize network
procedure InitializeNetwork;
var
  i, j, k: integer;
begin
  // Set up network architecture: 2-4-1 (2 inputs, 4 hidden, 1 output)
  network.num_layers := 3;
  network.layers[1] := 2;  // Input layer
  network.layers[2] := 4;  // Hidden layer
  network.layers[3] := 1;  // Output layer

  // Initialize weights with small random values
  for i := 1 to network.num_layers - 1 do
  begin
    for j := 1 to network.layers[i+1] do
    begin
      for k := 1 to network.layers[i] do
      begin
        network.weights[i][j][k] := (random - 0.5) * 0.5;
      end;
    end;
  end;

  // Initialize biases
  for i := 1 to network.num_layers - 1 do
  begin
    for j := 1 to network.layers[i+1] do
    begin
      network.biases[i][j] := (random - 0.5) * 0.5;
    end;
  end;
end;

// Forward propagation
procedure ForwardPropagation(input: TVector);
var
  i, j, k: integer;
  sum: real;
begin
  // Input layer
  for i := 1 to network.layers[1] do
  begin
    network.activations[1][i] := input[i];
  end;

  // Hidden and output layers
  for i := 1 to network.num_layers - 1 do
  begin
    for j := 1 to network.layers[i+1] do
    begin
      sum := network.biases[i][j];
      for k := 1 to network.layers[i] do
      begin
        sum := sum + network.weights[i][j][k] * network.activations[i][k];
      end;
      network.activations[i+1][j] := sigmoid(sum);
    end;
  end;
end;

// Backward propagation
procedure BackwardPropagation(expected: real);
var
  i, j, k: integer;
  error, delta: real;
begin
  // Calculate output layer error
  i := network.num_layers;
  for j := 1 to network.layers[i] do
  begin
    error := expected - network.activations[i][j];
    network.deltas[i][j] := error * sigmoid_derivative(network.activations[i][j]);
  end;

  // Calculate hidden layer errors (backpropagate)
  for i := network.num_layers - 1 downto 1 do
  begin
    for j := 1 to network.layers[i] do
    begin
      error := 0.0;
      for k := 1 to network.layers[i+1] do
      begin
        error := error + network.weights[i][k][j] * network.deltas[i+1][k];
      end;
      network.deltas[i][j] := error * sigmoid_derivative(network.activations[i][j]);
    end;
  end;

  // Update weights and biases
  for i := network.num_layers - 1 downto 1 do
  begin
    for j := 1 to network.layers[i+1] do
    begin
      // Update weights
      for k := 1 to network.layers[i] do
      begin
        network.weights[i][j][k] := network.weights[i][j][k] + 
          LEARNING_RATE * network.deltas[i+1][j] * network.activations[i][k];
      end;
      
      // Update biases
      network.biases[i][j] := network.biases[i][j] + 
        LEARNING_RATE * network.deltas[i+1][j];
    end;
  end;
end;

// Train the network
procedure TrainNetwork;
var
  i, j, iterations: integer;
  input: TVector;
  output: real;
  total_error: real;
begin
  writeln('Training neural network...');
  
  for iterations := 1 to MAX_ITERATIONS do
  begin
    total_error := 0;
    
    // Train on each data point
    for i := 1 to 4 do
    begin
      // Set input
      for j := 1 to 2 do
        input[j] := training_data[i][j];
      
      // Forward propagation
      ForwardPropagation(input);
      
      // Get output
      output := network.activations[network.num_layers][1];
      
      // Calculate error
      total_error := total_error + sqr(output - expected_output[i]);
      
      // Backward propagation
      BackwardPropagation(expected_output[i]);
    end;
    
    // Print progress every 1000 iterations
    if (iterations mod 1000 = 0) then
    begin
      writeln('Iteration: ', iterations, ' Error: ', total_error:0:6);
    end;
  end;
end;

// Test the network
procedure TestNetwork;
var
  i, j: integer;
  input: TVector;
  output: real;
begin
  writeln('Testing network:');
  writeln('Input -> Output (Expected)');
  writeln('--------------------------');
  
  for i := 1 to 4 do
  begin
    for j := 1 to 2 do
      input[j] := training_data[i][j];
    
    ForwardPropagation(input);
    output := network.activations[network.num_layers][1];
    
    writeln(input[1]:0:1, ' ', input[2]:0:1, ' -> ', output:0:4, ' (', expected_output[i]:0:1, ')');
  end;
end;

begin
  randomize;
  
  // XOR training data
  training_data[1,1] := 0; training_data[1,2] := 0; expected_output[1] := 0;
  training_data[2,1] := 0; training_data[2,2] := 1; expected_output[2] := 1;
  training_data[3,1] := 1; training_data[3,2] := 0; expected_output[3] := 1;
  training_data[4,1] := 1; training_data[4,2] := 1; expected_output[4] := 0;
  
  // Initialize and train network
  InitializeNetwork;
  TrainNetwork;
  
  // Test the trained network
  TestNetwork;
  
  writeln('');
  writeln('Press any key to exit...');
  readln;
end.
```

## Key Components Explained

### 1. **Data Structures**
- `TMatrix`: 2D array for weights
- `TVector`: 1D array for activations and biases
- `TNetwork`: Complete network structure with layers, weights, biases, and activations

### 2. **Core Functions**
- **Sigmoid**: Activation function for neurons
- **ForwardPropagation**: Computes outputs through the network
- **BackwardPropagation**: Updates weights and biases using gradient descent
- **TrainNetwork**: Main training loop with multiple iterations

### 3. **Algorithm Steps**
1. Initialize network with random weights
2. For each training iteration:
   - Forward propagate inputs
   - Calculate errors
   - Backpropagate errors to update weights
3. Test the trained network

### 4. **Training Data**
Uses XOR problem (classic neural network test case):
```
0 0 -> 0
0 1 -> 1
1 0 -> 1
1 1 -> 0
```

This implementation demonstrates the fundamental backpropagation algorithm in Pascal, showing how neural networks learn through gradient descent and error backpropagation.

