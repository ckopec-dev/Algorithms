# Backpropagation Algorithm in Forth

Here's an implementation of a simple backpropagation algorithm for a neural network in Forth:

```forth
\ Neural Network Backpropagation Implementation in Forth

\ Constants
1000 constant MAX_ITERATIONS
1e-3 constant LEARNING_RATE
256 constant MAX_NEURONS
256 constant MAX_LAYERS

\ Network structure
create weights MAX_LAYERS MAX_NEURONS MAX_NEURONS * * floats allot
create biases MAX_LAYERS MAX_NEURONS * floats allot
create activations MAX_LAYERS MAX_NEURONS * floats allot
create errors MAX_LAYERS MAX_NEURONS * floats allot

\ Sigmoid activation function
: sigmoid ( x -- y )
    1e0 swap exp 1e0 + 1e0 swap / ;

\ Derivative of sigmoid
: sigmoid-derivative ( x -- y )
    dup 1e0 swap - * ;

\ Initialize network weights and biases
: init-network ( layers neurons -- )
    0 do
        0 do
            \ Initialize weights with small random values
            0.1e0 rand 0.2e0 - 0.1e0 + 0.0e0 weights i j * + +
            \ Initialize biases to zero
            0.0e0 biases i j * + +
        loop
    loop ;

\ Forward propagation
: forward-prop ( input -- output )
    \ Copy input to first layer activations
    0 do
        input i + @ activations i + !
    loop
    
    \ Propagate through layers
    1 do
        0 do
            0.0e0
            0 j do
                weights i j * + @
                activations j + @ *
                +
            loop
            biases i j * + @ +
            sigmoid
            activations i j * + !
        loop
    loop ;

\ Backward propagation
: backward-prop ( target -- )
    \ Calculate output layer errors
    layers 1- do
        0 j do
            \ Error = (target - output) * derivative(output)
            activations layers 1- j * + @
            target j + @
            -
            activations layers 1- j * + @
            sigmoid-derivative
            *
            errors layers 1- j * + !
        loop
    loop
    
    \ Calculate errors for hidden layers
    layers 2- do
        0 j do
            0.0e0
            0 k do
                weights i 1+ k j * + @
                errors i 1+ k * + @ *
                +
            loop
            errors i j * + !
        loop
    loop ;

\ Update weights and biases
: update-weights ( -- )
    0 do
        0 j do
            \ Update weights
            0 k do
                weights i j * + @
                LEARNING_RATE
                errors i k * + @
                activations j + @ *
                *
                +
                weights i j * + !
            loop
            
            \ Update biases
            biases i j * + @
            LEARNING_RATE
            errors i j * + @ *
            +
            biases i j * + !
        loop
    loop ;

\ Training function
: train-network ( input target iterations -- )
    0 do
        forward-prop
        backward-prop
        update-weights
    loop ;

\ Example usage
: example-training ( -- )
    \ Define network structure: 2 inputs, 3 hidden neurons, 1 output
    3 layers 3 neurons init-network
    
    \ Training data (simple XOR problem)
    create training-inputs 2 floats allot
    create training-targets 1 floats allot
    
    \ Input: [0 0] -> Target: [0]
    0.0e0 training-inputs 0 + !
    0.0e0 training-inputs 1 + !
    0.0e0 training-targets 0 + !
    
    \ Train for 1000 iterations
    training-inputs training-targets MAX_ITERATIONS train-network ;
```

## Key Components Explained:

### 1. **Data Structures**
- `weights`: 3D array storing connection weights between neurons
- `biases`: Array storing bias values for each neuron
- `activations`: Stores neuron activation values
- `errors`: Stores error terms for backpropagation

### 2. **Core Functions**

**Sigmoid Activation**: 
```forth
: sigmoid ( x -- y )
    1e0 swap exp 1e0 + 1e0 swap / ;
```

**Forward Propagation**: 
Processes input through the network layer by layer

**Backward Propagation**: 
Calculates error gradients using chain rule

**Weight Updates**: 
Adjusts weights and biases using gradient descent

### 3. **Training Process**
The algorithm iteratively:
1. Performs forward propagation
2. Calculates errors using backward propagation  
3. Updates network parameters
4. Repeats until convergence

This implementation demonstrates the fundamental backpropagation algorithm in Forth, showing how neural networks learn through gradient-based optimization.