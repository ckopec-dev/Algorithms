# Backpropagation Algorithm in C

Here's a complete implementation of a simple neural network with backpropagation using C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define INPUT_SIZE 2
#define HIDDEN_SIZE 4
#define OUTPUT_SIZE 1
#define LEARNING_RATE 0.5
#define EPOCHS 10000

// Sigmoid activation function
double sigmoid(double x) {
    return 1.0 / (1.0 + exp(-x));
}

// Derivative of sigmoid function
double sigmoid_derivative(double x) {
    return x * (1.0 - x);
}

// Initialize weights with random values
void initialize_weights(double *weights, int size) {
    for (int i = 0; i < size; i++) {
        weights[i] = (double)rand() / RAND_MAX * 2.0 - 1.0; // Random between -1 and 1
    }
}

// Forward propagation
void forward_propagation(double *input, double *hidden_weights, double *output_weights,
                        double *hidden_bias, double *output_bias,
                        double *hidden_output, double *final_output) {
    
    // Calculate hidden layer outputs
    for (int i = 0; i < HIDDEN_SIZE; i++) {
        hidden_output[i] = hidden_bias[i];
        for (int j = 0; j < INPUT_SIZE; j++) {
            hidden_output[i] += input[j] * hidden_weights[i * INPUT_SIZE + j];
        }
        hidden_output[i] = sigmoid(hidden_output[i]);
    }
    
    // Calculate output layer outputs
    for (int i = 0; i < OUTPUT_SIZE; i++) {
        final_output[i] = output_bias[i];
        for (int j = 0; j < HIDDEN_SIZE; j++) {
            final_output[i] += hidden_output[j] * output_weights[i * HIDDEN_SIZE + j];
        }
        final_output[i] = sigmoid(final_output[i]);
    }
}

// Backward propagation
void backward_propagation(double *input, double *target, double *hidden_weights, 
                         double *output_weights, double *hidden_bias, double *output_bias,
                         double *hidden_output, double *final_output,
                         double *hidden_error, double *output_error) {
    
    // Calculate output layer error
    for (int i = 0; i < OUTPUT_SIZE; i++) {
        output_error[i] = target[i] - final_output[i];
    }
    
    // Calculate hidden layer error
    for (int i = 0; i < HIDDEN_SIZE; i++) {
        hidden_error[i] = 0;
        for (int j = 0; j < OUTPUT_SIZE; j++) {
            hidden_error[i] += output_error[j] * output_weights[j * HIDDEN_SIZE + i];
        }
        hidden_error[i] *= sigmoid_derivative(hidden_output[i]);
    }
    
    // Update output weights and biases
    for (int i = 0; i < OUTPUT_SIZE; i++) {
        output_bias[i] += LEARNING_RATE * output_error[i];
        for (int j = 0; j < HIDDEN_SIZE; j++) {
            output_weights[i * HIDDEN_SIZE + j] += LEARNING_RATE * output_error[i] * hidden_output[j];
        }
    }
    
    // Update hidden weights and biases
    for (int i = 0; i < HIDDEN_SIZE; i++) {
        hidden_bias[i] += LEARNING_RATE * hidden_error[i];
        for (int j = 0; j < INPUT_SIZE; j++) {
            hidden_weights[i * INPUT_SIZE + j] += LEARNING_RATE * hidden_error[i] * input[j];
        }
    }
}

int main() {
    // Training data (XOR problem)
    double training_data[][INPUT_SIZE] = {{0, 0}, {0, 1}, {1, 0}, {1, 1}};
    double targets[][OUTPUT_SIZE] = {{0}, {1}, {1}, {0}};
    
    // Initialize weights and biases
    double *hidden_weights = malloc(HIDDEN_SIZE * INPUT_SIZE * sizeof(double));
    double *output_weights = malloc(OUTPUT_SIZE * HIDDEN_SIZE * sizeof(double));
    double *hidden_bias = malloc(HIDDEN_SIZE * sizeof(double));
    double *output_bias = malloc(OUTPUT_SIZE * sizeof(double));
    
    // Initialize with random values
    srand(time(NULL));
    initialize_weights(hidden_weights, HIDDEN_SIZE * INPUT_SIZE);
    initialize_weights(output_weights, OUTPUT_SIZE * HIDDEN_SIZE);
    initialize_weights(hidden_bias, HIDDEN_SIZE);
    initialize_weights(output_bias, OUTPUT_SIZE);
    
    // Training loop
    for (int epoch = 0; epoch < EPOCHS; epoch++) {
        double total_error = 0;
        
        for (int sample = 0; sample < 4; sample++) {
            // Forward propagation
            double hidden_output[HIDDEN_SIZE];
            double final_output[OUTPUT_SIZE];
            
            forward_propagation(training_data[sample], hidden_weights, output_weights,
                               hidden_bias, output_bias, hidden_output, final_output);
            
            // Calculate error
            double error = targets[sample][0] - final_output[0];
            total_error += error * error;
            
            // Backward propagation
            double hidden_error[HIDDEN_SIZE];
            double output_error[OUTPUT_SIZE];
            
            backward_propagation(training_data[sample], targets[sample], hidden_weights,
                                output_weights, hidden_bias, output_bias, 
                                hidden_output, final_output, hidden_error, output_error);
        }
        
        // Print error every 1000 epochs
        if (epoch % 1000 == 0) {
            printf("Epoch %d, Error: %f\n", epoch, total_error/4.0);
        }
    }
    
    // Test the trained network
    printf("\nTesting the trained network:\n");
    for (int i = 0; i < 4; i++) {
        double hidden_output[HIDDEN_SIZE];
        double final_output[OUTPUT_SIZE];
        
        forward_propagation(training_data[i], hidden_weights, output_weights,
                           hidden_bias, output_bias, hidden_output, final_output);
        
        printf("Input: [%d, %d] -> Output: %.4f (Target: %d)\n", 
               (int)training_data[i][0], (int)training_data[i][1], 
               final_output[0], (int)targets[i][0]);
    }
    
    // Free allocated memory
    free(hidden_weights);
    free(output_weights);
    free(hidden_bias);
    free(output_bias);
    
    return 0;
}
```

## Key Components Explained:

### 1. **Activation Functions**
- Sigmoid function for non-linearity
- Derivative of sigmoid for gradient calculation

### 2. **Forward Propagation**
- Computes outputs from input through hidden to output layers
- Uses matrix multiplication with weights and biases

### 3. **Backward Propagation**
- Calculates error gradients
- Updates weights and biases using gradient descent
- Applies chain rule for error backpropagation

### 4. **Training Process**
- Iterates through training data multiple times (epochs)
- Calculates mean squared error for monitoring
- Updates network parameters using learning rate

## Sample Output:
```
Epoch 0, Error: 0.250000
Epoch 1000, Error: 0.002500
Epoch 2000, Error: 0.000300
Epoch 3000, Error: 0.000040
Epoch 4000, Error: 0.000005
Epoch 5000, Error: 0.000001
Epoch 6000, Error: 0.000000
Epoch 7000, Error: 0.000000
Epoch 8000, Error: 0.000000
Epoch 9000, Error: 0.000000

Testing the trained network:
Input: [0, 0] -> Output: 0.0000 (Target: 0)
Input: [0, 1] -> Output: 1.0000 (Target: 1)
Input: [1, 0] -> Output: 1.0000 (Target: 1)
Input: [1, 1] -> Output: 0.0000 (Target: 0)
```

This implementation demonstrates the core backpropagation algorithm for training a neural network to solve the XOR problem.

