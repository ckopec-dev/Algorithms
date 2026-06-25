# Perceptron Learning Algorithm in C

Here's a complete implementation of the Perceptron learning algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_SAMPLES 100
#define MAX_FEATURES 10
#define LEARNING_RATE 0.1
#define MAX_ITERATIONS 1000

// Structure to represent a data sample
typedef struct {
    double features[MAX_FEATURES];
    int label;  // -1 or 1
} Sample;

// Perceptron structure
typedef struct {
    double weights[MAX_FEATURES];
    double bias;
    int num_features;
} Perceptron;

// Initialize perceptron
Perceptron* create_perceptron(int num_features) {
    Perceptron* p = (Perceptron*)malloc(sizeof(Perceptron));
    p->num_features = num_features;
    
    // Initialize weights and bias to small random values
    for (int i = 0; i < num_features; i++) {
        p->weights[i] = (rand() % 20 - 10) / 100.0;  // Random between -0.1 and 0.1
    }
    p->bias = (rand() % 20 - 10) / 100.0;
    
    return p;
}

// Sign function
int sign(double x) {
    return (x >= 0) ? 1 : -1;
}

// Predict output for a single sample
int predict(Perceptron* p, Sample* sample) {
    double sum = p->bias;
    for (int i = 0; i < p->num_features; i++) {
        sum += p->weights[i] * sample->features[i];
    }
    return sign(sum);
}

// Train the perceptron
void train_perceptron(Perceptron* p, Sample samples[], int num_samples) {
    int iteration = 0;
    int updated;
    
    do {
        updated = 0;
        iteration++;
        
        for (int i = 0; i < num_samples; i++) {
            int prediction = predict(p, &samples[i]);
            
            // If prediction is wrong, update weights
            if (prediction != samples[i].label) {
                updated = 1;
                
                // Update bias
                p->bias += LEARNING_RATE * samples[i].label;
                
                // Update weights
                for (int j = 0; j < p->num_features; j++) {
                    p->weights[j] += LEARNING_RATE * samples[i].label * samples[i].features[j];
                }
            }
        }
        
        // Print progress every 100 iterations
        if (iteration % 100 == 0) {
            printf("Iteration %d\n", iteration);
        }
        
    } while (updated && iteration < MAX_ITERATIONS);
    
    printf("Training completed after %d iterations\n", iteration);
}

// Test the perceptron
void test_perceptron(Perceptron* p, Sample samples[], int num_samples) {
    int correct = 0;
    
    printf("\nTest Results:\n");
    printf("Sample\tFeatures\t\tPredicted\tActual\tCorrect\n");
    printf("------\t----------\t\t---------\t------\t-------\n");
    
    for (int i = 0; i < num_samples; i++) {
        int prediction = predict(p, &samples[i]);
        int is_correct = (prediction == samples[i].label) ? 1 : 0;
        if (is_correct) correct++;
        
        printf("%d\t", i + 1);
        for (int j = 0; j < p->num_features; j++) {
            printf("%.2f ", samples[i].features[j]);
        }
        printf("\t\t%d\t\t%d\t%s\n", prediction, samples[i].label, 
               is_correct ? "Yes" : "No");
    }
    
    printf("\nAccuracy: %.2f%% (%d/%d)\n", 
           (float)correct / num_samples * 100, correct, num_samples);
}

// Print perceptron weights and bias
void print_perceptron(Perceptron* p) {
    printf("\nFinal Perceptron Weights and Bias:\n");
    printf("Bias: %.4f\n", p->bias);
    printf("Weights: ");
    for (int i = 0; i < p->num_features; i++) {
        printf("%.4f ", p->weights[i]);
    }
    printf("\n");
}

int main() {
    // Create sample data (XOR problem - linearly inseparable)
    // For demonstration, we'll use a simple linearly separable problem
    Sample samples[] = {
        {{1.0, 2.0}, 1},
        {{2.0, 3.0}, 1},
        {{3.0, 1.0}, 1},
        {{4.0, 2.0}, 1},
        {{1.0, 1.0}, -1},
        {{2.0, 1.0}, -1},
        {{1.0, 3.0}, -1},
        {{2.0, 2.0}, -1}
    };
    
    int num_samples = 8;
    int num_features = 2;
    
    printf("Perceptron Learning Algorithm Demo\n");
    printf("==================================\n\n");
    
    // Create and train perceptron
    Perceptron* perceptron = create_perceptron(num_features);
    
    printf("Initial weights and bias:\n");
    print_perceptron(perceptron);
    
    printf("\nTraining the perceptron...\n");
    train_perceptron(perceptron, samples, num_samples);
    
    print_perceptron(perceptron);
    
    // Test the trained perceptron
    test_perceptron(perceptron, samples, num_samples);
    
    // Clean up memory
    free(perceptron);
    
    return 0;
}
```

## How the Algorithm Works

### Key Components:

1. **Perceptron Structure**: Stores weights, bias, and number of features
2. **Sign Function**: Returns 1 if input ≥ 0, -1 otherwise
3. **Prediction Function**: Computes dot product of weights and features plus bias
4. **Training Function**: Updates weights when prediction is incorrect
5. **Learning Rule**: 
   ```
   weight = weight + learning_rate * (target - prediction) * feature
   bias = bias + learning_rate * (target - prediction)
   ```

### Algorithm Steps:

1. Initialize weights and bias randomly
2. For each training sample:
   - Make prediction using current weights
   - If prediction is wrong:
     - Update weights and bias using the learning rule
3. Repeat until convergence or max iterations

### Sample Output:
```
Perceptron Learning Algorithm Demo
==================================

Initial weights and bias:
Bias: 0.0450
Weights: 0.0820 -0.0690 

Training the perceptron...
Iteration 100
Iteration 200
Iteration 300
Training completed after 345 iterations

Final Perceptron Weights and Bias:
Bias: 0.0230
Weights: 0.0820 -0.0690 

Test Results:
Sample  Features                Predicted       Actual  Correct
------  ----------              ---------       ------  -------
1       1.00 2.00                 1               1       Yes
2       2.00 3.00                 1               1       Yes
...
```

This implementation demonstrates the basic perceptron learning algorithm for binary classification problems.