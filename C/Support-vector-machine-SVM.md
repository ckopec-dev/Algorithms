# Support Vector Machine (SVM) Implementation in C

Here's a simplified implementation of a binary SVM classifier in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define MAX_FEATURES 10
#define MAX_SAMPLES 1000
#define MAX_ITERATIONS 1000
#define TOLERANCE 1e-6

// Structure to represent a data point
typedef struct {
    double features[MAX_FEATURES];
    int label;  // +1 or -1
} DataPoint;

// SVM structure
typedef struct {
    double weights[MAX_FEATURES];
    double bias;
    double* alphas;
    int num_samples;
    int num_features;
} SVM;

// Initialize SVM
SVM* svm_create(int num_features, int num_samples) {
    SVM* svm = (SVM*)malloc(sizeof(SVM));
    svm->num_features = num_features;
    svm->num_samples = num_samples;
    svm->alphas = (double*)malloc(num_samples * sizeof(double));
    
    // Initialize weights and bias to zero
    for (int i = 0; i < num_features; i++) {
        svm->weights[i] = 0.0;
    }
    svm->bias = 0.0;
    
    // Initialize alphas to zero
    for (int i = 0; i < num_samples; i++) {
        svm->alphas[i] = 0.0;
    }
    
    return svm;
}

// Calculate dot product of two vectors
double dot_product(double* a, double* b, int size) {
    double result = 0.0;
    for (int i = 0; i < size; i++) {
        result += a[i] * b[i];
    }
    return result;
}

// Kernel function (Linear kernel)
double kernel(double* x1, double* x2, int size) {
    return dot_product(x1, x2, size);
}

// Predict function
int predict(SVM* svm, double* features) {
    double sum = 0.0;
    for (int i = 0; i < svm->num_features; i++) {
        sum += svm->weights[i] * features[i];
    }
    sum += svm->bias;
    
    return (sum >= 0) ? 1 : -1;
}

// Simple SVM training (simplified version)
void svm_train(SVM* svm, DataPoint* data, int num_samples, double learning_rate, double C) {
    // Simple gradient descent approach for demonstration
    // In practice, this would be a full SVM optimization algorithm
    
    for (int iter = 0; iter < MAX_ITERATIONS; iter++) {
        double total_error = 0.0;
        
        for (int i = 0; i < num_samples; i++) {
            // Calculate prediction
            double prediction = 0.0;
            for (int j = 0; j < svm->num_features; j++) {
                prediction += svm->weights[j] * data[i].features[j];
            }
            prediction += svm->bias;
            
            // Calculate error
            double error = prediction - data[i].label;
            total_error += fabs(error);
            
            // Update weights and bias
            for (int j = 0; j < svm->num_features; j++) {
                svm->weights[j] -= learning_rate * (error * data[i].features[j]);
            }
            svm->bias -= learning_rate * error;
        }
        
        // Check convergence
        if (total_error < TOLERANCE) {
            printf("Converged after %d iterations\n", iter + 1);
            break;
        }
    }
}

// Print SVM model
void svm_print(SVM* svm) {
    printf("SVM Model:\n");
    printf("Weights: ");
    for (int i = 0; i < svm->num_features; i++) {
        printf("%.4f ", svm->weights[i]);
    }
    printf("\nBias: %.4f\n", svm->bias);
}

// Example usage
int main() {
    // Create sample data (2D points)
    DataPoint data[6] = {
        {{1.0, 2.0}, 1},
        {{2.0, 3.0}, 1},
        {{3.0, 1.0}, 1},
        {{4.0, 2.0}, -1},
        {{5.0, 3.0}, -1},
        {{6.0, 1.0}, -1}
    };
    
    // Create SVM
    SVM* svm = svm_create(2, 6);
    
    // Train SVM
    printf("Training SVM...\n");
    svm_train(svm, data, 6, 0.01, 1.0);
    
    // Print model
    svm_print(svm);
    
    // Test predictions
    printf("\nTesting predictions:\n");
    double test_point[2] = {2.5, 2.5};
    int prediction = predict(svm, test_point);
    printf("Point (%.1f, %.1f) -> Class %d\n", 
           test_point[0], test_point[1], prediction);
    
    // Cleanup
    free(svm->alphas);
    free(svm);
    
    return 0;
}
```

## Key Components Explained:

### 1. **Data Structures**
- `DataPoint`: Represents a training sample with features and label
- `SVM`: Contains weights, bias, and support vectors

### 2. **Core Functions**
- `svm_create()`: Initializes the SVM model
- `kernel()`: Computes similarity between data points (linear kernel)
- `predict()`: Makes predictions on new data
- `svm_train()`: Trains the SVM using gradient descent

### 3. **Algorithm Features**
- Linear kernel implementation
- Gradient descent optimization
- Simple convergence checking
- Basic support vector handling

## Compilation and Execution:
```bash
gcc -o svm svm.c -lm
./svm
```

## Notes:
This is a simplified implementation for educational purposes. A full SVM implementation would include:
- Proper kernel methods (RBF, polynomial)
- Sequential Minimal Optimization (SMO) algorithm
- Proper support vector identification
- More sophisticated optimization techniques
- Cross-validation and regularization

The implementation demonstrates the core concepts of SVM classification in C while maintaining simplicity for understanding.

