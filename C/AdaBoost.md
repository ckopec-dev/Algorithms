# AdaBoost Algorithm Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX_ITERATIONS 100
#define TOLERANCE 1e-6

// Structure to represent a data point
typedef struct {
    double features[2];  // Simple 2D features for demonstration
    int label;          // -1 or +1
} DataPoint;

// Structure for weak learner (decision stump)
typedef struct {
    int feature_index;
    double threshold;
    int polarity;  // +1 or -1
} WeakLearner;

// Function to calculate weighted error of a weak learner
double calculate_error(DataPoint* data, int* weights, int num_points, 
                       WeakLearner* learner) {
    double error = 0.0;
    
    for (int i = 0; i < num_points; i++) {
        double prediction = 0.0;
        
        // Decision stump: if feature > threshold, predict polarity, else predict -polarity
        if (data[i].features[learner->feature_index] > learner->threshold) {
            prediction = learner->polarity;
        } else {
            prediction = -learner->polarity;
        }
        
        // If prediction is wrong, add weight to error
        if (prediction != data[i].label) {
            error += weights[i];
        }
    }
    
    return error;
}

// Function to train a weak learner (decision stump)
WeakLearner* train_weak_learner(DataPoint* data, int* weights, int num_points) {
    WeakLearner* best_learner = (WeakLearner*)malloc(sizeof(WeakLearner));
    double best_error = 1.0;
    
    // Try different thresholds for each feature
    for (int feat = 0; feat < 2; feat++) {
        // Find min and max values for this feature
        double min_val = data[0].features[feat];
        double max_val = data[0].features[feat];
        
        for (int i = 1; i < num_points; i++) {
            if (data[i].features[feat] < min_val) min_val = data[i].features[feat];
            if (data[i].features[feat] > max_val) max_val = data[i].features[feat];
        }
        
        // Try different thresholds
        for (int t = 0; t <= 10; t++) {
            double threshold = min_val + (max_val - min_val) * t / 10.0;
            
            // Try both polarities
            for (int polarity = -1; polarity <= 1; polarity += 2) {
                WeakLearner temp_learner = {feat, threshold, polarity};
                double error = calculate_error(data, weights, num_points, &temp_learner);
                
                if (error < best_error) {
                    best_error = error;
                    best_learner->feature_index = feat;
                    best_learner->threshold = threshold;
                    best_learner->polarity = polarity;
                }
            }
        }
    }
    
    return best_learner;
}

// Main AdaBoost implementation
void adaboost_train(DataPoint* data, int num_points, int* final_weights, 
                   WeakLearner** weak_learners, double* alphas, int max_iterations) {
    
    // Initialize weights uniformly
    for (int i = 0; i < num_points; i++) {
        final_weights[i] = 1;
    }
    
    // Store weak learners and their weights
    for (int iteration = 0; iteration < max_iterations; iteration++) {
        // Train weak learner
        WeakLearner* learner = train_weak_learner(data, final_weights, num_points);
        
        // Calculate error of this weak learner
        double error = calculate_error(data, final_weights, num_points, learner);
        
        // Stop if error is too high or we're overfitting
        if (error >= 0.5) {
            printf("Stopping at iteration %d due to high error\n", iteration);
            break;
        }
        
        // Calculate alpha (weight of this weak learner)
        double alpha = 0.5 * log((1.0 - error) / (error + TOLERANCE));
        alphas[iteration] = alpha;
        weak_learners[iteration] = learner;
        
        printf("Iteration %d: Error = %.4f, Alpha = %.4f\n", 
               iteration, error, alpha);
        
        // Update weights
        double Z = 0.0;
        for (int i = 0; i < num_points; i++) {
            double prediction = 0.0;
            
            if (data[i].features[learner->feature_index] > learner->threshold) {
                prediction = learner->polarity;
            } else {
                prediction = -learner->polarity;
            }
            
            double weight_change = exp(-alpha * data[i].label * prediction);
            final_weights[i] *= weight_change;
            Z += final_weights[i];
        }
        
        // Normalize weights
        for (int i = 0; i < num_points; i++) {
            final_weights[i] /= Z;
        }
    }
}

// Function to make predictions using AdaBoost
int predict(DataPoint* data_point, WeakLearner** weak_learners, 
           double* alphas, int num_learners) {
    double sum = 0.0;
    
    for (int i = 0; i < num_learners; i++) {
        if (weak_learners[i] == NULL) break;
        
        double prediction = 0.0;
        if (data_point->features[weak_learners[i]->feature_index] > weak_learners[i]->threshold) {
            prediction = weak_learners[i]->polarity;
        } else {
            prediction = -weak_learners[i]->polarity;
        }
        
        sum += alphas[i] * prediction;
    }
    
    return (sum >= 0) ? 1 : -1;
}

// Example usage
int main() {
    // Sample training data (2D points with labels)
    DataPoint data[] = {
        {{1.0, 2.0}, 1},
        {{2.0, 3.0}, 1},
        {{3.0, 1.0}, -1},
        {{4.0, 2.0}, -1},
        {{5.0, 3.0}, 1},
        {{6.0, 1.0}, -1}
    };
    
    int num_points = 6;
    int* weights = (int*)malloc(num_points * sizeof(int));
    WeakLearner** learners = (WeakLearner**)malloc(MAX_ITERATIONS * sizeof(WeakLearner*));
    double* alphas = (double*)malloc(MAX_ITERATIONS * sizeof(double));
    
    // Train AdaBoost
    printf("Training AdaBoost algorithm...\n");
    adaboost_train(data, num_points, weights, learners, alphas, MAX_ITERATIONS);
    
    // Test predictions
    printf("\nTesting predictions:\n");
    DataPoint test_point = {{2.5, 2.5}, 0};
    int prediction = predict(&test_point, learners, alphas, MAX_ITERATIONS);
    printf("Test point (%.1f, %.1f) -> Prediction: %d\n", 
           test_point.features[0], test_point.features[1], prediction);
    
    // Clean up
    for (int i = 0; i < MAX_ITERATIONS; i++) {
        if (learners[i] != NULL) {
            free(learners[i]);
        }
    }
    free(weights);
    free(learners);
    free(alphas);
    
    return 0;
}
```

## Key Components of this AdaBoost Implementation:

### 1. **Data Structures**
- `DataPoint`: Represents training examples with features and labels
- `WeakLearner`: Decision stumps (simple decision rules)

### 2. **Core Functions**
- `calculate_error()`: Computes weighted error of a weak learner
- `train_weak_learner()`: Finds the best decision stump
- `adaboost_train()`: Main AdaBoost training loop
- `predict()`: Makes predictions using the trained ensemble

### 3. **Algorithm Steps**
1. Initialize uniform weights for all training examples
2. For each iteration:
   - Train a weak learner (decision stump)
   - Calculate its weighted error
   - Compute learner's weight (alpha)
   - Update example weights based on prediction accuracy
   - Normalize weights
3. Combine all weak learners into a strong classifier

### 4. **Key Features**
- Uses decision stumps as weak learners
- Implements proper weight updating mechanism
- Handles both positive and negative labels
- Includes convergence checking

This implementation demonstrates the fundamental principles of AdaBoost in C, showing how weak learners are combined to create a strong classifier through iterative boosting.

