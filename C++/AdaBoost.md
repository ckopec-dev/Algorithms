# AdaBoost Algorithm Implementation in C++

Here's a complete implementation of the AdaBoost algorithm for binary classification:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <random>

// Structure to represent a data point
struct DataPoint {
    std::vector<double> features;
    int label; // -1 or 1
    
    DataPoint(const std::vector<double>& f, int l) : features(f), label(l) {}
};

// Weak learner (decision stump)
class DecisionStump {
public:
    int feature_index;
    double threshold;
    int polarity; // 1 or -1
    
    DecisionStump() : feature_index(0), threshold(0.0), polarity(1) {}
    
    // Make prediction for a single data point
    int predict(const std::vector<double>& features) const {
        double value = features[feature_index];
        if (value < threshold) {
            return polarity;
        } else {
            return -polarity;
        }
    }
    
    // Calculate error for this weak learner
    double calculate_error(const std::vector<DataPoint>& data, 
                          const std::vector<double>& weights) const {
        double error = 0.0;
        for (size_t i = 0; i < data.size(); ++i) {
            int prediction = predict(data[i].features);
            if (prediction != data[i].label) {
                error += weights[i];
            }
        }
        return error;
    }
};

// AdaBoost Classifier
class AdaBoost {
private:
    std::vector<DecisionStump> weak_learners;
    std::vector<double> learner_weights;
    int max_iterations;
    
public:
    AdaBoost(int max_iter = 10) : max_iterations(max_iter) {}
    
    // Train the AdaBoost model
    void fit(const std::vector<DataPoint>& data) {
        size_t n_samples = data.size();
        size_t n_features = data[0].features.size();
        
        // Initialize weights uniformly
        std::vector<double> weights(n_samples, 1.0 / n_samples);
        
        weak_learners.clear();
        learner_weights.clear();
        
        for (int iteration = 0; iteration < max_iterations; ++iteration) {
            // Find best weak learner
            DecisionStump best_stump;
            double best_error = 1.0;
            
            // Try all features and thresholds
            for (int feature_idx = 0; feature_idx < n_features; ++feature_idx) {
                std::vector<double> feature_values;
                for (const auto& point : data) {
                    feature_values.push_back(point.features[feature_idx]);
                }
                
                // Sort values to find good thresholds
                std::sort(feature_values.begin(), feature_values.end());
                
                // Try different thresholds
                for (size_t i = 0; i < feature_values.size() - 1; ++i) {
                    double threshold = (feature_values[i] + feature_values[i+1]) / 2.0;
                    
                    // Try both polarities
                    for (int polarity : {1, -1}) {
                        DecisionStump stump;
                        stump.feature_index = feature_idx;
                        stump.threshold = threshold;
                        stump.polarity = polarity;
                        
                        double error = stump.calculate_error(data, weights);
                        
                        if (error < best_error) {
                            best_error = error;
                            best_stump = stump;
                        }
                    }
                }
            }
            
            // Calculate alpha (learner weight)
            const double epsilon = 1e-10; // Small value to avoid division by zero
            double alpha = 0.5 * log((1.0 - best_error) / (best_error + epsilon));
            
            // Update weights
            for (size_t i = 0; i < n_samples; ++i) {
                int prediction = best_stump.predict(data[i].features);
                double weight_update = alpha * data[i].label * prediction;
                weights[i] *= exp(-weight_update);
            }
            
            // Normalize weights
            double sum_weights = 0.0;
            for (double w : weights) {
                sum_weights += w;
            }
            for (double& w : weights) {
                w /= sum_weights;
            }
            
            weak_learners.push_back(best_stump);
            learner_weights.push_back(alpha);
        }
    }
    
    // Make prediction for a single sample
    int predict(const std::vector<double>& features) const {
        double score = 0.0;
        for (size_t i = 0; i < weak_learners.size(); ++i) {
            int prediction = weak_learners[i].predict(features);
            score += learner_weights[i] * prediction;
        }
        
        return (score >= 0) ? 1 : -1;
    }
    
    // Make predictions for multiple samples
    std::vector<int> predict(const std::vector<std::vector<double>>& features_list) const {
        std::vector<int> predictions;
        for (const auto& features : features_list) {
            predictions.push_back(predict(features));
        }
        return predictions;
    }
    
    // Get the number of weak learners
    size_t get_num_learners() const {
        return weak_learners.size();
    }
};

// Example usage
int main() {
    // Create sample dataset (simple 2D classification)
    std::vector<DataPoint> training_data = {
        DataPoint({1.0, 2.0}, -1),
        DataPoint({2.0, 3.0}, -1),
        DataPoint({3.0, 1.0}, -1),
        DataPoint({4.0, 2.0}, 1),
        DataPoint({5.0, 3.0}, 1),
        DataPoint({6.0, 1.0}, 1),
        DataPoint({7.0, 2.0}, 1),
        DataPoint({8.0, 3.0}, 1)
    };
    
    // Create and train AdaBoost classifier
    AdaBoost boost(5); // 5 weak learners
    boost.fit(training_data);
    
    // Test predictions
    std::vector<std::vector<double>> test_points = {
        {1.5, 2.5},
        {4.5, 2.5},
        {7.5, 2.5}
    };
    
    std::cout << "AdaBoost Predictions:\n";
    auto predictions = boost.predict(test_points);
    
    for (size_t i = 0; i < test_points.size(); ++i) {
        std::cout << "Point (" << test_points[i][0] << ", " << test_points[i][1] 
                  << ") -> Prediction: " << predictions[i] << std::endl;
    }
    
    std::cout << "\nNumber of weak learners used: " << boost.get_num_learners() << std::endl;
    
    return 0;
}
```

## Key Components Explained:

### 1. **DataPoint Structure**
- Stores features and labels for each training sample
- Features are stored as a vector of doubles
- Labels are binary (-1 or 1)

### 2. **DecisionStump (Weak Learner)**
- Simple decision tree with one split
- Uses only one feature and threshold
- Can have two polarities (left vs right splits)

### 3. **AdaBoost Class**
- **Training**: Iteratively finds best weak learners
- **Weight Update**: Adjusts sample weights based on previous learner's performance
- **Alpha Calculation**: Computes learner weight based on its error rate
- **Prediction**: Combines all weak learners with their weights

### 4. **Algorithm Steps**
1. Initialize uniform weights for all training samples
2. For each iteration:
   - Find the best weak learner (decision stump)
   - Calculate its weighted error
   - Compute learner weight (alpha)
   - Update sample weights
   - Normalize weights
3. Combine all weak learners for final prediction

## Sample Output:
```
AdaBoost Predictions:
Point (1.5, 2.5) -> Prediction: -1
Point (4.5, 2.5) -> Prediction: 1
Point (7.5, 2.5) -> Prediction: 1

Number of weak learners used: 5
```

This implementation demonstrates the core concepts of AdaBoost while being practical for educational purposes. The algorithm can be extended with additional features like different weak learner types or more sophisticated optimization techniques.

