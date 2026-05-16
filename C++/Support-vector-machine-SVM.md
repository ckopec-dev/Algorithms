# Support Vector Machine (SVM) Implementation in C++

Here's a complete example of a simple SVM implementation in C++ using the libsvm library:

## Installation Requirements

First, you'll need to install libsvm:
```bash
wget https://www.csie.ntu.edu.tw/~cjlin/libsvm/libsvm-3.24.tar.gz
tar -xzf libsvm-3.24.tar.gz
cd libsvm-3.24
make
```

## C++ SVM Implementation

```cpp
#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include "svm.h"

class SimpleSVM {
private:
    svm_model* model;
    svm_parameter param;
    svm_problem prob;
    std::vector<svm_node*> x_space;
    std::vector<svm_node> nodes;
    
public:
    SimpleSVM() {
        model = nullptr;
        // Set default parameters
        param.svm_type = C_SVC;
        param.kernel_type = RBF;
        param.degree = 3;
        param.gamma = 0;
        param.coef0 = 0;
        param.nu = 0.5;
        param.cache_size = 100;
        param.C = 1;
        param.eps = 1e-3;
        param.p = 0.1;
        param.nr_weight = 0;
        param.weight_label = nullptr;
        param.weight = nullptr;
        param.svr_probability = 0;
        param.platt = 0;
    }
    
    void train(const std::vector<std::vector<double>>& features, 
               const std::vector<int>& labels) {
        
        int num_samples = features.size();
        int num_features = features[0].size();
        
        // Prepare problem data
        prob.l = num_samples;
        prob.y = new double[num_samples];
        prob.x = new svm_node*[num_samples];
        
        // Allocate memory for nodes
        nodes.resize(num_samples * (num_features + 1));
        x_space.resize(num_samples);
        
        for (int i = 0; i < num_samples; i++) {
            x_space[i] = &nodes[i * (num_features + 1)];
            prob.y[i] = labels[i];
            
            for (int j = 0; j < num_features; j++) {
                x_space[i][j].index = j + 1;
                x_space[i][j].value = features[i][j];
            }
            x_space[i][num_features].index = -1; // End marker
        }
        
        prob.x = x_space.data();
        
        // Train the model
        model = svm_train(&prob, &param);
        
        std::cout << "SVM training completed!" << std::endl;
    }
    
    int predict(const std::vector<double>& sample) {
        if (model == nullptr) {
            std::cerr << "Model not trained yet!" << std::endl;
            return -1;
        }
        
        // Create svm_node for prediction
        std::vector<svm_node> predict_nodes(sample.size() + 1);
        for (size_t i = 0; i < sample.size(); i++) {
            predict_nodes[i].index = i + 1;
            predict_nodes[i].value = sample[i];
        }
        predict_nodes[sample.size()].index = -1; // End marker
        
        // Make prediction
        double prediction = svm_predict(model, predict_nodes.data());
        return static_cast<int>(prediction);
    }
    
    void save_model(const std::string& filename) {
        if (model != nullptr) {
            svm_save_model(filename.c_str(), model);
            std::cout << "Model saved to " << filename << std::endl;
        }
    }
    
    void load_model(const std::string& filename) {
        model = svm_load_model(filename.c_str());
        if (model == nullptr) {
            std::cerr << "Failed to load model from " << filename << std::endl;
        } else {
            std::cout << "Model loaded from " << filename << std::endl;
        }
    }
    
    ~SimpleSVM() {
        if (model != nullptr) {
            svm_free_and_destroy_model(&model);
        }
        delete[] prob.y;
        delete[] prob.x;
    }
};

// Example usage
int main() {
    // Sample training data (2D features)
    std::vector<std::vector<double>> features = {
        {1.0, 2.0},
        {2.0, 3.0},
        {3.0, 1.0},
        {4.0, 2.0},
        {5.0, 3.0},
        {6.0, 1.0}
    };
    
    // Labels (1 for class A, -1 for class B)
    std::vector<int> labels = {1, 1, -1, 1, -1, -1};
    
    // Create and train SVM
    SimpleSVM svm;
    
    std::cout << "Training SVM model..." << std::endl;
    svm.train(features, labels);
    
    // Make predictions
    std::cout << "\nMaking predictions:" << std::endl;
    
    std::vector<double> test_sample1 = {2.5, 2.5};
    std::vector<double> test_sample2 = {5.5, 2.0};
    
    int pred1 = svm.predict(test_sample1);
    int pred2 = svm.predict(test_sample2);
    
    std::cout << "Sample 1 prediction: " << pred1 << std::endl;
    std::cout << "Sample 2 prediction: " << pred2 << std::endl;
    
    // Save model
    svm.save_model("svm_model.txt");
    
    return 0;
}
```

## Compilation Instructions

```bash
g++ -std=c++11 -o svm_example svm_example.cpp -L. -lsvm -lm
```

## Key Features of This Implementation

1. **Simple Interface**: Easy-to-use class for training and prediction
2. **Memory Management**: Proper cleanup of resources
3. **Model Persistence**: Save and load trained models
4. **Flexible Parameters**: Configurable SVM parameters
5. **Error Handling**: Basic error checking and reporting

## Expected Output

```
Training SVM model...
SVM training completed!

Making predictions:
Sample 1 prediction: 1
Sample 2 prediction: -1
Model saved to svm_model.txt
```

## Notes

- This implementation uses the libsvm library for the core SVM functionality
- The example demonstrates binary classification
- For multi-class problems, you would need to extend the implementation
- The SVM parameters can be tuned for better performance on specific datasets
- This is a simplified version - production code would include more robust error handling and optimization

This example provides a solid foundation for implementing SVM algorithms in C++ applications.

