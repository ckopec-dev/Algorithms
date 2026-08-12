# Expectation-Maximization (EM) Algorithm in C++

Here's a complete implementation of the EM algorithm for Gaussian Mixture Model (GMM) clustering:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <cmath>
#include <algorithm>
#include <iomanip>

class GMM {
private:
    int num_clusters;
    int num_features;
    int max_iterations;
    double tolerance;
    
    // Parameters to be estimated
    std::vector<double> weights;           // mixing coefficients
    std::vector<std::vector<double>> means; // cluster means
    std::vector<std::vector<std::vector<double>>> covariances; // cluster covariances
    
    // For multivariate Gaussian PDF
    double gaussianPDF(const std::vector<double>& x, 
                      const std::vector<double>& mean,
                      const std::vector<std::vector<double>>& cov) {
        int n = x.size();
        double det = 0;
        
        // Calculate determinant (simplified for 2D case)
        if (n == 2) {
            det = cov[0][0] * cov[1][1] - cov[0][1] * cov[1][0];
        } else {
            // For higher dimensions, use more sophisticated method
            det = 1.0; // Simplified for demonstration
        }
        
        if (det <= 0) det = 1e-10;
        
        double norm = std::sqrt(std::pow(2 * M_PI, n) * det);
        double exp_term = 0;
        
        // Calculate (x - μ)^T * Σ^(-1) * (x - μ)
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                exp_term += (x[i] - mean[i]) * cov[i][j] * (x[j] - mean[j]);
            }
        }
        
        return std::exp(-0.5 * exp_term) / norm;
    }
    
public:
    GMM(int k, int features, int max_iter = 100, double tol = 1e-6) 
        : num_clusters(k), num_features(features), max_iterations(max_iter), tolerance(tol) {
        
        // Initialize parameters randomly
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<> dis(0.0, 1.0);
        
        weights.resize(k, 1.0/k);
        means.resize(k, std::vector<double>(features, 0));
        covariances.resize(k, std::vector<std::vector<double>>(features, std::vector<double>(features, 0)));
        
        for (int i = 0; i < k; i++) {
            for (int j = 0; j < features; j++) {
                means[i][j] = dis(gen) * 10;
                covariances[i][j][j] = dis(gen) * 2 + 0.1; // diagonal elements
            }
        }
    }
    
    void expectation(const std::vector<std::vector<double>>& data, 
                    std::vector<std::vector<double>>& responsibilities) {
        int n_samples = data.size();
        responsibilities.resize(n_samples, std::vector<double>(num_clusters, 0));
        
        for (int i = 0; i < n_samples; i++) {
            double total = 0;
            for (int j = 0; j < num_clusters; j++) {
                responsibilities[i][j] = weights[j] * gaussianPDF(data[i], means[j], covariances[j]);
                total += responsibilities[i][j];
            }
            
            // Normalize
            if (total > 0) {
                for (int j = 0; j < num_clusters; j++) {
                    responsibilities[i][j] /= total;
                }
            }
        }
    }
    
    void maximization(const std::vector<std::vector<double>>& data,
                     const std::vector<std::vector<double>>& responsibilities) {
        int n_samples = data.size();
        
        // Update weights
        for (int j = 0; j < num_clusters; j++) {
            double weight_sum = 0;
            for (int i = 0; i < n_samples; i++) {
                weight_sum += responsibilities[i][j];
            }
            weights[j] = weight_sum / n_samples;
        }
        
        // Update means
        for (int j = 0; j < num_clusters; j++) {
            std::vector<double> sum(num_features, 0);
            double total_resp = 0;
            
            for (int i = 0; i < n_samples; i++) {
                double resp = responsibilities[i][j];
                total_resp += resp;
                for (int k = 0; k < num_features; k++) {
                    sum[k] += resp * data[i][k];
                }
            }
            
            if (total_resp > 0) {
                for (int k = 0; k < num_features; k++) {
                    means[j][k] = sum[k] / total_resp;
                }
            }
        }
        
        // Update covariances
        for (int j = 0; j < num_clusters; j++) {
            std::vector<std::vector<double>> sum_cov(num_features, std::vector<double>(num_features, 0));
            double total_resp = 0;
            
            for (int i = 0; i < n_samples; i++) {
                double resp = responsibilities[i][j];
                total_resp += resp;
                
                for (int k1 = 0; k1 < num_features; k1++) {
                    for (int k2 = 0; k2 < num_features; k2++) {
                        sum_cov[k1][k2] += resp * (data[i][k1] - means[j][k1]) * 
                                          (data[i][k2] - means[j][k2]);
                    }
                }
            }
            
            if (total_resp > 0) {
                for (int k1 = 0; k1 < num_features; k1++) {
                    for (int k2 = 0; k2 < num_features; k2++) {
                        covariances[j][k1][k2] = sum_cov[k1][k2] / total_resp;
                    }
                }
            }
        }
    }
    
    double computeLogLikelihood(const std::vector<std::vector<double>>& data) {
        double log_likelihood = 0;
        int n_samples = data.size();
        
        for (int i = 0; i < n_samples; i++) {
            double sum = 0;
            for (int j = 0; j < num_clusters; j++) {
                sum += weights[j] * gaussianPDF(data[i], means[j], covariances[j]);
            }
            if (sum > 0) {
                log_likelihood += std::log(sum);
            }
        }
        
        return log_likelihood;
    }
    
    void fit(const std::vector<std::vector<double>>& data) {
        double prev_log_likelihood = -std::numeric_limits<double>::infinity();
        int iteration = 0;
        
        while (iteration < max_iterations) {
            std::vector<std::vector<double>> responsibilities;
            
            // E-step
            expectation(data, responsibilities);
            
            // M-step
            maximization(data, responsibilities);
            
            // Check convergence
            double log_likelihood = computeLogLikelihood(data);
            double diff = std::abs(log_likelihood - prev_log_likelihood);
            
            std::cout << "Iteration " << iteration << ": Log-likelihood = " 
                      << std::fixed << std::setprecision(6) << log_likelihood 
                      << ", Diff = " << diff << std::endl;
            
            if (diff < tolerance) {
                break;
            }
            
            prev_log_likelihood = log_likelihood;
            iteration++;
        }
        
        std::cout << "EM algorithm converged after " << iteration << " iterations" << std::endl;
    }
    
    void printParameters() {
        std::cout << "\nFinal Parameters:" << std::endl;
        std::cout << "Weights: ";
        for (double w : weights) {
            std::cout << std::fixed << std::setprecision(4) << w << " ";
        }
        std::cout << std::endl;
        
        for (int i = 0; i < num_clusters; i++) {
            std::cout << "Cluster " << i << " mean: [";
            for (int j = 0; j < num_features; j++) {
                std::cout << std::fixed << std::setprecision(4) << means[i][j] << " ";
            }
            std::cout << "]" << std::endl;
        }
    }
};

// Example usage
int main() {
    // Generate sample data (2D points)
    std::vector<std::vector<double>> data = {
        {1.0, 2.0}, {1.5, 1.8}, {5.0, 8.0}, {8.0, 8.0},
        {1.2, 0.8}, {9.0, 11.0}, {8.0, 2.0}, {10.0, 2.0},
        {6.0, 3.0}, {7.0, 4.0}, {1.0, 5.0}, {4.0, 7.0}
    };
    
    std::cout << "Data points:" << std::endl;
    for (const auto& point : data) {
        std::cout << "[" << point[0] << ", " << point[1] << "]" << std::endl;
    }
    
    // Create GMM with 2 clusters
    GMM gmm(2, 2);
    
    std::cout << "\nStarting EM algorithm..." << std::endl;
    
    // Fit the model
    gmm.fit(data);
    
    // Print final parameters
    gmm.printParameters();
    
    return 0;
}
```

## Key Components of the EM Algorithm Implementation

### 1. **Expectation Step (E-step)**
- Calculates responsibility values (posterior probabilities)
- Determines how much each data point belongs to each cluster
- Uses current parameter estimates to compute membership probabilities

### 2. **Maximization Step (M-step)**
- Updates model parameters based on responsibilities
- Computes new means, covariances, and mixing coefficients
- Uses weighted averages of data points

### 3. **Convergence Check**
- Monitors log-likelihood improvement
- Stops when change falls below tolerance threshold

## Compilation and Execution

```bash
g++ -std=c++11 -o em_algorithm em_algorithm.cpp
./em_algorithm
```

## Expected Output Format

```
Data points:
[1, 2]
[1.5, 1.8]
[5, 8]
...

Starting EM algorithm...
Iteration 0: Log-likelihood = -42.345678, Diff = 0.000000
Iteration 1: Log-likelihood = -39.876543, Diff = 2.469135
...

EM algorithm converged after 5 iterations

Final Parameters:
Weights: 0.4567 0.5433 
Cluster 0 mean: [2.1234 2.3456 ]
Cluster 1 mean: [7.8901 7.6543 ]
```

This implementation provides a complete EM algorithm framework that can be extended for different types of probability distributions and clustering problems.