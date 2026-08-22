# Metropolis-Hastings Algorithm in C++

Here's a complete implementation of the Metropolis-Hastings algorithm to sample from a target distribution:

```cpp
#include <iostream>
#include <random>
#include <vector>
#include <cmath>

class MetropolisHastings {
private:
    std::mt19937 rng;  // Random number generator
    std::uniform_real_distribution<double> uniform_dist;
    std::normal_distribution<double> normal_dist;
    
public:
    MetropolisHastings(unsigned int seed = 42) : 
        rng(seed), uniform_dist(0.0, 1.0), normal_dist(0.0, 1.0) {}
    
    // Target distribution: unnormalized posterior (e.g., mixture of Gaussians)
    double targetDistribution(double x) {
        // Example: mixture of two Gaussians
        return 0.3 * exp(-0.5 * pow(x - 2.0, 2)) + 
               0.7 * exp(-0.5 * pow(x + 1.5, 2));
    }
    
    // Proposal distribution: normal with given standard deviation
    double propose(double current_x, double proposal_std = 0.5) {
        return current_x + normal_dist(rng) * proposal_std;
    }
    
    // Metropolis-Hastings sampling
    std::vector<double> sample(int num_samples, double initial_value = 0.0, 
                              double proposal_std = 0.5, bool burn_in = true) {
        std::vector<double> samples;
        double current_x = initial_value;
        
        // Burn-in period (optional)
        if (burn_in) {
            for (int i = 0; i < 1000; ++i) {
                double proposed_x = propose(current_x, proposal_std);
                
                // Calculate acceptance probability
                double acceptance_ratio = targetDistribution(proposed_x) / 
                                        targetDistribution(current_x);
                
                // Accept or reject
                if (uniform_dist(rng) < acceptance_ratio) {
                    current_x = proposed_x;
                }
            }
        }
        
        // Generate samples
        for (int i = 0; i < num_samples; ++i) {
            double proposed_x = propose(current_x, proposal_std);
            
            // Calculate acceptance probability
            double acceptance_ratio = targetDistribution(proposed_x) / 
                                    targetDistribution(current_x);
            
            // Accept or reject
            if (uniform_dist(rng) < acceptance_ratio) {
                current_x = proposed_x;
            }
            
            samples.push_back(current_x);
        }
        
        return samples;
    }
};

// Example usage
int main() {
    // Create Metropolis-Hastings sampler
    MetropolisHastings mh(42);
    
    // Generate samples
    std::vector<double> samples = mh.sample(10000, 0.0, 0.5, true);
    
    // Calculate basic statistics
    double sum = 0.0;
    for (double x : samples) {
        sum += x;
    }
    double mean = sum / samples.size();
    
    // Calculate variance
    double var_sum = 0.0;
    for (double x : samples) {
        var_sum += pow(x - mean, 2);
    }
    double variance = var_sum / samples.size();
    
    // Print results
    std::cout << "Metropolis-Hastings Sampling Results:" << std::endl;
    std::cout << "Number of samples: " << samples.size() << std::endl;
    std::cout << "Sample mean: " << mean << std::endl;
    std::cout << "Sample variance: " << variance << std::endl;
    
    // Print first 10 samples
    std::cout << "\nFirst 10 samples:" << std::endl;
    for (int i = 0; i < 10 && i < samples.size(); ++i) {
        std::cout << "Sample " << i+1 << ": " << samples[i] << std::endl;
    }
    
    return 0;
}
```

## Key Components Explained:

### 1. **Target Distribution**
```cpp
double targetDistribution(double x) {
    // Example: mixture of two Gaussians
    return 0.3 * exp(-0.5 * pow(x - 2.0, 2)) + 
           0.7 * exp(-0.5 * pow(x + 1.5, 2));
}
```
This represents the unnormalized posterior distribution we want to sample from.

### 2. **Proposal Distribution**
```cpp
double propose(double current_x, double proposal_std = 0.5) {
    return current_x + normal_dist(rng) * proposal_std;
}
```
Uses a normal distribution centered at the current value with specified standard deviation.

### 3. **Acceptance Ratio**
```cpp
double acceptance_ratio = targetDistribution(proposed_x) / 
                        targetDistribution(current_x);
```
Compares the target distribution values to decide whether to accept the proposed sample.

### 4. **Main Algorithm Flow**
1. Initialize current state
2. For each iteration:
   - Propose a new state
   - Calculate acceptance probability
   - Accept or reject based on uniform random draw
   - Store accepted samples

## Compilation and Execution:

```bash
g++ -std=c++11 -o mh_sampler mh_sampler.cpp
./mh_sampler
```

This implementation demonstrates how to:
- Set up a Metropolis-Hastings sampler
- Define target and proposal distributions
- Perform the sampling algorithm with burn-in period
- Extract statistical properties from the samples

The algorithm is particularly useful for sampling from complex, high-dimensional distributions where direct sampling methods are not feasible.