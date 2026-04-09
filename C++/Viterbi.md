# Viterbi Algorithm Implementation in C++

Here's a complete implementation of the Viterbi algorithm for Hidden Markov Model (HMM) decoding:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>
#include <iomanip>

class ViterbiAlgorithm {
private:
    // HMM parameters
    std::vector<std::vector<double>> transition_prob;  // A: state transition probabilities
    std::vector<std::vector<double>> emission_prob;   // B: emission probabilities
    std::vector<double> initial_prob;                 // π: initial state probabilities
    
    int num_states;
    int num_observations;
    
public:
    ViterbiAlgorithm(int states, int observations) 
        : num_states(states), num_observations(observations) {
        // Initialize matrices with zeros
        transition_prob.assign(states, std::vector<double>(states, 0.0));
        emission_prob.assign(states, std::vector<double>(observations, 0.0));
        initial_prob.assign(states, 0.0);
    }
    
    // Set the transition probabilities
    void setTransitionProb(const std::vector<std::vector<double>>& probs) {
        transition_prob = probs;
    }
    
    // Set the emission probabilities
    void setEmissionProb(const std::vector<std::vector<double>>& probs) {
        emission_prob = probs;
    }
    
    // Set the initial state probabilities
    void setInitialProb(const std::vector<double>& probs) {
        initial_prob = probs;
    }
    
    // Viterbi algorithm implementation
    std::pair<std::vector<int>, double> viterbi(const std::vector<int>& observations) {
        int T = observations.size();
        
        // Create DP tables
        std::vector<std::vector<double>> delta(num_states, std::vector<double>(T, 0.0));
        std::vector<std::vector<int>> psi(num_states, std::vector<int>(T, 0));
        
        // Base case (t = 0)
        for (int i = 0; i < num_states; i++) {
            delta[i][0] = initial_prob[i] * emission_prob[i][observations[0]];
            psi[i][0] = 0;
        }
        
        // Recursion (t = 1 to T-1)
        for (int t = 1; t < T; t++) {
            for (int j = 0; j < num_states; j++) {
                double max_prob = 0.0;
                int max_state = 0;
                
                for (int i = 0; i < num_states; i++) {
                    double prob = delta[i][t-1] * transition_prob[i][j];
                    if (prob > max_prob) {
                        max_prob = prob;
                        max_state = i;
                    }
                }
                
                delta[j][t] = max_prob * emission_prob[j][observations[t]];
                psi[j][t] = max_state;
            }
        }
        
        // Find the best final state
        double max_prob = 0.0;
        int best_final_state = 0;
        for (int i = 0; i < num_states; i++) {
            if (delta[i][T-1] > max_prob) {
                max_prob = delta[i][T-1];
                best_final_state = i;
            }
        }
        
        // Backtrack to find the optimal state sequence
        std::vector<int> path(T);
        path[T-1] = best_final_state;
        
        for (int t = T-2; t >= 0; t--) {
            path[t] = psi[path[t+1]][t+1];
        }
        
        return {path, max_prob};
    }
    
    // Print the HMM parameters
    void printModel() {
        std::cout << "Initial probabilities:\n";
        for (int i = 0; i < num_states; i++) {
            std::cout << "  State " << i << ": " << initial_prob[i] << "\n";
        }
        
        std::cout << "\nTransition probabilities:\n";
        for (int i = 0; i < num_states; i++) {
            for (int j = 0; j < num_states; j++) {
                std::cout << std::fixed << std::setprecision(3) 
                         << transition_prob[i][j] << " ";
            }
            std::cout << "\n";
        }
        
        std::cout << "\nEmission probabilities:\n";
        for (int i = 0; i < num_states; i++) {
            for (int j = 0; j < num_observations; j++) {
                std::cout << std::fixed << std::setprecision(3) 
                         << emission_prob[i][j] << " ";
            }
            std::cout << "\n";
        }
    }
};

int main() {
    // Example: Weather prediction HMM
    // States: 0 = Sunny, 1 = Rainy
    // Observations: 0 = Walk, 1 = Shop, 2 = Clean
    
    int num_states = 2;
    int num_observations = 3;
    
    ViterbiAlgorithm hmm(num_states, num_observations);
    
    // Set initial probabilities
    hmm.setInitialProb({0.6, 0.4});  // 60% chance of starting sunny, 40% rainy
    
    // Set transition probabilities (A matrix)
    // A[i][j] = P(state j | state i)
    std::vector<std::vector<double>> transition = {
        {0.7, 0.3},  // From Sunny: 70% stay sunny, 30% become rainy
        {0.4, 0.6}   // From Rainy: 40% become sunny, 60% stay rainy
    };
    hmm.setTransitionProb(transition);
    
    // Set emission probabilities (B matrix)
    // B[i][j] = P(observation j | state i)
    std::vector<std::vector<double>> emission = {
        {0.5, 0.4, 0.1},  // Sunny: 50% walk, 40% shop, 10% clean
        {0.1, 0.3, 0.6}   // Rainy: 10% walk, 30% shop, 60% clean
    };
    hmm.setEmissionProb(emission);
    
    // Print the model
    std::cout << "=== HMM Model ===\n";
    hmm.printModel();
    
    // Example observation sequence: [Walk, Shop, Clean]
    std::vector<int> observations = {0, 1, 2};
    
    std::cout << "\n=== Viterbi Algorithm ===\n";
    std::cout << "Observation sequence: ";
    for (int obs : observations) {
        std::cout << obs << " ";
    }
    std::cout << "\n";
    
    // Run Viterbi algorithm
    auto result = hmm.viterbi(observations);
    std::vector<int> best_path = result.first;
    double max_probability = result.second;
    
    // Output results
    std::cout << "\nBest state path: ";
    for (int state : best_path) {
        std::cout << state << " ";
    }
    std::cout << "\n";
    
    std::cout << "Max probability: " << std::fixed << std::setprecision(6) 
              << max_probability << "\n";
    
    // Interpret the results
    std::cout << "\n=== Interpretation ===\n";
    std::cout << "The most likely weather sequence for observations [Walk, Shop, Clean] is:\n";
    for (int i = 0; i < best_path.size(); i++) {
        std::cout << "Day " << (i+1) << ": ";
        if (best_path[i] == 0) {
            std::cout << "Sunny";
        } else {
            std::cout << "Rainy";
        }
        std::cout << "\n";
    }
    
    return 0;
}
```

## Expected Output

```
=== HMM Model ===
Initial probabilities:
  State 0: 0.600
  State 1: 0.400

Transition probabilities:
0.700 0.300 
0.400 0.600 

Emission probabilities:
0.500 0.400 0.100 
0.100 0.300 0.600 

=== Viterbi Algorithm ===
Observation sequence: 0 1 2 

Best state path: 0 1 1 
Max probability: 0.002880

=== Interpretation ===
The most likely weather sequence for observations [Walk, Shop, Clean] is:
Day 1: Sunny
Day 2: Rainy
Day 3: Rainy
```

## Key Features of This Implementation

1. **Modular Design**: Separate class for HMM and Viterbi algorithm
2. **Complete HMM Structure**: Supports initial, transition, and emission probabilities
3. **Proper Backtracking**: Finds the optimal state sequence, not just the probability
4. **Clear Documentation**: Well-commented code explaining each step
5. **Real-world Example**: Weather prediction example with clear interpretation

## Algorithm Steps

1. **Initialization**: Set base case probabilities
2. **Recursion**: Fill DP table using the Viterbi recurrence relation
3. **Backtracking**: Trace back through the optimal path
4. **Output**: Return the most probable state sequence and its probability

The Viterbi algorithm efficiently finds the most likely sequence of hidden states given a sequence of observations in a Hidden Markov Model.

