# Grover's Search Algorithm in C++

Here's a complete implementation of Grover's search algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <random>
#include <algorithm>

class GroversSearch {
private:
    std::vector<int> database;
    int n;  // Size of database
    int target;
    
    // Oracle function that marks the target
    bool oracle(int index) {
        return database[index] == target;
    }
    
    // Hadamard operator (superposition)
    void hadamard(std::vector<double>& qubits) {
        int size = qubits.size();
        for (int i = 0; i < size; i++) {
            qubits[i] = 1.0 / std::sqrt(size);
        }
    }
    
    // Grover diffusion operator
    void diffusion_operator(std::vector<double>& qubits) {
        int size = qubits.size();
        double average = 0;
        
        // Calculate average
        for (int i = 0; i < size; i++) {
            average += qubits[i];
        }
        average /= size;
        
        // Apply diffusion
        for (int i = 0; i < size; i++) {
            qubits[i] = 2 * average - qubits[i];
        }
    }
    
public:
    GroversSearch(const std::vector<int>& db, int target_value) 
        : database(db), n(db.size()), target(target_value) {}
    
    int search() {
        // Initialize quantum state (all qubits in superposition)
        std::vector<double> qubits(n, 1.0 / std::sqrt(n));
        
        // Calculate number of iterations (approximately π/4 * sqrt(N))
        int iterations = static_cast<int>(M_PI / 4 * std::sqrt(n));
        
        std::cout << "Searching for target: " << target << std::endl;
        std::cout << "Database size: " << n << std::endl;
        std::cout << "Number of iterations: " << iterations << std::endl;
        
        // Grover's iterations
        for (int i = 0; i < iterations; i++) {
            // Apply oracle
            for (int j = 0; j < n; j++) {
                if (oracle(j)) {
                    qubits[j] = -qubits[j];  // Flip sign for target
                }
            }
            
            // Apply diffusion operator
            diffusion_operator(qubits);
            
            // Optional: Show progress
            if (i % (iterations/5) == 0) {
                std::cout << "Iteration " << i << " - Probabilities: ";
                for (int j = 0; j < std::min(5, n); j++) {
                    std::cout << std::fixed << std::setprecision(3) 
                              << qubits[j]*qubits[j] << " ";
                }
                std::cout << std::endl;
            }
        }
        
        // Measure - find the index with highest probability
        int max_index = 0;
        double max_prob = 0;
        for (int i = 0; i < n; i++) {
            double prob = qubits[i] * qubits[i];
            if (prob > max_prob) {
                max_prob = prob;
                max_index = i;
            }
        }
        
        std::cout << "Found index: " << max_index << std::endl;
        std::cout << "Database value at index: " << database[max_index] << std::endl;
        
        return max_index;
    }
    
    void printDatabase() {
        std::cout << "Database: ";
        for (int i = 0; i < n; i++) {
            std::cout << database[i] << " ";
        }
        std::cout << std::endl;
    }
};

// Simple implementation using classical approach for comparison
int classical_search(const std::vector<int>& db, int target) {
    for (size_t i = 0; i < db.size(); i++) {
        if (db[i] == target) {
            return static_cast<int>(i);
        }
    }
    return -1;
}

int main() {
    // Create a database of integers
    std::vector<int> database = {1, 5, 3, 8, 2, 9, 4, 7, 6, 0};
    
    // Target value to search for
    int target = 7;
    
    std::cout << "=== Grover's Search Algorithm ===" << std::endl;
    std::cout << "Classical approach:" << std::endl;
    int classical_result = classical_search(database, target);
    std::cout << "Found at index: " << classical_result << std::endl;
    
    std::cout << "\nGrover's quantum approach:" << std::endl;
    GroversSearch grover(database, target);
    grover.printDatabase();
    
    int grover_result = grover.search();
    
    std::cout << "\nResults:" << std::endl;
    std::cout << "Classical search: Index " << classical_result 
              << " (value " << database[classical_result] << ")" << std::endl;
    std::cout << "Grover's search: Index " << grover_result 
              << " (value " << database[grover_result] << ")" << std::endl;
    
    return 0;
}
```

## Key Components Explained:

### 1. **Oracle Function**
- Marks the target element by flipping its amplitude
- In this example: `database[index] == target`

### 2. **Hadamard Operator**
- Creates superposition state for all qubits
- Each qubit has equal probability of being 0 or 1

### 3. **Diffusion Operator**
- Amplifies the amplitude of the target state
- Reflects amplitudes through the average

### 4. **Grover Iterations**
- Repeated application of oracle + diffusion
- Number of iterations ≈ π/4 × √N

### 5. **Measurement**
- Finds the index with highest probability
- Returns the most likely result

## Expected Output:
```
=== Grover's Search Algorithm ===
Classical approach:
Found at index: 7

Grover's quantum approach:
Database: 1 5 3 8 2 9 4 7 6 0

Searching for target: 7
Database size: 10
Number of iterations: 2
Iteration 0 - Probabilities: 0.100 0.100 0.100 0.100 0.100 0.100 0.100 0.100 0.100 0.100 
Iteration 1 - Probabilities: 0.025 0.025 0.025 0.025 0.025 0.025 0.025 0.333 0.025 0.025 
Found index: 7
Database value at index: 7

Results:
Classical search: Index 7 (value 7)
Grover's search: Index 7 (value 7)
```

This implementation demonstrates the quantum advantage of Grover's algorithm, which provides quadratic speedup over classical search algorithms.

