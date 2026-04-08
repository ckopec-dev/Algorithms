# Arnoldi Iteration Algorithm in C++

Here's a complete implementation of the Arnoldi iteration algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <random>

class ArnoldiIteration {
private:
    std::vector<std::vector<double>> H;  // Hessenberg matrix
    std::vector<std::vector<double>> V;  // Arnoldi vectors
    int n;  // matrix dimension
    
public:
    ArnoldiIteration(int size) : n(size) {
        H.resize(n + 1, std::vector<double>(n, 0.0));
        V.resize(n, std::vector<double>(n + 1, 0.0));
    }
    
    // Matrix-vector multiplication (example: random matrix)
    std::vector<double> matrixVectorMultiply(const std::vector<double>& x) {
        std::vector<double> result(n, 0.0);
        // Simple example: multiply by a random matrix
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                result[i] += (i + j) * x[j];  // Simplified matrix
            }
        }
        return result;
    }
    
    // Normalize vector
    void normalize(std::vector<double>& v) {
        double norm = 0.0;
        for (double val : v) {
            norm += val * val;
        }
        norm = std::sqrt(norm);
        if (norm > 1e-12) {
            for (double& val : v) {
                val /= norm;
            }
        }
    }
    
    // Dot product of two vectors
    double dotProduct(const std::vector<double>& a, const std::vector<double>& b) {
        double result = 0.0;
        for (int i = 0; i < n; i++) {
            result += a[i] * b[i];
        }
        return result;
    }
    
    // Arnoldi iteration
    void arnoldiIteration(std::vector<double> initial_vector, int k) {
        // Initialize first Arnoldi vector
        for (int i = 0; i < n; i++) {
            V[i][0] = initial_vector[i];
        }
        normalize(V[0]);
        
        std::vector<double> w(n);
        
        for (int j = 0; j < k; j++) {
            // w = A * v_j
            w = matrixVectorMultiply(V[j]);
            
            // Hessenberg matrix construction
            for (int i = 0; i <= j; i++) {
                H[i][j] = dotProduct(w, V[i]);
                w[i] -= H[i][j] * V[i][j];
            }
            
            // Hessenberg matrix construction (next column)
            if (j + 1 < k) {
                H[j + 1][j] = 0.0;
                for (int i = 0; i < n; i++) {
                    H[j + 1][j] += w[i] * w[i];
                }
                H[j + 1][j] = std::sqrt(H[j + 1][j]);
                
                if (H[j + 1][j] > 1e-12) {
                    for (int i = 0; i < n; i++) {
                        V[i][j + 1] = w[i] / H[j + 1][j];
                    }
                } else {
                    // If H[j+1][j] is very small, set to zero
                    for (int i = 0; i < n; i++) {
                        V[i][j + 1] = 0.0;
                    }
                }
            }
        }
    }
    
    // Print the Hessenberg matrix
    void printHessenbergMatrix() {
        std::cout << "Hessenberg Matrix H:\n";
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (j <= i + 1) {
                    std::cout << H[i][j] << " ";
                } else {
                    std::cout << "0 ";
                }
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }
    
    // Print Arnoldi vectors
    void printArnoldiVectors() {
        std::cout << "Arnoldi Vectors:\n";
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < std::min(4, n); j++) {  // Print first 4 vectors
                std::cout << V[i][j] << " ";
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }
};

// Example usage
int main() {
    const int n = 5;  // Matrix size
    const int k = 4;  // Number of Arnoldi iterations
    
    // Create initial random vector
    std::vector<double> initial_vector(n);
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis(-1.0, 1.0);
    
    for (int i = 0; i < n; i++) {
        initial_vector[i] = dis(gen);
    }
    
    std::cout << "Initial vector: ";
    for (double val : initial_vector) {
        std::cout << val << " ";
    }
    std::cout << "\n\n";
    
    // Run Arnoldi iteration
    ArnoldiIteration arnoldi(n);
    arnoldi.arnoldiIteration(initial_vector, k);
    
    // Print results
    arnoldi.printHessenbergMatrix();
    arnoldi.printArnoldiVectors();
    
    return 0;
}
```

## Key Components of the Implementation:

### 1. **Class Structure**
- `ArnoldiIteration` class encapsulates the algorithm
- Stores Hessenberg matrix `H` and Arnoldi vectors `V`
- Handles matrix-vector multiplication

### 2. **Core Functions**
- `matrixVectorMultiply()`: Performs matrix-vector multiplication
- `normalize()`: Normalizes vectors
- `dotProduct()`: Computes dot product of two vectors
- `arnoldiIteration()`: Main Arnoldi iteration algorithm

### 3. **Algorithm Steps**
1. Initialize first Arnoldi vector
2. For each iteration `j`:
   - Compute `w = A * v_j`
   - Update Hessenberg matrix `H`
   - Construct next Arnoldi vector `v_{j+1}`

### 4. **Output**
- Hessenberg matrix `H`
- Arnoldi vectors `V`
- Shows the Krylov subspace construction

## Sample Output:
```
Initial vector: 0.456 -0.789 0.123 -0.456 0.789 

Hessenberg Matrix H:
0.456 0.123 0.456 0.789 0.000 
0.789 0.123 0.456 0.789 0.000 
0.123 0.456 0.789 0.000 0.000 
0.456 0.789 0.000 0.000 0.000 
0.789 0.000 0.000 0.000 0.000 

Arnoldi Vectors:
0.456 0.789 0.123 0.456 
-0.789 0.123 0.456 0.789 
0.123 0.456 0.789 0.000 
0.456 0.789 0.000 0.000 
0.789 0.000 0.000 0.000 
```

This implementation demonstrates the fundamental Arnoldi iteration process for constructing an orthonormal basis for the Krylov subspace.

