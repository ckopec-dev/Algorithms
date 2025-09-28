# Matrix Exponentiation Algorithm in C++

Here's a complete implementation of matrix exponentiation using fast exponentiation (binary exponentiation) algorithm:

```cpp
#include <iostream>
#include <vector>
using namespace std;

class Matrix {
private:
    vector<vector<long long>> data;
    int rows, cols;
    
public:
    // Constructor
    Matrix(int n, int m) : rows(n), cols(m) {
        data.resize(rows, vector<long long>(cols, 0));
    }
    
    // Constructor with initial values
    Matrix(const vector<vector<long long>>& mat) {
        data = mat;
        rows = mat.size();
        cols = mat[0].size();
    }
    
    // Get dimensions
    int getRows() const { return rows; }
    int getCols() const { return cols; }
    
    // Access operator
    vector<long long>& operator[](int index) {
        return data[index];
    }
    
    // Matrix multiplication
    Matrix multiply(const Matrix& other) const {
        if (cols != other.rows) {
            throw "Matrix dimensions don't match for multiplication";
        }
        
        Matrix result(rows, other.cols);
        
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < other.cols; j++) {
                result[i][j] = 0;
                for (int k = 0; k < cols; k++) {
                    result[i][j] += data[i][k] * other[k][j];
                }
            }
        }
        
        return result;
    }
    
    // Identity matrix
    static Matrix identity(int n) {
        Matrix I(n, n);
        for (int i = 0; i < n; i++) {
            I[i][i] = 1;
        }
        return I;
    }
    
    // Print matrix
    void print() const {
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                cout << data[i][j] << " ";
            }
            cout << endl;
        }
    }
};

// Fast matrix exponentiation using binary exponentiation
Matrix matrixPower(Matrix base, long long n) {
    if (n == 0) {
        return Matrix::identity(base.getRows());
    }
    
    if (n == 1) {
        return base;
    }
    
    // For negative powers, we need to compute inverse
    if (n < 0) {
        throw "Negative exponent not supported for this implementation";
    }
    
    Matrix result = Matrix::identity(base.getRows());
    Matrix base_copy = base;
    
    while (n > 0) {
        if (n % 2 == 1) {
            result = result.multiply(base_copy);
        }
        base_copy = base_copy.multiply(base_copy);
        n /= 2;
    }
    
    return result;
}

// Example usage: Computing Fibonacci numbers using matrix exponentiation
long long fibonacci(long long n) {
    if (n == 0) return 0;
    if (n == 1) return 1;
    
    // Fibonacci transition matrix
    Matrix fib_matrix = Matrix({
        {1, 1},
        {1, 0}
    });
    
    // Compute matrix^(n-1)
    Matrix result = matrixPower(fib_matrix, n - 1);
    
    // F(n) = result[0][0] * F(1) + result[0][1] * F(0)
    return result[0][0] * 1 + result[0][1] * 0;
}

int main() {
    // Example 1: Basic matrix exponentiation
    cout << "Example 1: Matrix Exponentiation" << endl;
    Matrix mat = Matrix({
        {2, 1},
        {1, 1}
    });
    
    cout << "Original matrix:" << endl;
    mat.print();
    
    Matrix result = matrixPower(mat, 3);
    cout << "\nMatrix^3:" << endl;
    result.print();
    
    // Example 2: Fibonacci using matrix exponentiation
    cout << "\nExample 2: Fibonacci Numbers" << endl;
    for (int i = 0; i <= 10; i++) {
        cout << "F(" << i << ") = " << fibonacci(i) << endl;
    }
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Matrix Class**
- Supports dynamic matrix creation
- Matrix multiplication operation
- Identity matrix generation
- Print functionality for debugging

### 2. **Fast Exponentiation Algorithm**
- Uses binary exponentiation technique
- Time complexity: O(log n)
- Space complexity: O(1) additional space

### 3. **Algorithm Steps**
1. If n = 0, return identity matrix
2. If n = 1, return the base matrix
3. Use binary representation of n:
   - If current bit is 1, multiply result by current base
   - Square the base and halve the exponent
4. Continue until exponent becomes 0

### 4. **Applications Demonstrated**
- General matrix exponentiation
- Fibonacci number computation (more efficient than recursion)

### 5. **Time Complexity**
- Matrix multiplication: O(n³) for n×n matrices
- Matrix exponentiation: O(log k × n³) where k is the exponent

This implementation efficiently computes matrix powers and can be used for various mathematical applications including solving recurrence relations, graph algorithms, and more.

