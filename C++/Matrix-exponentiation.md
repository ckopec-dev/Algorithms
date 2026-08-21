# Matrix Exponentiation Algorithm in C++

Matrix exponentiation is a technique used to compute matrix powers efficiently using fast exponentiation (binary exponentiation) to reduce time complexity from O(n) to O(log n).

```cpp
#include <iostream>
#include <vector>
using namespace std;

class Matrix {
private:
    vector<vector<long long>> mat;
    int rows, cols;
    
public:
    // Constructor
    Matrix(int r, int c) : rows(r), cols(c) {
        mat.resize(r, vector<long long>(c, 0));
    }
    
    // Get dimensions
    int getRows() const { return rows; }
    int getCols() const { return cols; }
    
    // Access operator
    vector<long long>& operator[](int index) {
        return mat[index];
    }
    
    // Matrix multiplication
    Matrix multiply(const Matrix& other) const {
        if (cols != other.rows) {
            cout << "Error: Matrix dimensions don't match for multiplication\n";
            return Matrix(0, 0);
        }
        
        Matrix result(rows, other.cols);
        
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < other.cols; j++) {
                result[i][j] = 0;
                for (int k = 0; k < cols; k++) {
                    result[i][j] += mat[i][k] * other.mat[k][j];
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
    
    // Matrix exponentiation using binary exponentiation
    static Matrix power(Matrix base, long long n) {
        if (n == 0) {
            return identity(base.getRows());
        }
        
        Matrix result = identity(base.getRows());
        Matrix temp = base;
        
        while (n > 0) {
            if (n % 2 == 1) {
                result = result.multiply(temp);
            }
            temp = temp.multiply(temp);
            n /= 2;
        }
        
        return result;
    }
    
    // Print matrix
    void print() const {
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                cout << mat[i][j] << " ";
            }
            cout << endl;
        }
    }
};

// Example usage: Computing Fibonacci number using matrix exponentiation
long long fibonacci(int n) {
    if (n == 0) return 0;
    if (n == 1) return 1;
    
    // Base matrix for Fibonacci: [[1, 1], [1, 0]]
    Matrix base(2, 2);
    base[0][0] = 1; base[0][1] = 1;
    base[1][0] = 1; base[1][1] = 0;
    
    // Compute base^(n-1)
    Matrix result = Matrix::power(base, n - 1);
    
    // Fibonacci(n) = result[0][0] * F(1) + result[0][1] * F(0)
    return result[0][0] * 1 + result[0][1] * 0;
}

int main() {
    cout << "Matrix Exponentiation Example\n";
    cout << "=============================\n\n";
    
    // Example 1: Basic matrix exponentiation
    cout << "Example 1: Matrix exponentiation\n";
    Matrix A(2, 2);
    A[0][0] = 1; A[0][1] = 2;
    A[1][0] = 3; A[1][1] = 4;
    
    cout << "Matrix A:\n";
    A.print();
    
    Matrix A_squared = Matrix::power(A, 2);
    cout << "\nA^2:\n";
    A_squared.print();
    
    // Example 2: Fibonacci using matrix exponentiation
    cout << "\nExample 2: Fibonacci numbers\n";
    for (int i = 0; i <= 10; i++) {
        cout << "F(" << i << ") = " << fibonacci(i) << endl;
    }
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Matrix Class**
- Stores matrix data in a 2D vector
- Provides basic matrix operations (multiplication, access)
- Includes identity matrix creation

### 2. **Fast Matrix Exponentiation**
- Uses binary exponentiation technique
- Time complexity: O(log n × m³) where m is matrix dimension
- Space complexity: O(m²)

### 3. **Applications Shown**
- Basic matrix power computation
- Fibonacci sequence calculation (efficient O(log n) approach)

## Sample Output:
```
Matrix Exponentiation Example
=============================

Example 1: Matrix exponentiation
Matrix A:
1 2 
3 4 

A^2:
7 10 
15 22 

Example 2: Fibonacci numbers
F(0) = 0
F(1) = 1
F(2) = 1
F(3) = 2
F(4) = 3
F(5) = 5
F(6) = 8
F(7) = 13
F(8) = 21
F(9) = 34
F(10) = 55
```

This implementation demonstrates how matrix exponentiation can be used not only for general matrix power computation but also for efficiently solving problems like Fibonacci sequences, linear recurrence relations, and more complex mathematical computations.