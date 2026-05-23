# Strassen's Matrix Multiplication Algorithm in C++

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

class StrassenMatrix {
private:
    // Helper function to add two matrices
    vector<vector<int>> addMatrices(const vector<vector<int>>& A, 
                                   const vector<vector<int>>& B) {
        int n = A.size();
        vector<vector<int>> result(n, vector<int>(n, 0));
        
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                result[i][j] = A[i][j] + B[i][j];
            }
        }
        return result;
    }
    
    // Helper function to subtract two matrices
    vector<vector<int>> subtractMatrices(const vector<vector<int>>& A, 
                                       const vector<vector<int>>& B) {
        int n = A.size();
        vector<vector<int>> result(n, vector<int>(n, 0));
        
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                result[i][j] = A[i][j] - B[i][j];
            }
        }
        return result;
    }
    
    // Helper function to copy a submatrix
    vector<vector<int>> copySubmatrix(const vector<vector<int>>& matrix, 
                                     int startRow, int startCol, int size) {
        vector<vector<int>> submatrix(size, vector<int>(size, 0));
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                submatrix[i][j] = matrix[startRow + i][startCol + j];
            }
        }
        return submatrix;
    }
    
    // Helper function to fill a submatrix
    void fillSubmatrix(vector<vector<int>>& matrix, 
                      const vector<vector<int>>& submatrix, 
                      int startRow, int startCol) {
        int size = submatrix.size();
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                matrix[startRow + i][startCol + j] = submatrix[i][j];
            }
        }
    }
    
    // Strassen's multiplication algorithm
    vector<vector<int>> strassenMultiply(vector<vector<int>>& A, 
                                       vector<vector<int>>& B) {
        int n = A.size();
        
        // Base case: if matrix size is 1x1
        if (n == 1) {
            return {{A[0][0] * B[0][0]}};
        }
        
        // Make sure matrix size is even by padding with zeros if needed
        if (n % 2 == 1) {
            vector<vector<int>> newA(n + 1, vector<int>(n + 1, 0));
            vector<vector<int>> newB(n + 1, vector<int>(n + 1, 0));
            
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    newA[i][j] = A[i][j];
                    newB[i][j] = B[i][j];
                }
            }
            A = newA;
            B = newB;
            n++;
        }
        
        int newSize = n / 2;
        vector<vector<int>> result(n, vector<int>(n, 0));
        
        // Divide matrices into submatrices
        vector<vector<int>> A11 = copySubmatrix(A, 0, 0, newSize);
        vector<vector<int>> A12 = copySubmatrix(A, 0, newSize, newSize);
        vector<vector<int>> A21 = copySubmatrix(A, newSize, 0, newSize);
        vector<vector<int>> A22 = copySubmatrix(A, newSize, newSize, newSize);
        
        vector<vector<int>> B11 = copySubmatrix(B, 0, 0, newSize);
        vector<vector<int>> B12 = copySubmatrix(B, 0, newSize, newSize);
        vector<vector<int>> B21 = copySubmatrix(B, newSize, 0, newSize);
        vector<vector<int>> B22 = copySubmatrix(B, newSize, newSize, newSize);
        
        // Calculate Strassen's seven products
        vector<vector<int>> M1 = strassenMultiply(addMatrices(A11, A22), addMatrices(B11, B22));
        vector<vector<int>> M2 = strassenMultiply(addMatrices(A21, A22), B11);
        vector<vector<int>> M3 = strassenMultiply(A11, subtractMatrices(B12, B22));
        vector<vector<int>> M4 = strassenMultiply(A22, subtractMatrices(B21, B11));
        vector<vector<int>> M5 = strassenMultiply(addMatrices(A11, A12), B22);
        vector<vector<int>> M6 = strassenMultiply(subtractMatrices(A21, A11), addMatrices(B11, B12));
        vector<vector<int>> M7 = strassenMultiply(subtractMatrices(A12, A22), addMatrices(B21, B22));
        
        // Calculate the four quadrants of the result matrix
        vector<vector<int>> C11 = addMatrices(subtractMatrices(addMatrices(M1, M4), M5), M7);
        vector<vector<int>> C12 = addMatrices(M3, M5);
        vector<vector<int>> C21 = addMatrices(M2, M4);
        vector<vector<int>> C22 = addMatrices(subtractMatrices(addMatrices(M1, M3), M2), M6);
        
        // Fill the result matrix
        fillSubmatrix(result, C11, 0, 0);
        fillSubmatrix(result, C12, 0, newSize);
        fillSubmatrix(result, C21, newSize, 0);
        fillSubmatrix(result, C22, newSize, newSize);
        
        // Remove padding if needed
        if (n > A.size()) {
            vector<vector<int>> finalResult(A.size(), vector<int>(A.size(), 0));
            for (int i = 0; i < A.size(); i++) {
                for (int j = 0; j < A.size(); j++) {
                    finalResult[i][j] = result[i][j];
                }
            }
            return finalResult;
        }
        
        return result;
    }
    
public:
    // Public method to perform matrix multiplication using Strassen's algorithm
    vector<vector<int>> multiply(vector<vector<int>>& A, vector<vector<int>>& B) {
        // Check if matrices are compatible for multiplication
        if (A[0].size() != B.size()) {
            cout << "Error: Matrix dimensions are not compatible for multiplication!" << endl;
            return {};
        }
        
        return strassenMultiply(A, B);
    }
    
    // Utility function to print matrix
    void printMatrix(const vector<vector<int>>& matrix) {
        for (const auto& row : matrix) {
            for (int element : row) {
                cout << element << " ";
            }
            cout << endl;
        }
        cout << endl;
    }
};

// Example usage
int main() {
    // Create two 4x4 matrices
    vector<vector<int>> A = {
        {1, 2, 3, 4},
        {5, 6, 7, 8},
        {9, 10, 11, 12},
        {13, 14, 15, 16}
    };
    
    vector<vector<int>> B = {
        {1, 0, 0, 0},
        {0, 1, 0, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 1}
    };
    
    cout << "Matrix A:" << endl;
    StrassenMatrix strassen;
    strassen.printMatrix(A);
    
    cout << "Matrix B:" << endl;
    strassen.printMatrix(B);
    
    // Perform matrix multiplication using Strassen's algorithm
    vector<vector<int>> result = strassen.multiply(A, B);
    
    cout << "Result of A × B using Strassen's algorithm:" << endl;
    strassen.printMatrix(result);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Recursive Approach**: Uses the divide-and-conquer principle of Strassen's algorithm
2. **Seven Multiplications**: Implements the seven matrix multiplications required by Strassen's method
3. **Base Case Handling**: Handles 1x1 matrices as base cases
4. **Padding Support**: Automatically handles odd-sized matrices by padding with zeros
5. **Submatrix Operations**: Efficiently copies and fills submatrices
6. **Error Handling**: Checks for compatible matrix dimensions

## Time Complexity:
- **Strassen's Algorithm**: O(n^2.807)
- **Traditional Method**: O(n^3)
- **Space Complexity**: O(n^2)

## Advantages:
- More efficient for large matrices
- Reduces the number of multiplications from 8 to 7

## Disadvantages:
- Higher constant factors make it less efficient for small matrices
- More complex implementation
- Requires additional memory for temporary matrices

