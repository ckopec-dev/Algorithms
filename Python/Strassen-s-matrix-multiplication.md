# Strassen's Matrix Multiplication Algorithm

Strassen's algorithm is a divide-and-conquer approach to matrix multiplication that reduces the time complexity from O(n³) to approximately O(n^2.807).

```python
def strassen_multiply(A, B):
    """
    Strassen's matrix multiplication algorithm
    """
    # Base case: if matrices are 1x1
    if len(A) == 1:
        return [[A[0][0] * B[0][0]]]
    
    # Ensure matrices are square and have dimensions that are powers of 2
    n = len(A)
    if n % 2 == 1:
        # Pad matrices with zeros to make them even-sized
        A = pad_matrix(A)
        B = pad_matrix(B)
        n = len(A)
    
    # Divide matrices into quadrants
    mid = n // 2
    
    # Split matrices into quadrants
    A11 = [row[:mid] for row in A[:mid]]
    A12 = [row[mid:] for row in A[:mid]]
    A21 = [row[:mid] for row in A[mid:]]
    A22 = [row[mid:] for row in A[mid:]]
    
    B11 = [row[:mid] for row in B[:mid]]
    B12 = [row[mid:] for row in B[:mid]]
    B21 = [row[:mid] for row in B[mid:]]
    B22 = [row[mid:] for row in B[mid:]]
    
    # Calculate Strassen's seven products
    M1 = strassen_multiply(matrix_add(A11, A22), matrix_add(B11, B22))
    M2 = strassen_multiply(matrix_add(A21, A22), B11)
    M3 = strassen_multiply(A11, matrix_subtract(B12, B22))
    M4 = strassen_multiply(A22, matrix_subtract(B21, B11))
    M5 = strassen_multiply(matrix_add(A11, A12), B22)
    M6 = strassen_multiply(matrix_subtract(A21, A11), matrix_add(B11, B12))
    M7 = strassen_multiply(matrix_subtract(A12, A22), matrix_add(B21, B22))
    
    # Calculate the quadrants of the result matrix
    C11 = matrix_add(matrix_subtract(matrix_add(M1, M4), M5), M7)
    C12 = matrix_add(M3, M5)
    C21 = matrix_add(M2, M4)
    C22 = matrix_add(matrix_subtract(matrix_add(M1, M3), M2), M6)
    
    # Combine quadrants into result matrix
    result = []
    for i in range(mid):
        result.append(C11[i] + C12[i])
    for i in range(mid):
        result.append(C21[i] + C22[i])
    
    return result

def matrix_add(A, B):
    """Add two matrices"""
    return [[A[i][j] + B[i][j] for j in range(len(A[0]))] for i in range(len(A))]

def matrix_subtract(A, B):
    """Subtract two matrices"""
    return [[A[i][j] - B[i][j] for j in range(len(A[0]))] for i in range(len(A))]

def pad_matrix(matrix):
    """Pad matrix with zeros to make it even-sized"""
    n = len(matrix)
    padded = [[0 for _ in range(n + 1)] for _ in range(n + 1)]
    
    for i in range(n):
        for j in range(n):
            padded[i][j] = matrix[i][j]
    
    return padded

# Example usage
if __name__ == "__main__":
    # Example 2x2 matrices
    A = [[1, 2],
         [3, 4]]
    
    B = [[5, 6],
         [7, 8]]
    
    print("Matrix A:")
    for row in A:
        print(row)
    
    print("\nMatrix B:")
    for row in B:
        print(row)
    
    # Perform Strassen multiplication
    result = strassen_multiply(A, B)
    
    print("\nResult of A × B:")
    for row in result:
        print(row)
    
    # Verify with standard multiplication
    expected = [[19, 22],
                [43, 50]]
    
    print("\nExpected result:")
    for row in expected:
        print(row)
    
    print(f"\nResults match: {result == expected}")
```

## Output:
```
Matrix A:
[1, 2]
[3, 4]

Matrix B:
[5, 6]
[7, 8]

Result of A × B:
[19, 22]
[43, 50]

Expected result:
[19, 22]
[43, 50]

Results match: True
```

## Key Features:

1. **Divide and Conquer**: Splits matrices into quadrants recursively
2. **Seven Multiplications**: Uses only 7 multiplications instead of 8 (as in standard method)
3. **Base Case**: Handles 1x1 matrices directly
4. **Padding**: Automatically pads matrices to ensure even dimensions
5. **Recursive**: Calls itself on smaller submatrices

## Time Complexity:
- **Strassen's**: O(n^2.807)
- **Standard**: O(n³)
- **Space Complexity**: O(n²)

This implementation demonstrates the core principles of Strassen's algorithm while handling edge cases and providing a clean, readable solution.

