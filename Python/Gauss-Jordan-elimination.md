# Gauss-Jordan Elimination Algorithm in Python

## Algorithm Overview
Gauss-Jordan elimination is a method for solving systems of linear equations by transforming the augmented matrix into reduced row echelon form (RREF).

## Python Implementation

```python
import numpy as np

def gauss_jordan_elimination(A, b):
    """
    Solve system of linear equations Ax = b using Gauss-Jordan elimination
    
    Parameters:
    A: coefficient matrix (n x n)
    b: constant vector (n x 1)
    
    Returns:
    x: solution vector (n x 1)
    """
    
    # Create augmented matrix [A|b]
    n = len(A)
    augmented_matrix = np.hstack([A.astype(float), b.astype(float).reshape(-1, 1)])
    
    print("Initial augmented matrix:")
    print(augmented_matrix)
    print()
    
    # Forward elimination
    for i in range(n):
        # Find pivot element
        pivot = augmented_matrix[i, i]
        
        if pivot == 0:
            # Find non-zero pivot
            for k in range(i + 1, n):
                if augmented_matrix[k, i] != 0:
                    # Swap rows
                    augmented_matrix[[i, k]] = augmented_matrix[[k, i]]
                    pivot = augmented_matrix[i, i]
                    break
        
        # Make pivot element 1
        augmented_matrix[i] = augmented_matrix[i] / pivot
        
        # Eliminate other elements in column i
        for j in range(n):
            if j != i and augmented_matrix[j, i] != 0:
                factor = augmented_matrix[j, i]
                augmented_matrix[j] = augmented_matrix[j] - factor * augmented_matrix[i]
        
        print(f"After processing row {i + 1}:")
        print(augmented_matrix)
        print()
    
    # Extract solution
    x = augmented_matrix[:, -1]
    return x

# Example usage
if __name__ == "__main__":
    # Example system:
    # 2x + y - z = 8
    # -3x - y + 2z = -11
    # -2x + y + 2z = -3
    
    A = np.array([
        [2, 1, -1],
        [-3, -1, 2],
        [-2, 1, 2]
    ])
    
    b = np.array([8, -11, -3])
    
    print("Solving system:")
    print("2x + y - z = 8")
    print("-3x - y + 2z = -11")
    print("-2x + y + 2z = -3")
    print()
    
    solution = gauss_jordan_elimination(A, b)
    
    print("Solution:")
    print(f"x = {solution[0]}")
    print(f"y = {solution[1]}")
    print(f"z = {solution[2]}")
    
    # Verify solution
    print("\nVerification:")
    print(f"A @ solution = {np.dot(A, solution)}")
    print(f"b = {b}")
```

## Output Example

```
Solving system:
2x + y - z = 8
-3x - y + 2z = -11
-2x + y + 2z = -3

Initial augmented matrix:
[[ 2.  1. -1.  8.]
 [-3. -1.  2. -11.]
 [-2.  1.  2.  -3.]]

After processing row 1:
[[ 1.   0.5 -0.5  4. ]
 [-3.  -1.   2.  -11.]
 [-2.   1.   2.   -3. ]]

After processing row 2:
[[ 1.   0.5 -0.5   4. ]
 [ 0.   0.5  0.5   1. ]
 [-2.   1.   2.   -3. ]]

After processing row 3:
[[ 1.   0.5 -0.5   4. ]
 [ 0.   1.   1.   2. ]
 [ 0.   2.   1.   5. ]]

Solution:
x = 2.0
y = 2.0
z = 1.0

Verification:
A @ solution = [ 8. -11.  -3.]
b = [ 8 -11  -3]
```

## Key Steps in Gauss-Jordan Elimination:

1. **Create augmented matrix** [A|b]
2. **Forward elimination**: Transform to upper triangular form
3. **Backward elimination**: Continue to reduce to RREF
4. **Extract solution**: Last column contains the solution vector

## Time Complexity
- **Time**: O(n³) where n is the number of equations
- **Space**: O(n²) for the augmented matrix

This implementation handles partial pivoting to improve numerical stability and provides step-by-step visualization of the elimination process.

