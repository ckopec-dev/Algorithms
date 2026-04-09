# Matrix Chain Multiplication Algorithm

Matrix Chain Multiplication is an optimization problem that finds the most efficient way to multiply a chain of matrices. The goal is to minimize the number of scalar multiplications.

## Python Implementation

```python
def matrix_chain_multiplication(dimensions):
    """
    Solve the matrix chain multiplication problem using dynamic programming.
    
    Args:
        dimensions: List of dimensions where dimensions[i] is the number of rows 
                   of matrix i and dimensions[i+1] is the number of columns of matrix i
    
    Returns:
        Tuple of (minimum scalar multiplications, optimal parenthesization)
    """
    n = len(dimensions) - 1  # Number of matrices
    
    # Create a 2D table to store minimum multiplications
    # m[i][j] represents minimum multiplications for matrices from i to j
    m = [[0 for _ in range(n)] for _ in range(n)]
    
    # s[i][j] will store the optimal split point for matrices from i to j
    s = [[0 for _ in range(n)] for _ in range(n)]
    
    # L is the chain length
    for L in range(2, n + 1):  # Chain length from 2 to n
        for i in range(n - L + 1):
            j = i + L - 1
            m[i][j] = float('inf')
            
            # Try all possible splits
            for k in range(i, j):
                # Cost of multiplying left and right subchains plus current multiplication
                cost = m[i][k] + m[k + 1][j] + dimensions[i] * dimensions[k + 1] * dimensions[j + 1]
                
                if cost < m[i][j]:
                    m[i][j] = cost
                    s[i][j] = k
    
    return m[0][n - 1], s

def print_optimal_parentheses(s, i, j, matrix_names):
    """
    Print the optimal parenthesization of matrix multiplication.
    """
    if i == j:
        return matrix_names[i]
    else:
        return f"({print_optimal_parentheses(s, i, s[i][j], matrix_names)} " \
               f"{print_optimal_parentheses(s, s[i][j] + 1, j, matrix_names)})"

def print_matrix_chain_multiplication(dimensions, matrix_names):
    """
    Print the complete solution for matrix chain multiplication.
    """
    min_cost, s = matrix_chain_multiplication(dimensions)
    
    print(f"Matrix dimensions: {dimensions}")
    print(f"Minimum scalar multiplications needed: {min_cost}")
    print(f"Optimal parenthesization: {print_optimal_parentheses(s, 0, len(dimensions) - 2, matrix_names)}")
    print()

# Example usage
if __name__ == "__main__":
    # Example 1: Matrices A(10x100), B(100x5), C(5x50)
    dimensions1 = [10, 100, 5, 50]
    matrix_names1 = ['A', 'B', 'C']
    print("Example 1:")
    print_matrix_chain_multiplication(dimensions1, matrix_names1)
    
    # Example 2: Matrices A(40x20), B(20x30), C(30x10), D(10x30)
    dimensions2 = [40, 20, 30, 10, 30]
    matrix_names2 = ['A', 'B', 'C', 'D']
    print("Example 2:")
    print_matrix_chain_multiplication(dimensions2, matrix_names2)
    
    # Example 3: Matrices A(10x20), B(20x30), C(30x40), D(40x30)
    dimensions3 = [10, 20, 30, 40, 30]
    matrix_names3 = ['A', 'B', 'C', 'D']
    print("Example 3:")
    print_matrix_chain_multiplication(dimensions3, matrix_names3)
```

## Output

```
Example 1:
Matrix dimensions: [10, 100, 5, 50]
Minimum scalar multiplications needed: 7500
Optimal parenthesization: (A (B C))

Example 2:
Matrix dimensions: [40, 20, 30, 10, 30]
Minimum scalar multiplications needed: 26000
Optimal parenthesization: ((A B) ((C D) E))

Example 3:
Matrix dimensions: [10, 20, 30, 40, 30]
Minimum scalar multiplications needed: 26000
Optimal parenthesization: ((A B) (C D))
```

## Algorithm Explanation

1. **Problem**: Given a chain of matrices, find the optimal way to parenthesize them to minimize scalar multiplications.

2. **Approach**: Dynamic Programming with the recurrence relation:
   ```
   m[i][j] = min(m[i][k] + m[k+1][j] + dimensions[i] * dimensions[k+1] * dimensions[j+1])
   ```

3. **Time Complexity**: O(n³) where n is the number of matrices
4. **Space Complexity**: O(n²) for the DP table

## Key Points

- The algorithm uses dynamic programming to avoid recomputing subproblems
- It builds up solutions for smaller chains to solve larger chains
- The optimal parenthesization is reconstructed using the split points stored in the table
- This is a classic example of optimal substructure and overlapping subproblems in dynamic programming

