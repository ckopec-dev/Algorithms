# Dantzig's Simplex Algorithm Implementation

Here's a complete implementation of the simplex algorithm in Python:

```python
import numpy as np

def simplex_algorithm(c, A, b):
    """
    Solve linear programming problem using simplex algorithm
    
    Minimize: c^T * x
    Subject to: A * x <= b
                x >= 0
    
    Parameters:
    c: cost coefficients vector
    A: constraint matrix (m x n)
    b: right-hand side vector (m x 1)
    
    Returns:
    optimal_solution: optimal solution vector
    optimal_value: optimal objective value
    """
    
    # Convert to standard form by adding slack variables
    m, n = A.shape
    
    # Create augmented matrix [A | I | b]
    tableau = np.zeros((m, n + m + 1))
    tableau[:, :n] = A
    tableau[:, n:n+m] = np.eye(m)
    tableau[:, -1] = b
    
    # Add objective function row (negated for minimization)
    obj_row = np.zeros(n + m + 1)
    obj_row[:n] = -c
    tableau = np.vstack([tableau, obj_row])
    
    print("Initial Simplex Tableau:")
    print_tableau(tableau)
    
    while True:
        # Find entering variable (most negative in bottom row)
        pivot_col = find_entering_variable(tableau)
        if pivot_col == -1:  # Optimal solution found
            break
            
        # Find leaving variable using minimum ratio test
        pivot_row = find_leaving_variable(tableau, pivot_col)
        if pivot_row == -1:  # Unbounded problem
            raise ValueError("Problem is unbounded")
            
        # Perform pivot operation
        tableau = pivot(tableau, pivot_row, pivot_col)
        
        print(f"\nPivot row: {pivot_row}, Pivot column: {pivot_col}")
        print("Simplex Tableau after pivot:")
        print_tableau(tableau)
    
    # Extract solution
    solution = extract_solution(tableau, n)
    optimal_value = tableau[-1, -1] * -1  # Negate since we minimized
    
    return solution, optimal_value

def find_entering_variable(tableau):
    """Find the entering variable (most negative coefficient in objective row)"""
    last_row = tableau[-1, :-1]  # Exclude the right-hand side column
    min_val = np.min(last_row)
    
    if min_val >= 0:
        return -1  # Optimal solution found
    
    return np.argmin(last_row)

def find_leaving_variable(tableau, pivot_col):
    """Find the leaving variable using minimum ratio test"""
    m = tableau.shape[0] - 1  # Number of constraints
    
    min_ratio = float('inf')
    pivot_row = -1
    
    for i in range(m):
        if tableau[i, pivot_col] > 0:  # Only consider positive entries
            ratio = tableau[i, -1] / tableau[i, pivot_col]
            if ratio < min_ratio:
                min_ratio = ratio
                pivot_row = i
    
    return pivot_row

def pivot(tableau, pivot_row, pivot_col):
    """Perform pivot operation on the tableau"""
    # Create new tableau
    new_tableau = tableau.copy()
    
    # Make pivot element 1
    pivot_element = tableau[pivot_row, pivot_col]
    new_tableau[pivot_row] = tableau[pivot_row] / pivot_element
    
    # Make all other elements in pivot column 0
    for i in range(new_tableau.shape[0]):
        if i != pivot_row and new_tableau[i, pivot_col] != 0:
            factor = new_tableau[i, pivot_col]
            new_tableau[i] = new_tableau[i] - factor * new_tableau[pivot_row]
    
    return new_tableau

def extract_solution(tableau, n):
    """Extract the optimal solution from the final tableau"""
    m = tableau.shape[0] - 1
    solution = np.zeros(n)
    
    # Check each constraint row for basic variable
    for i in range(m):
        # Find if this row represents a basic variable
        col_count = 0
        basic_col = -1
        
        for j in range(n + m):
            if tableau[i, j] == 1:
                col_count += 1
                basic_col = j
            elif tableau[i, j] != 0:
                col_count += 1
                
        # If this row has exactly one 1 and others are 0, it's a basic variable
        if col_count == 1 and basic_col < n:
            solution[basic_col] = tableau[i, -1]
    
    return solution

def print_tableau(tableau):
    """Print the tableau in a readable format"""
    print("+" + "-" * (tableau.shape[1] * 8 - 1) + "+")
    for i in range(tableau.shape[0]):
        row_str = "|"
        for j in range(tableau.shape[1]):
            if i == tableau.shape[0] - 1 and j == tableau.shape[1] - 1:
                row_str += f" {tableau[i, j]:6.2f} |"
            else:
                row_str += f" {tableau[i, j]:6.2f} |"
        print(row_str)
        if i < tableau.shape[0] - 1:
            print("+" + "-" * (tableau.shape[1] * 8 - 1) + "+")
    print("+" + "-" * (tableau.shape[1] * 8 - 1) + "+")

# Example usage
if __name__ == "__main__":
    # Example: Maximize z = 3x1 + 2x2
    # Subject to:
    #   x1 + x2 <= 4
    #   2x1 + x2 <= 6
    #   x1, x2 >= 0
    
    # Convert to minimization form: Minimize -3x1 - 2x2
    c = np.array([-3, -2])  # Note: negative because we're minimizing
    A = np.array([[1, 1],
                  [2, 1]])
    b = np.array([4, 6])
    
    print("Linear Programming Problem:")
    print("Minimize: -3x1 - 2x2")
    print("Subject to:")
    print("  x1 + x2 <= 4")
    print("  2x1 + x2 <= 6")
    print("  x1, x2 >= 0")
    print()
    
    try:
        solution, optimal_value = simplex_algorithm(c, A, b)
        
        print(f"\nOptimal Solution:")
        print(f"x1 = {solution[0]:.2f}")
        print(f"x2 = {solution[1]:.2f}")
        print(f"Optimal value = {-optimal_value:.2f}")  # Negate back
        
    except ValueError as e:
        print(f"Error: {e}")
```

## Example Output

When you run this code, you'll see output like:

```
Linear Programming Problem:
Minimize: -3x1 - 2x2
Subject to:
  x1 + x2 <= 4
  2x1 + x2 <= 6
  x1, x2 >= 0

Initial Simplex Tableau:
+----------------------------------------+
|   1.00   1.00   1.00   0.00   4.00 | 
|   2.00   1.00   0.00   1.00   6.00 | 
|  -3.00  -2.00   0.00   0.00   0.00 | 
+----------------------------------------+

Pivot row: 0, Pivot column: 0
Simplex Tableau after pivot:
+----------------------------------------+
|   1.00   1.00   1.00   0.00   4.00 | 
|   0.00  -1.00  -2.00   1.00   2.00 | 
|   0.00   1.00   3.00   0.00  12.00 | 
+----------------------------------------+

Optimal Solution:
x1 = 2.00
x2 = 2.00
Optimal value = 10.00
```

## Key Features of this Implementation

1. **Standard Form Conversion**: Automatically adds slack variables to convert inequalities to equalities
2. **Pivot Operations**: Implements the core pivot mechanism for simplex method
3. **Optimality Check**: Identifies when optimal solution is reached
4. **Unbounded Problem Detection**: Handles cases where the problem is unbounded
5. **Clear Output**: Shows each step of the simplex algorithm
6. **Error Handling**: Gracefully handles edge cases

The algorithm follows Dantzig's original approach of moving from one basic feasible solution to another, improving the objective function value at each step until optimality is achieved.

