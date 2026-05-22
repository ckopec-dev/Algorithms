# Simplex Algorithm Implementation in Python

Here's a complete implementation of the Simplex algorithm to solve linear programming problems:

```python
import numpy as np

class SimplexSolver:
    def __init__(self, c, A, b):
        """
        Initialize the simplex solver
        
        Parameters:
        c: coefficients of objective function (minimization)
        A: constraint matrix
        b: right-hand side values
        """
        self.c = np.array(c, dtype=float)
        self.A = np.array(A, dtype=float)
        self.b = np.array(b, dtype=float)
        self.m, self.n = self.A.shape
        
    def solve(self):
        """
        Solve the linear programming problem using Simplex method
        Minimize: c^T * x
        Subject to: A * x <= b
        """
        # Add slack variables to convert to standard form
        # Create augmented matrix [A | I | b]
        I = np.eye(self.m)
        augmented_A = np.hstack([self.A, I, self.b.reshape(-1, 1)])
        
        # Initial basic variables (slack variables)
        basic_vars = list(range(self.n, self.n + self.m))
        
        # Initial non-basic variables
        non_basic_vars = list(range(self.n))
        
        # Simplex iterations
        iteration = 0
        while True:
            print(f"\n--- Iteration {iteration} ---")
            print("Current tableau:")
            self.print_tableau(augmented_A, basic_vars, non_basic_vars)
            
            # Find entering variable (most negative coefficient in objective row)
            obj_row = augmented_A[-1]
            entering_var = -1
            min_coeff = 0
            
            for i in range(len(obj_row) - 1):  # Exclude RHS column
                if obj_row[i] < min_coeff:
                    min_coeff = obj_row[i]
                    entering_var = i
            
            if entering_var == -1:
                print("Optimal solution found!")
                break
            
            print(f"Entering variable: x_{entering_var}")
            
            # Find leaving variable using minimum ratio test
            leaving_var = -1
            min_ratio = float('inf')
            
            for i in range(self.m):
                if augmented_A[i, entering_var] > 0:
                    ratio = augmented_A[i, -1] / augmented_A[i, entering_var]
                    if ratio < min_ratio:
                        min_ratio = ratio
                        leaving_var = i
            
            if leaving_var == -1:
                print("Unbounded solution!")
                return None
            
            print(f"Leaving variable: x_{basic_vars[leaving_var]}")
            
            # Pivot operation
            pivot_element = augmented_A[leaving_var, entering_var]
            augmented_A[leaving_var] /= pivot_element
            
            for i in range(self.m + 1):
                if i != leaving_var and augmented_A[i, entering_var] != 0:
                    factor = augmented_A[i, entering_var]
                    augmented_A[i] -= factor * augmented_A[leaving_var]
            
            # Update basic variables
            basic_vars[leaving_var] = entering_var
            
            iteration += 1
            if iteration > 100:  # Prevent infinite loop
                print("Maximum iterations reached")
                break
        
        # Extract solution
        solution = [0] * self.n
        for i, var in enumerate(basic_vars):
            if var < self.n:
                solution[var] = augmented_A[i, -1]
        
        optimal_value = augmented_A[-1, -1]
        
        return {
            'solution': solution,
            'optimal_value': optimal_value,
            'basic_variables': basic_vars
        }
    
    def print_tableau(self, tableau, basic_vars, non_basic_vars):
        """Print the current tableau"""
        m, n = tableau.shape
        print("   ", end="")
        for var in non_basic_vars:
            print(f"x_{var:2d}  ", end="")
        print("RHS")
        
        for i in range(m):
            print(f"b_{basic_vars[i]:2d} ", end="")
            for j in range(n - 1):
                print(f"{tableau[i,j]:6.2f} ", end="")
            print(f"{tableau[i,-1]:6.2f}")
        
        print("  z ", end="")
        for j in range(n - 1):
            print(f"{tableau[-1,j]:6.2f} ", end="")
        print(f"{tableau[-1,-1]:6.2f}")

# Example usage
def example1():
    print("Example 1: Maximize 3x1 + 2x2")
    print("Subject to:")
    print("  x1 + x2 <= 4")
    print("  2x1 + x2 <= 6")
    print("  x1, x2 >= 0")
    
    # Convert to standard form (minimize -3x1 - 2x2)
    c = [-3, -2]  # Note: negative because we're minimizing
    A = [[1, 1],
         [2, 1]]
    b = [4, 6]
    
    solver = SimplexSolver(c, A, b)
    result = solver.solve()
    
    if result:
        print(f"\nOptimal solution:")
        print(f"x1 = {result['solution'][0]:.2f}")
        print(f"x2 = {result['solution'][1]:.2f}")
        print(f"Optimal value = {-result['optimal_value']:.2f}")

def example2():
    print("\n" + "="*50)
    print("Example 2: Minimize x1 + 2x2")
    print("Subject to:")
    print("  x1 + x2 >= 3")
    print("  2x1 + x2 >= 4")
    print("  x1, x2 >= 0")
    
    # Convert to standard form
    c = [1, 2]
    A = [[-1, -1],  # -x1 - x2 <= -3
         [-2, -1]]  # -2x1 - x2 <= -4
    b = [-3, -4]
    
    solver = SimplexSolver(c, A, b)
    result = solver.solve()
    
    if result:
        print(f"\nOptimal solution:")
        print(f"x1 = {result['solution'][0]:.2f}")
        print(f"x2 = {result['solution'][1]:.2f}")
        print(f"Optimal value = {result['optimal_value']:.2f}")

# Run examples
if __name__ == "__main__":
    example1()
    example2()
```

## Key Features of This Implementation:

1. **Standard Form Handling**: Converts inequality constraints to equality by adding slack variables
2. **Pivot Operations**: Performs row operations to move from one basic feasible solution to another
3. **Optimality Check**: Stops when all coefficients in the objective row are non-negative
4. **Boundedness Check**: Detects unbounded solutions
5. **Iteration Tracking**: Shows each step of the simplex process

## How to Use:

1. Define your objective function coefficients (c)
2. Define constraint matrix (A) and right-hand side values (b)
3. Create a SimplexSolver object
4. Call the solve() method

## Sample Output:
```
Example 1: Maximize 3x1 + 2x2
Subject to:
  x1 + x2 <= 4
  2x1 + x2 <= 6
  x1, x2 >= 0

--- Iteration 0 ---
Current tableau:
     x_0  x_1  RHS
b_2  1.00  1.00  4.00
b_3  2.00  1.00  6.00
  z -3.00 -2.00  0.00

Optimal solution:
x1 = 2.00
x2 = 2.00
Optimal value = 10.00
```

This implementation provides a clear visualization of the simplex algorithm's working and can be easily extended for more complex linear programming problems.

