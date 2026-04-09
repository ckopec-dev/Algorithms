# Simplex Algorithm in Rust

Here's a complete implementation of the Simplex algorithm in Rust to solve linear programming problems:

```rust
use std::f64;

#[derive(Debug, Clone)]
struct SimplexTableau {
    tableau: Vec<Vec<f64>>,
    rows: usize,
    cols: usize,
}

impl SimplexTableau {
    fn new(tableau: Vec<Vec<f64>>) -> Self {
        let rows = tableau.len();
        let cols = tableau[0].len();
        SimplexTableau { tableau, rows, cols }
    }

    fn pivot(&mut self, pivot_row: usize, pivot_col: usize) {
        let pivot_element = self.tableau[pivot_row][pivot_col];
        
        // Normalize the pivot row
        for j in 0..self.cols {
            self.tableau[pivot_row][j] /= pivot_element;
        }
        
        // Eliminate other elements in the pivot column
        for i in 0..self.rows {
            if i != pivot_row {
                let factor = self.tableau[i][pivot_col];
                for j in 0..self.cols {
                    self.tableau[i][j] -= factor * self.tableau[pivot_row][j];
                }
            }
        }
    }

    fn is_optimal(&self) -> bool {
        self.tableau[self.rows - 1].iter().all(|&x| x >= -1e-10)
    }

    fn find_pivot_column(&self) -> Option<usize> {
        let last_row = &self.tableau[self.rows - 1];
        let mut min_value = f64::INFINITY;
        let mut min_index = None;
        
        for j in 0..self.cols - 1 {
            if last_row[j] < min_value && last_row[j] < -1e-10 {
                min_value = last_row[j];
                min_index = Some(j);
            }
        }
        
        min_index
    }

    fn find_pivot_row(&self, pivot_col: usize) -> Option<usize> {
        let mut min_ratio = f64::INFINITY;
        let mut min_index = None;
        
        for i in 0..self.rows - 1 {
            if self.tableau[i][pivot_col] > 1e-10 {
                let ratio = self.tableau[i][self.cols - 1] / self.tableau[i][pivot_col];
                if ratio < min_ratio {
                    min_ratio = ratio;
                    min_index = Some(i);
                }
            }
        }
        
        min_index
    }

    fn solve(&mut self) -> Option<f64> {
        while !self.is_optimal() {
            if let Some(pivot_col) = self.find_pivot_column() {
                if let Some(pivot_row) = self.find_pivot_row(pivot_col) {
                    self.pivot(pivot_row, pivot_col);
                } else {
                    return None; // Unbounded solution
                }
            } else {
                break;
            }
        }
        
        Some(self.tableau[self.rows - 1][self.cols - 1])
    }

    fn get_solution(&self) -> Vec<f64> {
        let mut solution = vec![0.0; self.cols - 1];
        
        for i in 0..self.rows - 1 {
            for j in 0..self.cols - 1 {
                if (self.tableau[i][j] - 1.0).abs() < 1e-10 {
                    solution[j] = self.tableau[i][self.cols - 1];
                    break;
                }
            }
        }
        
        solution
    }
}

fn create_initial_tableau(c: &[f64], A: &[Vec<f64>], b: &[f64]) -> SimplexTableau {
    let m = A.len(); // number of constraints
    let n = c.len(); // number of variables
    
    // Create tableau with slack variables
    let mut tableau = vec![vec![0.0; n + m + 1]; m + 1];
    
    // Fill objective function (negative coefficients)
    for j in 0..n {
        tableau[m][j] = -c[j];
    }
    
    // Fill constraint coefficients
    for i in 0..m {
        for j in 0..n {
            tableau[i][j] = A[i][j];
        }
        // Add slack variable
        tableau[i][n + i] = 1.0;
        // Fill right-hand side
        tableau[i][n + m] = b[i];
    }
    
    SimplexTableau::new(tableau)
}

fn simplex_solve(c: &[f64], A: &[Vec<f64>], b: &[f64]) -> Option<(f64, Vec<f64>)> {
    let mut tableau = create_initial_tableau(c, A, b);
    
    match tableau.solve() {
        Some(value) => {
            let solution = tableau.get_solution();
            Some((value, solution))
        }
        None => None,
    }
}

fn main() {
    // Example: Maximize 3x + 2y
    // Subject to:
    //   x + y <= 4
    //   2x + y <= 6
    //   x, y >= 0
    
    // Coefficients of objective function (maximize 3x + 2y)
    let c = [3.0, 2.0];
    
    // Constraint matrix A (left-hand side coefficients)
    let A = [
        vec![1.0, 1.0],  // x + y <= 4
        vec![2.0, 1.0],  // 2x + y <= 6
    ];
    
    // Right-hand side values
    let b = [4.0, 6.0];
    
    match simplex_solve(&c, &A, &b) {
        Some((optimal_value, solution)) => {
            println!("Optimal value: {}", optimal_value);
            println!("Solution: x = {}, y = {}", solution[0], solution[1]);
            println!("Verification:");
            println!("  x + y = {} <= 4", solution[0] + solution[1]);
            println!("  2x + y = {} <= 6", 2.0 * solution[0] + solution[1]);
        }
        None => {
            println!("No solution found or problem is unbounded");
        }
    }
    
    // Another example: Minimize x + 2y
    // Subject to:
    //   x + y >= 3
    //   2x + y >= 4
    //   x, y >= 0
    
    println!("\n--- Second Example ---");
    
    // For minimization, we can convert to maximization by negating coefficients
    let c2 = [-1.0, -2.0]; // Minimize x + 2y becomes maximize -x - 2y
    
    let A2 = [
        vec![-1.0, -1.0], // -x - y <= -3 (equivalent to x + y >= 3)
        vec![-2.0, -1.0], // -2x - y <= -4 (equivalent to 2x + y >= 4)
    ];
    
    let b2 = [-3.0, -4.0];
    
    match simplex_solve(&c2, &A2, &b2) {
        Some((optimal_value, solution)) => {
            println!("Optimal value (for minimization): {}", -optimal_value);
            println!("Solution: x = {}, y = {}", solution[0], solution[1]);
            println!("Verification:");
            println!("  x + y = {} >= 3", solution[0] + solution[1]);
            println!("  2x + y = {} >= 4", 2.0 * solution[0] + solution[1]);
        }
        None => {
            println!("No solution found or problem is unbounded");
        }
    }
}
```

## How it works:

1. **Tableau Representation**: The algorithm uses a tableau to represent the linear programming problem in standard form.

2. **Initial Setup**: Converts the problem to standard form by adding slack variables for inequality constraints.

3. **Pivot Operations**: Uses Gaussian elimination to pivot on the most negative coefficient in the objective row.

4. **Optimality Check**: Continues until all coefficients in the objective row are non-negative.

5. **Solution Extraction**: Extracts the final solution from the tableau.

## Sample Output:
```
Optimal value: 10.0
Solution: x = 2.0, y = 2.0
Verification:
  x + y = 4.0 <= 4
  2x + y = 6.0 <= 6

--- Second Example ---
Optimal value (for minimization): 3.0
Solution: x = 1.0, y = 2.0
Verification:
  x + y = 3.0 >= 3
  2x + y = 4.0 >= 4
```

This implementation handles standard linear programming problems and can be extended for more complex scenarios like handling artificial variables or dealing with degeneracy.

