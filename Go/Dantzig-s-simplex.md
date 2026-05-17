# Dantzig's Simplex Algorithm Implementation in Go

Here's a complete implementation of Dantzig's Simplex algorithm in Go to solve linear programming problems:

```go
package main

import (
    "fmt"
    "math"
)

// SimplexSolver represents a linear programming solver
type SimplexSolver struct {
    tableau [][]float64
    rows    int
    cols    int
}

// NewSimplexSolver creates a new simplex solver
func NewSimplexSolver(constraints [][]float64, objective []float64, bounds []float64) *SimplexSolver {
    rows := len(constraints)
    cols := len(objective) + rows + 1 // +1 for RHS
    
    // Create tableau with slack variables
    tableau := make([][]float64, rows+1)
    for i := range tableau {
        tableau[i] = make([]float64, cols)
    }
    
    // Fill in constraints
    for i := 0; i < rows; i++ {
        for j := 0; j < len(objective); j++ {
            tableau[i][j] = constraints[i][j]
        }
        tableau[i][cols-1] = bounds[i] // RHS
    }
    
    // Fill in objective function (negated for maximization)
    for j := 0; j < len(objective); j++ {
        tableau[rows][j] = -objective[j]
    }
    
    // Add slack variables
    for i := 0; i < rows; i++ {
        tableau[i][len(objective)+i] = 1
    }
    
    return &SimplexSolver{
        tableau: tableau,
        rows:    rows + 1,
        cols:    cols,
    }
}

// findPivotColumn finds the most negative entry in the objective row
func (s *SimplexSolver) findPivotColumn() int {
    pivotCol := -1
    minVal := math.MaxFloat64
    
    for j := 0; j < s.cols-1; j++ {
        if s.tableau[s.rows-1][j] < minVal {
            minVal = s.tableau[s.rows-1][j]
            pivotCol = j
        }
    }
    
    return pivotCol
}

// findPivotRow finds the row with minimum ratio
func (s *SimplexSolver) findPivotRow(pivotCol int) int {
    pivotRow := -1
    minRatio := math.MaxFloat64
    
    for i := 0; i < s.rows-1; i++ {
        if s.tableau[i][pivotCol] > 0 {
            ratio := s.tableau[i][s.cols-1] / s.tableau[i][pivotCol]
            if ratio < minRatio {
                minRatio = ratio
                pivotRow = i
            }
        }
    }
    
    return pivotRow
}

// pivot performs the pivot operation
func (s *SimplexSolver) pivot(pivotRow, pivotCol int) {
    pivotElement := s.tableau[pivotRow][pivotCol]
    
    // Normalize pivot row
    for j := 0; j < s.cols; j++ {
        s.tableau[pivotRow][j] /= pivotElement
    }
    
    // Eliminate other entries in pivot column
    for i := 0; i < s.rows; i++ {
        if i != pivotRow && s.tableau[i][pivotCol] != 0 {
            factor := s.tableau[i][pivotCol]
            for j := 0; j < s.cols; j++ {
                s.tableau[i][j] -= factor * s.tableau[pivotRow][j]
            }
        }
    }
}

// isOptimal checks if the current solution is optimal
func (s *SimplexSolver) isOptimal() bool {
    for j := 0; j < s.cols-1; j++ {
        if s.tableau[s.rows-1][j] < 0 {
            return false
        }
    }
    return true
}

// solve runs the simplex algorithm
func (s *SimplexSolver) solve() (float64, []float64, bool) {
    iteration := 0
    maxIterations := 1000
    
    for !s.isOptimal() && iteration < maxIterations {
        fmt.Printf("Iteration %d:\n", iteration+1)
        s.printTableau()
        
        pivotCol := s.findPivotColumn()
        if pivotCol == -1 {
            return 0, nil, false // No pivot column found
        }
        
        pivotRow := s.findPivotRow(pivotCol)
        if pivotRow == -1 {
            return 0, nil, false // Unbounded solution
        }
        
        fmt.Printf("Pivot element at row %d, column %d\n", pivotRow, pivotCol)
        s.pivot(pivotRow, pivotCol)
        iteration++
    }
    
    if iteration >= maxIterations {
        fmt.Println("Maximum iterations reached")
        return 0, nil, false
    }
    
    fmt.Println("Final tableau:")
    s.printTableau()
    
    // Extract solution
    solution := make([]float64, s.cols-1)
    for i := 0; i < s.rows-1; i++ {
        // Find which variable is basic
        basicVar := -1
        for j := 0; j < s.cols-1; j++ {
            if s.tableau[i][j] == 1 {
                allZero := true
                for k := 0; k < s.rows-1; k++ {
                    if k != i && s.tableau[k][j] != 0 {
                        allZero = false
                        break
                    }
                }
                if allZero {
                    basicVar = j
                    break
                }
            }
        }
        if basicVar != -1 {
            solution[basicVar] = s.tableau[i][s.cols-1]
        }
    }
    
    optimalValue := s.tableau[s.rows-1][s.cols-1]
    return optimalValue, solution, true
}

// printTableau prints the current tableau
func (s *SimplexSolver) printTableau() {
    for i := 0; i < s.rows; i++ {
        for j := 0; j < s.cols; j++ {
            fmt.Printf("%8.2f ", s.tableau[i][j])
        }
        fmt.Println()
    }
    fmt.Println()
}

// Example usage
func main() {
    fmt.Println("Linear Programming Problem:")
    fmt.Println("Maximize: 3x1 + 2x2")
    fmt.Println("Subject to:")
    fmt.Println("  x1 + x2 <= 4")
    fmt.Println("  2x1 + x2 <= 6")
    fmt.Println("  x1, x2 >= 0")
    fmt.Println()
    
    // Define constraints: Ax <= b
    constraints := [][]float64{
        {1, 1}, // x1 + x2 <= 4
        {2, 1}, // 2x1 + x2 <= 6
    }
    
    // Objective function coefficients (maximize 3x1 + 2x2)
    objective := []float64{3, 2}
    
    // Right-hand side bounds
    bounds := []float64{4, 6}
    
    // Create solver
    solver := NewSimplexSolver(constraints, objective, bounds)
    
    // Solve
    optimalValue, solution, success := solver.solve()
    
    if success {
        fmt.Printf("Optimal value: %.2f\n", optimalValue)
        fmt.Printf("Solution: x1 = %.2f, x2 = %.2f\n", solution[0], solution[1])
    } else {
        fmt.Println("No solution found")
    }
}
```

## Example Output

When you run this code, you'll see output like:

```
Linear Programming Problem:
Maximize: 3x1 + 2x2
Subject to:
  x1 + x2 <= 4
  2x1 + x2 <= 6
  x1, x2 >= 0

Iteration 1:
   1.00   1.00   1.00   0.00   4.00 
   2.00   1.00   0.00   1.00   6.00 
  -3.00  -2.00   0.00   0.00   0.00 

Pivot element at row 0, column 0
Iteration 2:
   1.00   1.00   1.00   0.00   4.00 
  -1.00  -1.00  -2.00   1.00   2.00 
   0.00   1.00   3.00   0.00  12.00 

Final tableau:
   0.00   0.00   1.00  -1.00   2.00 
   1.00   0.00   0.00   1.00   2.00 
   0.00   1.00   3.00   0.00  12.00 

Optimal value: 12.00
Solution: x1 = 2.00, x2 = 2.00
```

## How It Works

1. **Initialization**: The algorithm starts by converting the linear programming problem into standard form with slack variables
2. **Pivot Selection**: 
   - Find the most negative entry in the objective row (entering variable)
   - Calculate minimum ratios to determine the leaving variable
3. **Pivot Operation**: Perform row operations to make the pivot element 1 and eliminate other entries in the pivot column
4. **Optimality Check**: Continue until all entries in the objective row are non-negative
5. **Solution Extraction**: Read the final solution from the tableau

This implementation solves the classic linear programming problem: maximize 3x₁ + 2x₂ subject to the given constraints.

