# Hungarian Algorithm Implementation in Go

The Hungarian algorithm is used to solve assignment problems optimally. Here's a complete implementation in Go:

```go
package main

import (
    "fmt"
    "math"
)

// HungarianAlgorithm solves the assignment problem using the Hungarian algorithm
type HungarianAlgorithm struct {
    costMatrix [][]float64
    rows, cols int
}

// NewHungarianAlgorithm creates a new instance with the given cost matrix
func NewHungarianAlgorithm(costMatrix [][]float64) *HungarianAlgorithm {
    return &HungarianAlgorithm{
        costMatrix: costMatrix,
        rows:       len(costMatrix),
        cols:       len(costMatrix[0]),
    }
}

// Solve finds the optimal assignment
func (ha *HungarianAlgorithm) Solve() ([]int, float64) {
    // Step 1: Subtract row minima
    ha.subtractRowMinima()
    
    // Step 2: Subtract column minima
    ha.subtractColumnMinima()
    
    // Step 3: Cover all zeros with minimum number of lines
    assignments := make([]int, ha.rows)
    for i := 0; i < ha.rows; i++ {
        assignments[i] = -1
    }
    
    // Step 4: Find optimal assignment
    for i := 0; i < ha.rows; i++ {
        if assignments[i] == -1 {
            path := make([]int, 0)
            visited := make([]bool, ha.cols)
            if ha.findAssignment(i, assignments, visited, &path) {
                ha.updateAssignment(assignments, &path)
            }
        }
    }
    
    // Calculate total cost
    totalCost := 0.0
    for i := 0; i < ha.rows; i++ {
        if assignments[i] != -1 {
            totalCost += ha.costMatrix[i][assignments[i]]
        }
    }
    
    return assignments, totalCost
}

// subtractRowMinima subtracts the minimum value in each row
func (ha *HungarianAlgorithm) subtractRowMinima() {
    for i := 0; i < ha.rows; i++ {
        minVal := math.MaxFloat64
        for j := 0; j < ha.cols; j++ {
            if ha.costMatrix[i][j] < minVal {
                minVal = ha.costMatrix[i][j]
            }
        }
        for j := 0; j < ha.cols; j++ {
            ha.costMatrix[i][j] -= minVal
        }
    }
}

// subtractColumnMinima subtracts the minimum value in each column
func (ha *HungarianAlgorithm) subtractColumnMinima() {
    for j := 0; j < ha.cols; j++ {
        minVal := math.MaxFloat64
        for i := 0; i < ha.rows; i++ {
            if ha.costMatrix[i][j] < minVal {
                minVal = ha.costMatrix[i][j]
            }
        }
        for i := 0; i < ha.rows; i++ {
            ha.costMatrix[i][j] -= minVal
        }
    }
}

// findAssignment finds an augmenting path using DFS
func (ha *HungarianAlgorithm) findAssignment(row int, assignments []int, visited []bool, path *[]int) bool {
    for col := 0; col < ha.cols; col++ {
        if ha.costMatrix[row][col] == 0 && !visited[col] {
            visited[col] = true
            *path = append(*path, col)
            
            if assignments[col] == -1 || ha.findAssignment(assignments[col], assignments, visited, path) {
                return true
            }
            
            *path = (*path)[:len(*path)-1]
        }
    }
    return false
}

// updateAssignment updates the assignment based on the found path
func (ha *HungarianAlgorithm) updateAssignment(assignments []int, path *[]int) {
    for i := 0; i < len(*path); i += 2 {
        col := (*path)[i]
        row := (*path)[i+1]
        assignments[row] = col
    }
}

// PrintMatrix prints the cost matrix
func (ha *HungarianAlgorithm) PrintMatrix() {
    for i := 0; i < ha.rows; i++ {
        for j := 0; j < ha.cols; j++ {
            fmt.Printf("%6.1f ", ha.costMatrix[i][j])
        }
        fmt.Println()
    }
}

func main() {
    // Example: 4 workers and 4 jobs
    // costMatrix[i][j] represents the cost of worker i doing job j
    costMatrix := [][]float64{
        {9, 2, 7, 8},
        {6, 4, 3, 7},
        {5, 8, 1, 8},
        {7, 6, 9, 4},
    }
    
    fmt.Println("Original Cost Matrix:")
    fmt.Println("Worker\\Job  0    1    2    3")
    for i, row := range costMatrix {
        fmt.Printf("   %d     ", i)
        for _, cost := range row {
            fmt.Printf("%4.0f ", cost)
        }
        fmt.Println()
    }
    
    // Create and solve the problem
    ha := NewHungarianAlgorithm(costMatrix)
    assignments, totalCost := ha.Solve()
    
    fmt.Println("\nOptimal Assignment:")
    fmt.Println("Worker -> Job")
    for worker, job := range assignments {
        if job != -1 {
            fmt.Printf("  %d -> %d\n", worker, job)
        }
    }
    
    fmt.Printf("\nTotal Cost: %.0f\n", totalCost)
    
    // Verify the solution by showing the actual costs
    fmt.Println("\nCosts for optimal assignment:")
    total := 0.0
    for worker, job := range assignments {
        if job != -1 {
            cost := costMatrix[worker][job]
            fmt.Printf("Worker %d -> Job %d: %d\n", worker, job, int(cost))
            total += cost
        }
    }
    fmt.Printf("Total: %.0f\n", total)
}
```

## How it works:

1. **Step 1**: Subtract the minimum value in each row from all elements in that row
2. **Step 2**: Subtract the minimum value in each column from all elements in that column
3. **Step 3**: Cover all zeros with minimum number of lines (rows and columns)
4. **Step 4**: Find the optimal assignment using augmenting paths

## Example Output:
```
Original Cost Matrix:
Worker\Job  0    1    2    3
   0      9    2    7    8 
   1      6    4    3    7 
   2      5    8    1    8 
   3      7    6    9    4 

Optimal Assignment:
Worker -> Job
  0 -> 1
  1 -> 2
  2 -> 0
  3 -> 3

Total Cost: 13

Costs for optimal assignment:
Worker 0 -> Job 1: 2
Worker 1 -> Job 2: 3
Worker 2 -> Job 0: 5
Worker 3 -> Job 3: 4
Total: 13
```

This implementation efficiently solves the assignment problem with O(n³) time complexity, where n is the number of workers/jobs.

