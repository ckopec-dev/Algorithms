# N-Queens Problem Solution in C#

Here's a complete implementation of the N-Queens problem using backtracking algorithm:

```csharp
using System;
using System.Collections.Generic;

public class NQueens
{
    private int n;
    private int[] board;
    private List<List<string>> solutions;

    public NQueens(int n)
    {
        this.n = n;
        this.board = new int[n];
        this.solutions = new List<List<string>>();
    }

    // Main method to solve N-Queens problem
    public List<List<string>> Solve()
    {
        solutions.Clear();
        SolveNQueens(0);
        return solutions;
    }

    // Recursive backtracking function
    private void SolveNQueens(int row)
    {
        if (row == n)
        {
            // Found a valid solution
            solutions.Add(ConvertBoardToString());
            return;
        }

        for (int col = 0; col < n; col++)
        {
            if (IsSafe(row, col))
            {
                board[row] = col;  // Place queen
                SolveNQueens(row + 1);  // Recurse to next row
                // No need to explicitly backtrack as we overwrite board[row]
            }
        }
    }

    // Check if placing a queen at (row, col) is safe
    private bool IsSafe(int row, int col)
    {
        for (int i = 0; i < row; i++)
        {
            int prevCol = board[i];
            
            // Check column conflict
            if (prevCol == col)
                return false;
            
            // Check diagonal conflicts
            if (Math.Abs(prevCol - col) == Math.Abs(i - row))
                return false;
        }
        return true;
    }

    // Convert board state to string representation
    private List<string> ConvertBoardToString()
    {
        List<string> solution = new List<string>();
        
        for (int i = 0; i < n; i++)
        {
            char[] row = new char[n];
            Array.Fill(row, '.');  // Fill with empty spaces
            row[board[i]] = 'Q';   // Place queen
            solution.Add(new string(row));
        }
        
        return solution;
    }

    // Print all solutions
    public void PrintSolutions()
    {
        Console.WriteLine($"Found {solutions.Count} solutions for {n}-Queens problem:");
        Console.WriteLine();

        for (int sol = 0; sol < solutions.Count; sol++)
        {
            Console.WriteLine($"Solution {sol + 1}:");
            foreach (string row in solutions[sol])
            {
                Console.WriteLine(row);
            }
            Console.WriteLine();
        }
    }
}

// Example usage
class Program
{
    static void Main(string[] args)
    {
        // Solve 4-Queens problem
        NQueens nQueens = new NQueens(4);
        var solutions = nQueens.Solve();
        
        Console.WriteLine("N-Queens Problem Solution");
        Console.WriteLine("========================");
        nQueens.PrintSolutions();
        
        // Solve 8-Queens problem (more complex)
        Console.WriteLine("Solving 8-Queens problem:");
        NQueens nQueens8 = new NQueens(8);
        var solutions8 = nQueens8.Solve();
        Console.WriteLine($"Found {solutions8.Count} solutions for 8-Queens problem");
    }
}
```

## How it works:

1. **Backtracking Algorithm**: Uses recursive backtracking to try placing queens row by row
2. **Safety Check**: `IsSafe()` function checks if a queen can be placed at a given position without conflicts
3. **Conflict Detection**: 
   - Same column check: `prevCol == col`
   - Diagonal check: `Math.Abs(prevCol - col) == Math.Abs(i - row)`
4. **Solution Storage**: Stores valid board configurations as lists of strings

## Sample Output:
```
Found 2 solutions for 4-Queens problem:

Solution 1:
.Q..
...Q
Q...
..Q.

Solution 2:
..Q.
Q...
...Q
.Q..

Found 92 solutions for 8-Queens problem
```

## Time Complexity:
- **Best Case**: O(N!) - when there are no conflicts early in the process
- **Worst Case**: O(N!) - in the worst case, we might need to explore all possibilities

## Space Complexity:
- O(N²) for storing the board and solutions
- O(N) for the recursion stack depth

This implementation efficiently solves the N-Queens problem using backtracking with proper conflict detection.