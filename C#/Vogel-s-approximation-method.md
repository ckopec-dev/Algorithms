# Vogel's Approximation Method (VAM) in C#

Vogel's Approximation Method is a heuristic technique for solving transportation problems in operations research. Here's a complete implementation in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class TransportationProblem
{
    public class TransportationSolution
    {
        public int[,] Allocation { get; set; }
        public double TotalCost { get; set; }
        public int Rows { get; set; }
        public int Columns { get; set; }
    }

    public static TransportationSolution SolveVogelsApproximationMethod(
        double[,] costs, 
        int[] supply, 
        int[] demand)
    {
        int rows = costs.GetLength(0);
        int cols = costs.GetLength(1);
        
        // Create copies of the original data
        double[,] costMatrix = new double[rows, cols];
        int[] supplyCopy = new int[rows];
        int[] demandCopy = new int[cols];
        
        for (int i = 0; i < rows; i++)
            for (int j = 0; j < cols; j++)
                costMatrix[i, j] = costs[i, j];
        
        Array.Copy(supply, supplyCopy, rows);
        Array.Copy(demand, demandCopy, cols);
        
        // Initialize allocation matrix
        int[,] allocation = new int[rows, cols];
        double totalCost = 0;
        
        while (HasUnsatisfiedSupplyOrDemand(supplyCopy, demandCopy))
        {
            // Calculate penalties for each row and column
            double[] rowPenalties = CalculateRowPenalties(costMatrix, supplyCopy, demandCopy);
            double[] colPenalties = CalculateColumnPenalties(costMatrix, supplyCopy, demandCopy);
            
            // Find maximum penalty
            double maxPenalty = Math.Max(rowPenalties.Max(), colPenalties.Max());
            
            // Find the cell with maximum penalty
            (int row, int col) = FindCellWithMaxPenalty(rowPenalties, colPenalties, maxPenalty);
            
            // Allocate maximum possible amount
            int allocationAmount = Math.Min(supplyCopy[row], demandCopy[col]);
            allocation[row, col] = allocationAmount;
            
            // Update supply and demand
            supplyCopy[row] -= allocationAmount;
            demandCopy[col] -= allocationAmount;
            
            // Update total cost
            totalCost += allocationAmount * costMatrix[row, col];
            
            // Remove rows/columns that are fully satisfied
            if (supplyCopy[row] == 0)
            {
                for (int j = 0; j < cols; j++)
                    costMatrix[row, j] = double.MaxValue;
            }
            
            if (demandCopy[col] == 0)
            {
                for (int i = 0; i < rows; i++)
                    costMatrix[i, col] = double.MaxValue;
            }
        }
        
        return new TransportationSolution
        {
            Allocation = allocation,
            TotalCost = totalCost,
            Rows = rows,
            Columns = cols
        };
    }
    
    private static bool HasUnsatisfiedSupplyOrDemand(int[] supply, int[] demand)
    {
        return supply.Sum() > 0 || demand.Sum() > 0;
    }
    
    private static double[] CalculateRowPenalties(double[,] costs, int[] supply, int[] demand)
    {
        double[] penalties = new double[costs.GetLength(0)];
        
        for (int i = 0; i < costs.GetLength(0); i++)
        {
            if (supply[i] == 0) continue;
            
            double min1 = double.MaxValue;
            double min2 = double.MaxValue;
            
            for (int j = 0; j < costs.GetLength(1); j++)
            {
                if (demand[j] == 0) continue;
                
                if (costs[i, j] < min1)
                {
                    min2 = min1;
                    min1 = costs[i, j];
                }
                else if (costs[i, j] < min2)
                {
                    min2 = costs[i, j];
                }
            }
            
            penalties[i] = min2 - min1;
        }
        
        return penalties;
    }
    
    private static double[] CalculateColumnPenalties(double[,] costs, int[] supply, int[] demand)
    {
        double[] penalties = new double[costs.GetLength(1)];
        
        for (int j = 0; j < costs.GetLength(1); j++)
        {
            if (demand[j] == 0) continue;
            
            double min1 = double.MaxValue;
            double min2 = double.MaxValue;
            
            for (int i = 0; i < costs.GetLength(0); i++)
            {
                if (supply[i] == 0) continue;
                
                if (costs[i, j] < min1)
                {
                    min2 = min1;
                    min1 = costs[i, j];
                }
                else if (costs[i, j] < min2)
                {
                    min2 = costs[i, j];
                }
            }
            
            penalties[j] = min2 - min1;
        }
        
        return penalties;
    }
    
    private static (int row, int col) FindCellWithMaxPenalty(double[] rowPenalties, double[] colPenalties, double maxPenalty)
    {
        // Find first row with maximum penalty
        for (int i = 0; i < rowPenalties.Length; i++)
        {
            if (rowPenalties[i] == maxPenalty)
                return (i, -1); // Return row index
        }
        
        // Find first column with maximum penalty
        for (int j = 0; j < colPenalties.Length; j++)
        {
            if (colPenalties[j] == maxPenalty)
                return (-1, j); // Return column index
        }
        
        return (0, 0); // Default case
    }
    
    public static void PrintSolution(TransportationSolution solution)
    {
        Console.WriteLine("Transportation Solution using Vogel's Approximation Method:");
        Console.WriteLine("=========================================================");
        
        Console.WriteLine("Allocation Matrix:");
        for (int i = 0; i < solution.Rows; i++)
        {
            for (int j = 0; j < solution.Columns; j++)
            {
                Console.Write($"{solution.Allocation[i, j],4} ");
            }
            Console.WriteLine();
        }
        
        Console.WriteLine($"\nTotal Cost: {solution.TotalCost:F2}");
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example transportation problem
        // Cost matrix (4 suppliers, 4 customers)
        double[,] costs = {
            {8, 6, 10, 9},
            {9, 12, 13, 7},
            {14, 9, 16, 5},
            {11, 8, 10, 6}
        };
        
        // Supply quantities
        int[] supply = { 11, 13, 12, 14 };
        
        // Demand quantities
        int[] demand = { 20, 10, 15, 15 };
        
        Console.WriteLine("Transportation Problem:");
        Console.WriteLine("Cost Matrix:");
        for (int i = 0; i < costs.GetLength(0); i++)
        {
            for (int j = 0; j < costs.GetLength(1); j++)
            {
                Console.Write($"{costs[i, j],4} ");
            }
            Console.WriteLine();
        }
        
        Console.WriteLine($"\nSupply: [{string.Join(", ", supply)}]");
        Console.WriteLine($"Demand: [{string.Join(", ", demand)}]");
        
        // Solve using Vogel's Approximation Method
        var solution = TransportationProblem.SolveVogelsApproximationMethod(costs, supply, demand);
        
        // Print the solution
        TransportationProblem.PrintSolution(solution);
    }
}
```

## How the Algorithm Works:

1. **Initialization**: Create copies of the cost matrix, supply, and demand arrays
2. **Penalty Calculation**: 
   - For each row, find the two smallest costs and calculate the difference (penalty)
   - For each column, find the two smallest costs and calculate the difference (penalty)
3. **Select Maximum Penalty**: Find the row or column with the highest penalty
4. **Allocate**: Allocate the maximum possible amount to the cell with the lowest cost in the selected row/column
5. **Update**: Adjust supply and demand, remove satisfied rows/columns, and repeat until all demands are met

## Key Features:

- **Heuristic Approach**: Provides near-optimal solutions quickly
- **Penalty Method**: Uses the difference between smallest costs to prioritize allocations
- **Efficient**: Reduces the number of iterations needed compared to basic methods
- **Complete Solution**: Returns both the allocation matrix and total cost

The algorithm is particularly useful for large transportation problems where exact methods like the simplex algorithm would be computationally expensive.

