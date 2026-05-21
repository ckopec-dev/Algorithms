# Simulated Annealing Algorithm in C#

Here's a complete implementation of the Simulated Annealing algorithm in C# for solving the Traveling Salesman Problem (TSP):

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class SimulatedAnnealing
{
    private double[][] distanceMatrix;
    private int[] currentSolution;
    private int[] bestSolution;
    private double currentCost;
    private double bestCost;
    private Random random;
    private int maxIterations;
    private double initialTemperature;
    private double coolingRate;
    private double minTemperature;

    public SimulatedAnnealing(double[][] distanceMatrix, int maxIterations = 10000, 
                             double initialTemperature = 1000, double coolingRate = 0.95, 
                             double minTemperature = 1e-8)
    {
        this.distanceMatrix = distanceMatrix;
        this.maxIterations = maxIterations;
        this.initialTemperature = initialTemperature;
        this.coolingRate = coolingRate;
        this.minTemperature = minTemperature;
        this.random = new Random();
        
        // Initialize with a random solution
        int numCities = distanceMatrix.Length;
        currentSolution = GenerateRandomSolution(numCities);
        bestSolution = new int[numCities];
        Array.Copy(currentSolution, bestSolution, numCities);
        
        currentCost = CalculateTotalDistance(currentSolution);
        bestCost = currentCost;
    }

    private int[] GenerateRandomSolution(int numCities)
    {
        int[] solution = Enumerable.Range(0, numCities).ToArray();
        for (int i = 0; i < numCities; i++)
        {
            int j = random.Next(i, numCities);
            int temp = solution[i];
            solution[i] = solution[j];
            solution[j] = temp;
        }
        return solution;
    }

    private double CalculateTotalDistance(int[] solution)
    {
        double totalDistance = 0;
        int numCities = solution.Length;
        
        for (int i = 0; i < numCities; i++)
        {
            int fromCity = solution[i];
            int toCity = solution[(i + 1) % numCities];
            totalDistance += distanceMatrix[fromCity][toCity];
        }
        
        return totalDistance;
    }

    private int[] GenerateNeighbor(int[] currentSolution)
    {
        int[] neighbor = new int[currentSolution.Length];
        Array.Copy(currentSolution, neighbor, currentSolution.Length);
        
        // Swap two random cities
        int i = random.Next(neighbor.Length);
        int j = random.Next(neighbor.Length);
        
        if (i != j)
        {
            int temp = neighbor[i];
            neighbor[i] = neighbor[j];
            neighbor[j] = temp;
        }
        
        return neighbor;
    }

    private double AcceptanceProbability(double currentCost, double neighborCost, double temperature)
    {
        if (neighborCost < currentCost)
            return 1.0;
        
        return Math.Exp(-(neighborCost - currentCost) / temperature);
    }

    public (int[], double) Solve()
    {
        double temperature = initialTemperature;
        int iteration = 0;

        while (iteration < maxIterations && temperature > minTemperature)
        {
            // Generate neighbor solution
            int[] neighbor = GenerateNeighbor(currentSolution);
            double neighborCost = CalculateTotalDistance(neighbor);
            
            // Calculate acceptance probability
            double acceptanceProb = AcceptanceProbability(currentCost, neighborCost, temperature);
            
            // Accept or reject the neighbor
            if (random.NextDouble() < acceptanceProb)
            {
                currentSolution = neighbor;
                currentCost = neighborCost;
                
                // Update best solution if found a better one
                if (currentCost < bestCost)
                {
                    bestCost = currentCost;
                    Array.Copy(currentSolution, bestSolution, currentSolution.Length);
                }
            }
            
            // Cool down the temperature
            temperature *= coolingRate;
            iteration++;
        }

        return (bestSolution, bestCost);
    }

    public void PrintSolution(int[] solution, double cost)
    {
        Console.WriteLine($"Best solution found: {string.Join(" -> ", solution)}");
        Console.WriteLine($"Total distance: {cost:F2}");
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example: 5 cities with distance matrix
        double[][] distanceMatrix = new double[][]
        {
            new double[] {0, 10, 15, 20, 25},
            new double[] {10, 0, 35, 25, 30},
            new double[] {15, 35, 0, 30, 20},
            new double[] {20, 25, 30, 0, 15},
            new double[] {25, 30, 20, 15, 0}
        };

        // Create and run simulated annealing
        SimulatedAnnealing sa = new SimulatedAnnealing(distanceMatrix, maxIterations: 5000);
        var (bestSolution, bestCost) = sa.Solve();
        
        Console.WriteLine("Simulated Annealing Results:");
        sa.PrintSolution(bestSolution, bestCost);
    }
}
```

## Key Components Explained:

### 1. **Core Algorithm Structure**
- **Temperature Management**: Starts with high temperature and gradually decreases
- **Neighbor Generation**: Creates small changes to current solution
- **Acceptance Criteria**: Uses Metropolis criterion to accept or reject new solutions

### 2. **Key Methods**
- `GenerateRandomSolution()`: Creates initial random tour
- `CalculateTotalDistance()`: Computes total distance of a tour
- `GenerateNeighbor()`: Creates neighboring solution by swapping cities
- `AcceptanceProbability()`: Determines if to accept worse solutions
- `Solve()`: Main optimization loop

### 3. **Parameters**
- `initialTemperature`: Starting temperature
- `coolingRate`: Rate at which temperature decreases (typically 0.8-0.99)
- `minTemperature`: Stopping condition
- `maxIterations`: Maximum number of iterations

### 4. **Advantages**
- Can escape local optima
- Probabilistic acceptance of worse solutions early on
- Flexible framework for various optimization problems

This implementation can be easily adapted for other optimization problems by modifying the neighbor generation and cost calculation functions.

