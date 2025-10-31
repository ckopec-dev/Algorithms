# Artificial Bee Colony Algorithm in C#

The Artificial Bee Colony (ABC) algorithm is a nature-inspired optimization algorithm based on the foraging behavior of honey bees. Below is a complete implementation in C#:

```csharp
using System;
using System.Linq;

public class ArtificialBeeColony
{
    private int populationSize;
    private int maxIterations;
    private int maxTrials;
    private double lowerBound;
    private double upperBound;
    private double[] bestSolution;
    private double bestFitness;
    private Random random;
    
    // Bee class to represent each solution
    public class Bee
    {
        public double[] position;
        public double fitness;
        public int trials;
        
        public Bee(int dimension)
        {
            position = new double[dimension];
            trials = 0;
        }
    }
    
    public ArtificialBeeColony(int populationSize, int maxIterations, int maxTrials, 
                              double lowerBound, double upperBound)
    {
        this.populationSize = populationSize;
        this.maxIterations = maxIterations;
        this.maxTrials = maxTrials;
        this.lowerBound = lowerBound;
        this.upperBound = upperBound;
        this.random = new Random();
        this.bestSolution = new double[0];
        this.bestFitness = double.MaxValue;
    }
    
    // Objective function to minimize (example: Sphere function)
    private double ObjectiveFunction(double[] position)
    {
        return position.Sum(x => x * x);
    }
    
    // Initialize population
    private Bee[] InitializePopulation(int dimension)
    {
        Bee[] population = new Bee[populationSize];
        
        for (int i = 0; i < populationSize; i++)
        {
            population[i] = new Bee(dimension);
            
            // Initialize each position randomly within bounds
            for (int j = 0; j < dimension; j++)
            {
                population[i].position[j] = lowerBound + 
                    random.NextDouble() * (upperBound - lowerBound);
            }
            
            population[i].fitness = ObjectiveFunction(population[i].position);
            
            // Update global best
            if (population[i].fitness < bestFitness)
            {
                bestFitness = population[i].fitness;
                bestSolution = (double[])population[i].position.Clone();
            }
        }
        
        return population;
    }
    
    // Employed bee phase
    private void EmployedBeePhase(Bee[] population, int dimension)
    {
        for (int i = 0; i < populationSize; i++)
        {
            // Choose a random dimension to modify
            int randomDimension = random.Next(dimension);
            
            // Choose a random neighbor bee
            int randomBee = random.Next(populationSize);
            
            // Generate new solution using formula: v_i = x_i + phi * (x_i - x_k)
            double phi = random.NextDouble() * 2.0 - 1.0; // Random value between -1 and 1
            double[] newPosition = new double[dimension];
            
            for (int j = 0; j < dimension; j++)
            {
                if (j == randomDimension)
                {
                    newPosition[j] = population[i].position[j] + 
                        phi * (population[i].position[j] - population[randomBee].position[j]);
                    
                    // Ensure new position is within bounds
                    newPosition[j] = Math.Max(lowerBound, Math.Min(upperBound, newPosition[j]));
                }
                else
                {
                    newPosition[j] = population[i].position[j];
                }
            }
            
            // Evaluate new solution
            double newFitness = ObjectiveFunction(newPosition);
            
            // Apply greedy selection
            if (newFitness < population[i].fitness)
            {
                population[i].position = newPosition;
                population[i].fitness = newFitness;
                population[i].trials = 0;
                
                // Update global best
                if (newFitness < bestFitness)
                {
                    bestFitness = newFitness;
                    bestSolution = (double[])newPosition.Clone();
                }
            }
            else
            {
                population[i].trials++;
            }
        }
    }
    
    // Onlooker bee phase
    private void OnlookerBeePhase(Bee[] population, int dimension)
    {
        double totalFitness = population.Sum(b => 1.0 / (b.fitness + 1e-10)); // Avoid division by zero
        
        for (int i = 0; i < populationSize; i++)
        {
            // Roulette wheel selection based on fitness
            double probability = (1.0 / (population[i].fitness + 1e-10)) / totalFitness;
            double rand = random.NextDouble();
            
            if (rand < probability)
            {
                // Choose a random dimension to modify
                int randomDimension = random.Next(dimension);
                int randomBee = random.Next(populationSize);
                
                // Generate new solution using formula: v_i = x_i + phi * (x_i - x_k)
                double phi = random.NextDouble() * 2.0 - 1.0;
                double[] newPosition = new double[dimension];
                
                for (int j = 0; j < dimension; j++)
                {
                    if (j == randomDimension)
                    {
                        newPosition[j] = population[i].position[j] + 
                            phi * (population[i].position[j] - population[randomBee].position[j]);
                        
                        // Ensure new position is within bounds
                        newPosition[j] = Math.Max(lowerBound, Math.Min(upperBound, newPosition[j]));
                    }
                    else
                    {
                        newPosition[j] = population[i].position[j];
                    }
                }
                
                // Evaluate new solution
                double newFitness = ObjectiveFunction(newPosition);
                
                // Apply greedy selection
                if (newFitness < population[i].fitness)
                {
                    population[i].position = newPosition;
                    population[i].fitness = newFitness;
                    population[i].trials = 0;
                    
                    // Update global best
                    if (newFitness < bestFitness)
                    {
                        bestFitness = newFitness;
                        bestSolution = (double[])newPosition.Clone();
                    }
                }
                else
                {
                    population[i].trials++;
                }
            }
        }
    }
    
    // Scout bee phase
    private void ScoutBeePhase(Bee[] population)
    {
        for (int i = 0; i < populationSize; i++)
        {
            if (population[i].trials >= maxTrials)
            {
                // Replace with new random solution
                for (int j = 0; j < population[i].position.Length; j++)
                {
                    population[i].position[j] = lowerBound + 
                        random.NextDouble() * (upperBound - lowerBound);
                }
                
                population[i].fitness = ObjectiveFunction(population[i].position);
                population[i].trials = 0;
                
                // Update global best
                if (population[i].fitness < bestFitness)
                {
                    bestFitness = population[i].fitness;
                    bestSolution = (double[])population[i].position.Clone();
                }
            }
        }
    }
    
    // Main ABC algorithm
    public double[] Optimize(int dimension)
    {
        // Initialize population
        Bee[] population = InitializePopulation(dimension);
        
        Console.WriteLine($"Initial best fitness: {bestFitness:F6}");
        
        // Main iteration loop
        for (int iteration = 0; iteration < maxIterations; iteration++)
        {
            // Employed bee phase
            EmployedBeePhase(population, dimension);
            
            // Onlooker bee phase
            OnlookerBeePhase(population, dimension);
            
            // Scout bee phase
            ScoutBeePhase(population);
            
            // Display progress every 100 iterations
            if ((iteration + 1) % 100 == 0)
            {
                Console.WriteLine($"Iteration {iteration + 1}: Best fitness = {bestFitness:F6}");
            }
        }
        
        Console.WriteLine($"\nFinal best solution:");
        Console.WriteLine($"Best fitness: {bestFitness:F6}");
        Console.WriteLine("Best position:");
        for (int i = 0; i < bestSolution.Length; i++)
        {
            Console.Write($"{bestSolution[i]:F6} ");
        }
        Console.WriteLine();
        
        return bestSolution;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        Console.WriteLine("Artificial Bee Colony Algorithm Demo");
        Console.WriteLine("=====================================");
        
        // Parameters for ABC algorithm
        int populationSize = 50;
        int maxIterations = 1000;
        int maxTrials = 100;
        double lowerBound = -5.0;
        double upperBound = 5.0;
        int dimension = 10; // For 10-dimensional optimization
        
        // Create ABC instance
        ArtificialBeeColony abc = new ArtificialBeeColony(
            populationSize, maxIterations, maxTrials, lowerBound, upperBound);
        
        // Run optimization
        double[] result = abc.Optimize(dimension);
        
        Console.WriteLine("\nOptimization completed!");
    }
}
```

## Key Features of this Implementation:

1. **Bee Class**: Represents individual solutions with position, fitness, and trial count
2. **Three Phases**:
   - **Employed Bee Phase**: Each bee searches for better solutions in its neighborhood
   - **Onlooker Bee Phase**: Bees select solutions based on fitness probability
   - **Scout Bee Phase**: Abandon poor solutions and generate new random ones

3. **Core Algorithm Components**:
   - Population initialization with random positions
   - Fitness evaluation using the sphere function (can be replaced)
   - Boundary constraints handling
   - Trial counter for abandoning solutions
   - Global best solution tracking

4. **Parameters**:
   - `populationSize`: Number of bees in the colony
   - `maxIterations`: Maximum number of iterations
   - `maxTrials`: Maximum trials before abandoning a solution
   - `lowerBound`, `upperBound`: Search space boundaries

## Usage Example:

The algorithm is demonstrated with a 10-dimensional sphere function optimization, which aims to find the minimum value (near zero) at the origin point. You can easily modify the `ObjectiveFunction` method to optimize different problems by replacing the sphere function with your specific objective function.

This implementation provides a solid foundation that can be extended for more complex optimization problems.

