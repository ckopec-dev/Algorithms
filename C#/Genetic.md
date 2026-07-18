# Genetic Algorithm Example in C#

Here's a complete implementation of a genetic algorithm in C# that solves the classic "maximize x²" problem within a given range:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class GeneticAlgorithm
{
    // Parameters
    private int populationSize = 100;
    private int chromosomeLength = 10;
    private double mutationRate = 0.01;
    private double crossoverRate = 0.8;
    private int maxGenerations = 1000;
    
    // Problem bounds (maximize x^2 where -5 <= x <= 5)
    private double minX = -5.0;
    private double maxX = 5.0;
    
    public class Individual
    {
        public List<int> genes { get; set; }
        public double fitness { get; set; }
        public double decodedValue { get; set; }
        
        public Individual(int length)
        {
            genes = new List<int>(length);
            for (int i = 0; i < length; i++)
            {
                genes.Add(new Random().Next(0, 2)); // Random 0 or 1
            }
            fitness = 0;
            decodedValue = 0;
        }
    }
    
    public static void Main(string[] args)
    {
        GeneticAlgorithm ga = new GeneticAlgorithm();
        ga.Run();
    }
    
    public void Run()
    {
        // Initialize population
        List<Individual> population = InitializePopulation();
        
        Console.WriteLine("Genetic Algorithm - Maximize x²");
        Console.WriteLine("================================");
        
        for (int generation = 0; generation < maxGenerations; generation++)
        {
            // Evaluate fitness
            EvaluateFitness(population);
            
            // Find best individual
            Individual best = population.OrderByDescending(i => i.fitness).First();
            double actualValue = DecodeIndividual(best);
            
            Console.WriteLine($"Generation {generation}: Best x = {actualValue:F4}, f(x) = {best.fitness:F4}");
            
            // Check if we've found a good solution
            if (Math.Abs(actualValue - Math.Sqrt(25)) < 0.01)
            {
                Console.WriteLine("Solution found!");
                break;
            }
            
            // Create new population
            List<Individual> newPopulation = new List<Individual>();
            
            // Keep best individuals (elitism)
            var sortedPopulation = population.OrderByDescending(i => i.fitness).ToList();
            for (int i = 0; i < populationSize / 10; i++)
            {
                newPopulation.Add(new Individual(chromosomeLength));
                newPopulation[i].genes = sortedPopulation[i].genes.ToList();
            }
            
            // Generate offspring
            while (newPopulation.Count < populationSize)
            {
                Individual parent1 = TournamentSelection(population);
                Individual parent2 = TournamentSelection(population);
                
                if (new Random().NextDouble() < crossoverRate)
                {
                    List<Individual> children = Crossover(parent1, parent2);
                    newPopulation.Add(Mutate(children[0]));
                    if (newPopulation.Count < populationSize)
                        newPopulation.Add(Mutate(children[1]));
                }
                else
                {
                    newPopulation.Add(new Individual(chromosomeLength));
                    newPopulation[newPopulation.Count - 1].genes = parent1.genes.ToList();
                    if (newPopulation.Count < populationSize)
                    {
                        newPopulation.Add(new Individual(chromosomeLength));
                        newPopulation[newPopulation.Count - 1].genes = parent2.genes.ToList();
                    }
                }
            }
            
            population = newPopulation;
        }
        
        // Final result
        EvaluateFitness(population);
        Individual finalBest = population.OrderByDescending(i => i.fitness).First();
        double finalValue = DecodeIndividual(finalBest);
        Console.WriteLine($"\nFinal Result: x = {finalValue:F4}, f(x) = {finalBest.fitness:F4}");
    }
    
    private List<Individual> InitializePopulation()
    {
        List<Individual> population = new List<Individual>();
        for (int i = 0; i < populationSize; i++)
        {
            population.Add(new Individual(chromosomeLength));
        }
        return population;
    }
    
    private void EvaluateFitness(List<Individual> population)
    {
        foreach (Individual individual in population)
        {
            individual.decodedValue = DecodeIndividual(individual);
            // Maximize x^2 (we want to maximize the function)
            individual.fitness = individual.decodedValue * individual.decodedValue;
        }
    }
    
    private double DecodeIndividual(Individual individual)
    {
        // Convert binary chromosome to decimal value in range [minX, maxX]
        double value = 0;
        double power = 1;
        
        for (int i = 0; i < individual.genes.Count; i++)
        {
            if (individual.genes[i] == 1)
            {
                value += power;
            }
            power *= 2;
        }
        
        // Scale to range [minX, maxX]
        double scaledValue = minX + (value / (Math.Pow(2, chromosomeLength) - 1)) * (maxX - minX);
        return scaledValue;
    }
    
    private Individual TournamentSelection(List<Individual> population)
    {
        int tournamentSize = 3;
        Individual best = null;
        
        for (int i = 0; i < tournamentSize; i++)
        {
            int randomIndex = new Random().Next(0, population.Count);
            Individual candidate = population[randomIndex];
            
            if (best == null || candidate.fitness > best.fitness)
            {
                best = candidate;
            }
        }
        
        return best;
    }
    
    private List<Individual> Crossover(Individual parent1, Individual parent2)
    {
        List<Individual> children = new List<Individual> { new Individual(chromosomeLength), new Individual(chromosomeLength) };
        
        int crossoverPoint = new Random().Next(1, chromosomeLength - 1);
        
        for (int i = 0; i < chromosomeLength; i++)
        {
            if (i < crossoverPoint)
            {
                children[0].genes[i] = parent1.genes[i];
                children[1].genes[i] = parent2.genes[i];
            }
            else
            {
                children[0].genes[i] = parent2.genes[i];
                children[1].genes[i] = parent1.genes[i];
            }
        }
        
        return children;
    }
    
    private Individual Mutate(Individual individual)
    {
        for (int i = 0; i < individual.genes.Count; i++)
        {
            if (new Random().NextDouble() < mutationRate)
            {
                individual.genes[i] = individual.genes[i] == 0 ? 1 : 0;
            }
        }
        return individual;
    }
}
```

## Key Components Explained

### 1. **Individual Class**
- Represents a chromosome with binary genes
- Stores fitness value and decoded decimal value
- Contains methods for genetic operations

### 2. **Main Algorithm Steps**
- **Initialization**: Create random population of individuals
- **Evaluation**: Calculate fitness for each individual
- **Selection**: Use tournament selection to choose parents
- **Crossover**: Combine parent genes to create offspring
- **Mutation**: Randomly flip bits in chromosomes
- **Replacement**: Generate new population

### 3. **Key Parameters**
```csharp
private int populationSize = 100;      // Number of individuals
private int chromosomeLength = 10;     // Binary string length
private double mutationRate = 0.01;    // Probability of mutation
private double crossoverRate = 0.8;    // Probability of crossover
private int maxGenerations = 1000;     // Maximum iterations
```

### 4. **Output Example**
```
Genetic Algorithm - Maximize x²
================================
Generation 0: Best x = 2.3456, f(x) = 5.4987
Generation 1: Best x = 3.1234, f(x) = 9.7578
Generation 2: Best x = 4.2345, f(x) = 17.9321
...
Final Result: x = 5.0000, f(x) = 25.0000
```

This implementation demonstrates the core principles of genetic algorithms including population initialization, selection, crossover, mutation, and fitness evaluation.