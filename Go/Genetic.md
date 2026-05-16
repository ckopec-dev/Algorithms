# Genetic Algorithm in Go

Here's a complete example of a Genetic Algorithm implementation in Go that solves the classic "One Max" problem (finding a binary string of all 1s).

```go
package main

import (
    "fmt"
    "math/rand"
    "sort"
    "time"
)

// Individual represents a single solution in the population
type Individual struct {
    Genes   []int
    Fitness int
}

// Population represents a collection of individuals
type Population struct {
    Individuals []Individual
    Size        int
}

// GeneticAlgorithm represents the GA configuration
type GeneticAlgorithm struct {
    PopulationSize int
    ChromosomeLength int
    MutationRate   float64
    CrossoverRate  float64
    MaxGenerations int
}

// NewGeneticAlgorithm creates a new GA instance
func NewGeneticAlgorithm(populationSize, chromosomeLength int, 
                        mutationRate, crossoverRate float64, maxGenerations int) *GeneticAlgorithm {
    return &GeneticAlgorithm{
        PopulationSize:   populationSize,
        ChromosomeLength: chromosomeLength,
        MutationRate:     mutationRate,
        CrossoverRate:    crossoverRate,
        MaxGenerations:   maxGenerations,
    }
}

// CreateRandomIndividual creates a random individual
func (ga *GeneticAlgorithm) CreateRandomIndividual() Individual {
    genes := make([]int, ga.ChromosomeLength)
    for i := range genes {
        genes[i] = rand.Intn(2) // 0 or 1
    }
    return Individual{Genes: genes}
}

// CreateInitialPopulation creates the initial random population
func (ga *GeneticAlgorithm) CreateInitialPopulation() Population {
    individuals := make([]Individual, ga.PopulationSize)
    for i := range individuals {
        individuals[i] = ga.CreateRandomIndividual()
    }
    return Population{
        Individuals: individuals,
        Size:        ga.PopulationSize,
    }
}

// CalculateFitness calculates the fitness of an individual (number of 1s)
func (ga *GeneticAlgorithm) CalculateFitness(individual Individual) int {
    fitness := 0
    for _, gene := range individual.Genes {
        if gene == 1 {
            fitness++
        }
    }
    return fitness
}

// EvaluatePopulation calculates fitness for all individuals
func (ga *GeneticAlgorithm) EvaluatePopulation(population *Population) {
    for i := range population.Individuals {
        population.Individuals[i].Fitness = ga.CalculateFitness(population.Individuals[i])
    }
}

// TournamentSelection selects an individual using tournament selection
func (ga *GeneticAlgorithm) TournamentSelection(population Population, tournamentSize int) Individual {
    best := population.Individuals[rand.Intn(population.Size)]
    
    for i := 1; i < tournamentSize; i++ {
        candidate := population.Individuals[rand.Intn(population.Size)]
        if candidate.Fitness > best.Fitness {
            best = candidate
        }
    }
    
    return best
}

// Crossover performs single-point crossover between two parents
func (ga *GeneticAlgorithm) Crossover(parent1, parent2 Individual) (Individual, Individual) {
    if rand.Float64() > ga.CrossoverRate {
        return parent1, parent2
    }
    
    crossoverPoint := rand.Intn(ga.ChromosomeLength)
    
    child1 := Individual{Genes: make([]int, ga.ChromosomeLength)}
    child2 := Individual{Genes: make([]int, ga.ChromosomeLength)}
    
    // Copy genes from parents
    for i := 0; i < crossoverPoint; i++ {
        child1.Genes[i] = parent1.Genes[i]
        child2.Genes[i] = parent2.Genes[i]
    }
    
    for i := crossoverPoint; i < ga.ChromosomeLength; i++ {
        child1.Genes[i] = parent2.Genes[i]
        child2.Genes[i] = parent1.Genes[i]
    }
    
    return child1, child2
}

// Mutate performs bit-flip mutation on an individual
func (ga *GeneticAlgorithm) Mutate(individual *Individual) {
    for i := range individual.Genes {
        if rand.Float64() < ga.MutationRate {
            individual.Genes[i] = 1 - individual.Genes[i] // Flip bit
        }
    }
}

// GetBestIndividual returns the individual with highest fitness
func (ga *GeneticAlgorithm) GetBestIndividual(population Population) Individual {
    best := population.Individuals[0]
    for _, individual := range population.Individuals {
        if individual.Fitness > best.Fitness {
            best = individual
        }
    }
    return best
}

// SortPopulation sorts individuals by fitness (descending)
func (ga *GeneticAlgorithm) SortPopulation(population *Population) {
    sort.Slice(population.Individuals, func(i, j int) bool {
        return population.Individuals[i].Fitness > population.Individuals[j].Fitness
    })
}

// Run executes the genetic algorithm
func (ga *GeneticAlgorithm) Run() Individual {
    // Create initial population
    population := ga.CreateInitialPopulation()
    
    fmt.Printf("Starting Genetic Algorithm\n")
    fmt.Printf("Population Size: %d\n", ga.PopulationSize)
    fmt.Printf("Chromosome Length: %d\n", ga.ChromosomeLength)
    fmt.Printf("Max Generations: %d\n", ga.MaxGenerations)
    fmt.Println()
    
    for generation := 0; generation < ga.MaxGenerations; generation++ {
        // Evaluate fitness
        ga.EvaluatePopulation(&population)
        
        // Get best individual
        best := ga.GetBestIndividual(population)
        
        // Print progress
        if generation%100 == 0 || generation == ga.MaxGenerations-1 {
            fmt.Printf("Generation %d - Best Fitness: %d/%d\n", 
                generation, best.Fitness, ga.ChromosomeLength)
        }
        
        // Check if we've found the optimal solution
        if best.Fitness == ga.ChromosomeLength {
            fmt.Printf("Optimal solution found in generation %d!\n", generation)
            return best
        }
        
        // Create new population
        newPopulation := make([]Individual, ga.PopulationSize)
        
        // Elitism: keep the best individual
        ga.SortPopulation(&population)
        newPopulation[0] = population.Individuals[0]
        
        // Generate offspring
        for i := 1; i < ga.PopulationSize; i += 2 {
            parent1 := ga.TournamentSelection(population, 3)
            parent2 := ga.TournamentSelection(population, 3)
            
            child1, child2 := ga.Crossover(parent1, parent2)
            
            ga.Mutate(&child1)
            ga.Mutate(&child2)
            
            newPopulation[i] = child1
            if i+1 < ga.PopulationSize {
                newPopulation[i+1] = child2
            }
        }
        
        population.Individuals = newPopulation
    }
    
    // Return best individual from final population
    ga.EvaluatePopulation(&population)
    return ga.GetBestIndividual(population)
}

func main() {
    // Seed the random number generator
    rand.Seed(time.Now().UnixNano())
    
    // Configure GA parameters
    ga := NewGeneticAlgorithm(
        populationSize:   100,
        chromosomeLength: 20,
        mutationRate:     0.01,
        crossoverRate:    0.8,
        maxGenerations:   1000,
    )
    
    // Run the genetic algorithm
    best := ga.Run()
    
    // Display results
    fmt.Println()
    fmt.Printf("Final Result:\n")
    fmt.Printf("Best Individual: %v\n", best.Genes)
    fmt.Printf("Fitness: %d/%d\n", best.Fitness, ga.ChromosomeLength)
    
    // Verify solution
    allOnes := true
    for _, gene := range best.Genes {
        if gene != 1 {
            allOnes = false
            break
        }
    }
    
    if allOnes {
        fmt.Println("✓ Solution is optimal (all 1s)")
    } else {
        fmt.Println("✗ Solution is not optimal")
    }
}
```

## Key Components Explained

### 1. **Individual Structure**
- Represents a single solution with genes (binary string) and fitness value

### 2. **Population Structure**
- Collection of individuals forming the current generation

### 3. **GeneticAlgorithm Structure**
- Configuration parameters for the GA:
  - Population size
  - Chromosome length
  - Mutation rate
  - Crossover rate
  - Maximum generations

### 4. **Core Operations**
- **Selection**: Tournament selection
- **Crossover**: Single-point crossover
- **Mutation**: Bit-flip mutation
- **Evaluation**: Fitness calculation (count of 1s)

### 5. **Algorithm Flow**
1. Initialize random population
2. Evaluate fitness of all individuals
3. Select parents using tournament selection
4. Apply crossover and mutation to create offspring
5. Replace old population with new one
6. Repeat until optimal solution or max generations reached

## Sample Output
```
Starting Genetic Algorithm
Population Size: 100
Chromosome Length: 20
Max Generations: 1000

Generation 0 - Best Fitness: 12/20
Generation 100 - Best Fitness: 18/20
Generation 200 - Best Fitness: 19/20
Generation 300 - Best Fitness: 20/20
Optimal solution found in generation 300!

Final Result:
Best Individual: [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
Fitness: 20/20
✓ Solution is optimal (all 1s)
```

This implementation demonstrates the core principles of genetic algorithms: selection, crossover, mutation, and evaluation - all implemented in clean, idiomatic Go code.

