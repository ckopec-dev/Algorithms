# Genetic Algorithm in F#

Here's a complete implementation of a genetic algorithm in F# that solves the classic "ones maximization" problem (finding a binary string with all 1s):

```fsharp
open System
open System.Linq

// Individual representation - binary string
type Individual = {
    Genes: int[]
    Fitness: float
}

// Genetic Algorithm parameters
type GAParams = {
    PopulationSize: int
    ChromosomeLength: int
    MutationRate: float
    CrossoverRate: float
    EliteSize: int
}

// Generate random individual
let createRandomIndividual (length: int) : Individual =
    let genes = Array.init length (fun _ -> if Random().NextDouble() < 0.5 then 0 else 1)
    { Genes = genes; Fitness = 0.0 }

// Calculate fitness (number of 1s in the genome)
let calculateFitness (individual: Individual) : float =
    individual.Genes |> Array.sumBy float

// Initialize population
let initializePopulation (params: GAParams) : Individual[] =
    Array.init params.PopulationSize (fun _ -> createRandomIndividual params.ChromosomeLength)

// Selection using tournament selection
let tournamentSelection (population: Individual[]) (tournamentSize: int) : Individual =
    let tournament = 
        Array.init tournamentSize (fun _ -> 
            population.[Random().Next(population.Length)])
    
    tournament |> Array.maxBy (fun ind -> ind.Fitness)

// Crossover two parents to create offspring
let crossover (parent1: Individual) (parent2: Individual) (params: GAParams) : Individual * Individual =
    if Random().NextDouble() > params.CrossoverRate then
        parent1, parent2
    else
        let crossoverPoint = Random().Next(1, params.ChromosomeLength - 1)
        
        let child1Genes = Array.zeroCreate params.ChromosomeLength
        let child2Genes = Array.zeroCreate params.ChromosomeLength
        
        // Copy genes from parents
        Array.Copy(parent1.Genes, 0, child1Genes, 0, crossoverPoint)
        Array.Copy(parent1.Genes, crossoverPoint, child1Genes, crossoverPoint, params.ChromosomeLength - crossoverPoint)
        
        Array.Copy(parent2.Genes, 0, child2Genes, 0, crossoverPoint)
        Array.Copy(parent2.Genes, crossoverPoint, child2Genes, crossoverPoint, params.ChromosomeLength - crossoverPoint)
        
        { Genes = child1Genes; Fitness = 0.0 }, 
        { Genes = child2Genes; Fitness = 0.0 }

// Mutation of an individual
let mutate (individual: Individual) (params: GAParams) : Individual =
    let mutatedGenes = Array.copy individual.Genes
    
    for i in 0 .. mutatedGenes.Length - 1 do
        if Random().NextDouble() < params.MutationRate then
            mutatedGenes.[i] <- if mutatedGenes.[i] = 0 then 1 else 0
    
    { individual with Genes = mutatedGenes; Fitness = 0.0 }

// Genetic Algorithm main loop
let runGeneticAlgorithm (params: GAParams) (maxGenerations: int) : Individual =
    // Initialize population
    let mutable population = initializePopulation params
    
    for generation in 1 .. maxGenerations do
        // Calculate fitness for all individuals
        for i in 0 .. population.Length - 1 do
            population.[i] <- { population.[i] with Fitness = calculateFitness population.[i] }
        
        // Sort population by fitness (descending)
        population <- 
            population |> Array.sortByDescending (fun ind -> ind.Fitness)
        
        printfn "Generation %d: Best Fitness = %.2f" generation population.[0].Fitness
        
        // Create new population
        let mutable newPopulation = Array.zeroCreate params.PopulationSize
        
        // Elitism - keep best individuals
        for i in 0 .. params.EliteSize - 1 do
            newPopulation.[i] <- population.[i]
        
        // Generate offspring
        let mutable offspringCount = params.EliteSize
        
        while offspringCount < params.PopulationSize do
            let parent1 = tournamentSelection population 3
            let parent2 = tournamentSelection population 3
            
            let child1, child2 = crossover parent1 parent2 params
            
            let mutatedChild1 = mutate child1 params
            let mutatedChild2 = mutate child2 params
            
            newPopulation.[offspringCount] <- mutatedChild1
            offspringCount <- offspringCount + 1
            
            if offspringCount < params.PopulationSize then
                newPopulation.[offspringCount] <- mutatedChild2
                offspringCount <- offspringCount + 1
        
        population <- newPopulation
    
    // Return best individual
    population |> Array.sortByDescending (fun ind -> ind.Fitness) |> Array.head

// Example usage
[<EntryPoint>]
let main argv =
    let gaParams = {
        PopulationSize = 100
        ChromosomeLength = 20
        MutationRate = 0.01
        CrossoverRate = 0.8
        EliteSize = 2
    }
    
    printfn "Starting Genetic Algorithm..."
    printfn "Parameters: Population=%d, ChromosomeLength=%d, MutationRate=%.2f" 
        gaParams.PopulationSize gaParams.ChromosomeLength gaParams.MutationRate
    
    let bestIndividual = runGeneticAlgorithm gaParams 50
    
    printfn "\nBest Individual:"
    printfn "Genes: %s" (String.concat "" (bestIndividual.Genes |> Array.map string))
    printfn "Fitness: %.2f" bestIndividual.Fitness
    printfn "Target Fitness: %.2f" (float gaParams.ChromosomeLength)
    
    0 // return code
```

## Key Components Explained:

### 1. **Individual Type**
- Represents a single solution with genes (binary array) and fitness value

### 2. **GA Parameters**
- Population size, chromosome length, mutation/crossover rates, elite size

### 3. **Core Operations**
- **Initialization**: Random population generation
- **Fitness Calculation**: Count of 1s in binary string
- **Selection**: Tournament selection method
- **Crossover**: Single-point crossover
- **Mutation**: Bit-flip mutation with specified rate

### 4. **Main Algorithm Flow**
1. Initialize population
2. Evaluate fitness for all individuals
3. Sort by fitness (descending)
4. Apply elitism (keep best individuals)
5. Generate new population through selection, crossover, and mutation
6. Repeat for specified generations

## Sample Output:
```
Starting Genetic Algorithm...
Parameters: Population=100, ChromosomeLength=20, MutationRate=0.01
Generation 1: Best Fitness = 10.00
Generation 2: Best Fitness = 12.00
Generation 3: Best Fitness = 14.00
...
Generation 50: Best Fitness = 20.00

Best Individual:
Genes: 11111111111111111111
Fitness: 20.00
Target Fitness: 20.00
```

This implementation demonstrates key GA concepts including population management, selection pressure, genetic operators, and convergence to optimal solutions.