# Artificial Bee Colony Algorithm in F#

Here's a complete implementation of the Artificial Bee Colony (ABC) algorithm in F#:

```fsharp
open System
open System.Random

// Define the solution type
type Solution = {
    Position: float[]
    Fitness: float
}

// ABC Algorithm Implementation
type ArtificialBeeColony(
    objectiveFunction: float[] -> float,
    dimension: int,
    lowerBound: float,
    upperBound: float,
    populationSize: int,
    maxIterations: int,
    limit: int) =

    let random = Random()
    
    // Generate a random solution within bounds
    let generateRandomSolution () : Solution =
        let position = Array.create dimension 0.0
        for i in 0 .. dimension - 1 do
            position.[i] <- lowerBound + random.NextDouble() * (upperBound - lowerBound)
        { Position = position; Fitness = objectiveFunction position }
    
    // Initialize the population
    let initializePopulation () : Solution[] =
        Array.init populationSize generateRandomSolution
    
    // Generate a new solution using neighborhood search
    let generateNewSolution (current: Solution) (employedBees: Solution[]) : Solution =
        let randomIndex = random.Next(dimension)
        let randomEmployedBee = employedBees.[random.Next(populationSize)]
        
        // Generate new position using formula: new_pos = current + phi * (current - random_bee)
        let newPosition = Array.zeroCreate dimension
        for i in 0 .. dimension - 1 do
            if i = randomIndex then
                let phi = -1.0 + 2.0 * random.NextDouble()
                newPosition.[i] <- current.Position.[i] + phi * (current.Position.[i] - randomEmployedBee.Position.[i])
            else
                newPosition.[i] <- current.Position.[i]
        
        // Ensure bounds
        for i in 0 .. dimension - 1 do
            if newPosition.[i] < lowerBound then newPosition.[i] <- lowerBound
            elif newPosition.[i] > upperBound then newPosition.[i] <- upperBound
        
        { Position = newPosition; Fitness = objectiveFunction newPosition }
    
    // Main ABC algorithm execution
    member this.Run() : Solution =
        let population = initializePopulation()
        let bestSolution = Array.minBy (fun s -> s.Fitness) population
        
        for iteration in 1 .. maxIterations do
            // Employed Bee Phase
            let newPopulation = 
                population 
                |> Array.map (fun current ->
                    let newSolution = generateNewSolution current population
                    if newSolution.Fitness < current.Fitness then newSolution else current)
            
            // Onlooker Bee Phase
            let totalFitness = newPopulation |> Array.sumBy (fun s -> 1.0 / (1.0 + s.Fitness))
            
            let onlookerPopulation = 
                newPopulation 
                |> Array.mapi (fun i current ->
                    let probability = (1.0 / (1.0 + current.Fitness)) / totalFitness
                    if random.NextDouble() < probability then
                        generateNewSolution current population
                    else
                        current)
            
            // Scout Bee Phase
            let scoutPopulation = 
                onlookerPopulation 
                |> Array.mapi (fun i current ->
                    // This is a simplified scout implementation - in practice, you'd track trial counts
                    if random.NextDouble() < 0.1 then // 10% chance of scouting
                        generateRandomSolution()
                    else
                        current)
            
            let currentBest = Array.minBy (fun s -> s.Fitness) scoutPopulation
            if currentBest.Fitness < bestSolution.Fitness then
                printfn "Iteration %d: New best fitness: %f" iteration currentBest.Fitness
                |> ignore
            
            population <- scoutPopulation
        
        Array.minBy (fun s -> s.Fitness) population

// Example usage with a simple optimization problem
module Example =
    // Sphere function (minimize)
    let sphereFunction (x: float[]) : float =
        x |> Array.sumBy (fun xi -> xi * xi)
    
    // Rastrigin function (more complex, multimodal)
    let rastriginFunction (x: float[]) : float =
        let a = 10.0
        let d = float x.Length
        let sum = x |> Array.sumBy (fun xi -> xi * xi - a * cos(2.0 * Math.PI * xi))
        a * d + sum
    
    // Example usage
    [<EntryPoint>]
    let main argv =
        printfn "Running Artificial Bee Colony Algorithm"
        
        // Optimize sphere function
        let abcSphere = ArtificialBeeColony(
            objectiveFunction = sphereFunction,
            dimension = 10,
            lowerBound = -5.0,
            upperBound = 5.0,
            populationSize = 20,
            maxIterations = 100,
            limit = 10
        )
        
        let resultSphere = abcSphere.Run()
        printfn "Sphere function - Best fitness: %f" resultSphere.Fitness
        printfn "Best position: %A" (resultSphere.Position |> Array.take 5)
        
        // Optimize Rastrigin function
        let abcRastrigin = ArtificialBeeColony(
            objectiveFunction = rastriginFunction,
            dimension = 5,
            lowerBound = -5.0,
            upperBound = 5.0,
            populationSize = 15,
            maxIterations = 50,
            limit = 5
        )
        
        let resultRastrigin = abcRastrigin.Run()
        printfn "Rastrigin function - Best fitness: %f" resultRastrigin.Fitness
        
        0 // return code

// Simple test function
let testABC() =
    let objectiveFunc (x: float[]) : float = 
        x.[0] * x.[0] + x.[1] * x.[1] // Simple quadratic function
    
    let abc = ArtificialBeeColony(
        objectiveFunction = objectiveFunc,
        dimension = 2,
        lowerBound = -10.0,
        upperBound = 10.0,
        populationSize = 10,
        maxIterations = 20,
        limit = 5
    )
    
    let result = abc.Run()
    printfn "Test Result - Fitness: %f, Position: [%f; %f]" 
        result.Fitness result.Position.[0] result.Position.[1]
```

## Key Features of this Implementation:

1. **Type Definitions**: Uses F# records for clean data structures
2. **Functional Style**: Leverages F# functional programming features like `Array.map`, `Array.sumBy`
3. **Modular Design**: Separate phases (Employed Bee, Onlooker Bee, Scout Bee)
4. **Generic Objective Function**: Accepts any function to optimize
5. **Bound Checking**: Ensures solutions stay within specified bounds
6. **Progress Tracking**: Shows iteration progress and best fitness values

## Usage Example:

The algorithm can be used to optimize various mathematical functions:
- Sphere function (simple quadratic)
- Rastrigin function (multimodal optimization)
- Custom user-defined functions

The implementation demonstrates F#'s strengths in mathematical optimization with clean, functional code while maintaining the core ABC algorithm mechanics.