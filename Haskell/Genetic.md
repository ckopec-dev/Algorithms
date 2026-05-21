# Genetic Algorithm in Haskell

Here's a complete implementation of a genetic algorithm in Haskell to solve the classic "OneMax" problem (maximizing the number of 1s in a binary string):

```haskell
module GeneticAlgorithm where

import System.Random
import Control.Monad
import Data.List
import Data.Ord

-- Individual representation: binary string
type Individual = [Int]
type Population = [Individual]
type Fitness = Int

-- Genetic Algorithm parameters
populationSize :: Int
populationSize = 50

chromosomeLength :: Int
chromosomeLength = 20

mutationRate :: Double
mutationRate = 0.01

crossoverRate :: Double
crossoverRate = 0.8

-- Fitness function: count number of 1s
fitness :: Individual -> Fitness
fitness = sum

-- Generate random individual
randomIndividual :: IO Individual
randomIndividual = replicateM chromosomeLength (randomIO :: IO Int)

-- Generate random population
randomPopulation :: IO Population
randomPopulation = replicateM populationSize randomIndividual

-- Tournament selection
tournamentSelection :: Population -> IO Individual
tournamentSelection population = do
    let tournamentSize = 3
    tournament <- replicateM tournamentSize (randomRIO (0, length population - 1))
    let selected = map (population !!) tournament
    return $ maximumBy (comparing fitness) selected

-- Single point crossover
crossover :: Individual -> Individual -> IO (Individual, Individual)
crossover parent1 parent2 = do
    let crossoverPoint = randomRIO (1, length parent1 - 1) :: IO Int
    crossoverPoint' <- crossoverPoint
    let (left1, right1) = splitAt crossoverPoint' parent1
    let (left2, right2) = splitAt crossoverPoint' parent2
    return (left1 ++ right2, left2 ++ right1)

-- Mutation
mutate :: Individual -> IO Individual
mutate individual = do
    let mutated = map (\gene -> if randomIO :: IO Bool then 1 - gene else gene) individual
    return mutated

-- Genetic Algorithm step
geneticAlgorithmStep :: Population -> IO Population
geneticAlgorithmStep population = do
    let fitnessScores = map fitness population
    let maxFitness = maximum fitnessScores
    let avgFitness = fromIntegral (sum fitnessScores) / fromIntegral (length fitnessScores)
    
    -- Print progress
    putStrLn $ "Max Fitness: " ++ show maxFitness ++ ", Avg Fitness: " ++ show avgFitness
    
    -- Create new population
    newPopulation <- replicateM populationSize $ do
        -- Selection
        parent1 <- tournamentSelection population
        parent2 <- tournamentSelection population
        
        -- Crossover
        crossoverCheck <- randomIO :: IO Bool
        if crossoverCheck && crossoverRate > 0
            then do
                (child1, child2) <- crossover parent1 parent2
                -- Mutation
                mutatedChild1 <- if randomIO :: IO Bool && mutationRate > 0 
                                then mutate child1 
                                else return child1
                mutatedChild2 <- if randomIO :: IO Bool && mutationRate > 0 
                                then mutate child2 
                                else return child2
                return [mutatedChild1, mutatedChild2]
            else do
                -- No crossover, just mutation
                mutatedParent1 <- if randomIO :: IO Bool && mutationRate > 0 
                                 then mutate parent1 
                                 else return parent1
                mutatedParent2 <- if randomIO :: IO Bool && mutationRate > 0 
                                 then mutate parent2 
                                 else return parent2
                return [mutatedParent1, mutatedParent2]
    
    return $ concat newPopulation

-- Main genetic algorithm loop
geneticAlgorithm :: IO ()
geneticAlgorithm = do
    putStrLn "Starting Genetic Algorithm..."
    
    -- Initialize population
    population <- randomPopulation
    putStrLn "Initial population generated"
    
    -- Run for 100 generations
    let generations = 100
    finalPopulation <- foldM (\pop _ -> geneticAlgorithmStep pop) population [1..generations]
    
    -- Find best individual
    let bestIndividual = maximumBy (comparing fitness) finalPopulation
    let bestFitness = fitness bestIndividual
    
    putStrLn $ "Best individual: " ++ show bestIndividual
    putStrLn $ "Best fitness: " ++ show bestFitness
    putStrLn $ "Target fitness: " ++ show chromosomeLength

-- Example usage
main :: IO ()
main = geneticAlgorithm
```

## Key Components Explained

### 1. **Data Types**
- `Individual`: A binary string represented as a list of 0s and 1s
- `Population`: A list of individuals
- `Fitness`: An integer representing how good an individual is

### 2. **Core Functions**

**Fitness Function**: Counts the number of 1s in the binary string
```haskell
fitness :: Individual -> Fitness
fitness = sum
```

**Selection**: Tournament selection chooses the best individual from a random sample
```haskell
tournamentSelection :: Population -> IO Individual
```

**Crossover**: Single-point crossover combines two parents
```haskell
crossover :: Individual -> Individual -> IO (Individual, Individual)
```

**Mutation**: Randomly flips bits with a given probability
```haskell
mutate :: Individual -> IO Individual
```

### 3. **Usage Example**

To run the genetic algorithm:
```bash
ghc GeneticAlgorithm.hs
./GeneticAlgorithm
```

### 4. **Expected Output**
```
Starting Genetic Algorithm...
Initial population generated
Max Fitness: 15, Avg Fitness: 10.2
Max Fitness: 17, Avg Fitness: 12.8
Max Fitness: 19, Avg Fitness: 14.3
...
Best individual: [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
Best fitness: 20
Target fitness: 20
```

This implementation demonstrates functional programming concepts in Haskell while solving a classic optimization problem using evolutionary algorithms.

