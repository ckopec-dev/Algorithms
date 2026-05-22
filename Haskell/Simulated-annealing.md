# Simulated Annealing Algorithm in Haskell

Here's a complete implementation of the Simulated Annealing algorithm in Haskell:

```haskell
{-# LANGUAGE TupleSections #-}

import System.Random
import Control.Monad
import Data.List (sortBy)
import Data.Ord (comparing)

-- | State representation - a simple 2D point
type Point = (Double, Double)

-- | Objective function to minimize (example: distance from origin)
objective :: Point -> Double
objective (x, y) = sqrt (x^2 + y^2)

-- | Neighbor function - generate a neighboring point
neighbor :: Point -> IO Point
neighbor (x, y) = do
    dx <- randomR (-0.5, 0.5)
    dy <- randomR (-0.5, 0.5)
    return (x + dx, y + dy)

-- | Simulated Annealing implementation
simulatedAnnealing :: Point -> Double -> Double -> Int -> IO Point
simulatedAnnealing initial temp decayFactor maxIterations = do
    current <- return initial
    currentEnergy <- return (objective current)
    finalState <- saLoop current currentEnergy temp decayFactor maxIterations
    return finalState
  where
    saLoop :: Point -> Double -> Double -> Double -> Int -> IO Point
    saLoop current currentEnergy temp decayFactor 0 = return current
    saLoop current currentEnergy temp decayFactor iterations = do
        -- Generate neighbor
        next <- neighbor current
        
        -- Calculate energy difference
        nextEnergy <- return (objective next)
        deltaEnergy = nextEnergy - currentEnergy
        
        -- Accept or reject based on Metropolis criterion
        accept <- if deltaEnergy < 0
            then return True
            else do
                probability <- randomR (0, 1)
                return (probability < exp (-deltaEnergy / temp))
        
        -- Update temperature
        newTemp = temp * decayFactor
        
        if accept
            then saLoop next nextEnergy newTemp decayFactor (iterations - 1)
            else saLoop current currentEnergy newTemp decayFactor (iterations - 1)

-- | Enhanced version with cooling schedule
simulatedAnnealingWithSchedule :: Point -> Double -> (Double -> Double) -> Int -> IO Point
simulatedAnnealingWithSchedule initial temp schedule maxIterations = do
    current <- return initial
    currentEnergy <- return (objective current)
    finalState <- saLoop current currentEnergy temp schedule maxIterations
    return finalState
  where
    saLoop :: Point -> Double -> Double -> (Double -> Double) -> Int -> IO Point
    saLoop current currentEnergy temp schedule 0 = return current
    saLoop current currentEnergy temp schedule iterations = do
        next <- neighbor current
        nextEnergy <- return (objective next)
        deltaEnergy = nextEnergy - currentEnergy
        
        accept <- if deltaEnergy < 0
            then return True
            else do
                probability <- randomR (0, 1)
                return (probability < exp (-deltaEnergy / temp))
        
        -- Apply cooling schedule
        newTemp = schedule temp
        
        if accept
            then saLoop next nextEnergy newTemp schedule (iterations - 1)
            else saLoop current currentEnergy newTemp schedule (iterations - 1)

-- | Cooling schedules
linearCooling :: Double -> Double
linearCooling temp = max 0.0 (temp - 0.01)

exponentialCooling :: Double -> Double
exponentialCooling temp = temp * 0.95

logarithmicCooling :: Double -> Double
logarithmicCooling temp = temp / (1.0 + log (temp + 1))

-- | Example usage
main :: IO ()
main = do
    putStrLn "Simulated Annealing Example"
    putStrLn "============================"
    
    -- Starting point
    let start = (5.0, 5.0)
    
    -- Run with different cooling schedules
    result1 <- simulatedAnnealing start 100.0 0.95 1000
    result2 <- simulatedAnnealingWithSchedule start 100.0 linearCooling 1000
    result3 <- simulatedAnnealingWithSchedule start 100.0 exponentialCooling 1000
    
    putStrLn $ "Initial point: " ++ show start
    putStrLn $ "Final point (default): " ++ show result1
    putStrLn $ "Final point (linear cooling): " ++ show result2
    putStrLn $ "Final point (exponential cooling): " ++ show result3
    
    putStrLn $ "Initial energy: " ++ show (objective start)
    putStrLn $ "Final energy (default): " ++ show (objective result1)
    putStrLn $ "Final energy (linear cooling): " ++ show (objective result2)
    putStrLn $ "Final energy (exponential cooling): " ++ show (objective result3)
```

## Key Components Explained

### 1. **State Representation**
```haskell
type Point = (Double, Double)
```
- Simple 2D point representation

### 2. **Objective Function**
```haskell
objective :: Point -> Double
objective (x, y) = sqrt (x^2 + y^2)
```
- Minimizes distance from origin (simple example)

### 3. **Neighbor Generation**
```haskell
neighbor :: Point -> IO Point
neighbor (x, y) = do
    dx <- randomR (-0.5, 0.5)
    dy <- randomR (-0.5, 0.5)
    return (x + dx, y + dy)
```
- Generates random neighboring points

### 4. **Core Algorithm**
```haskell
saLoop :: Point -> Double -> Double -> Double -> Int -> IO Point
```
- Implements the Metropolis acceptance criterion
- Temperature decay and iteration control

## Usage Example

```haskell
-- Run with default parameters
result <- simulatedAnnealing (5.0, 5.0) 100.0 0.95 1000

-- Run with custom cooling schedule
result <- simulatedAnnealingWithSchedule (5.0, 5.0) 100.0 exponentialCooling 1000
```

This implementation demonstrates the core principles of Simulated Annealing:
- **Temperature-based acceptance**: Accept worse solutions with probability
- **Cooling schedule**: Gradually reduce temperature
- **Random neighborhood exploration**: Move to neighboring states
- **Stochastic optimization**: Handles complex optimization landscapes

