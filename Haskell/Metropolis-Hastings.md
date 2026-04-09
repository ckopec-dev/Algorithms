```haskell
{-# LANGUAGE TupleSections #-}

import System.Random
import Control.Monad.State
import Data.List (scanl')

-- Metropolis-Hastings algorithm implementation
metropolisHastings :: (RandomGen g) 
                   => (Double -> Double)  -- target distribution (up to normalization)
                   -> (Double -> Double)  -- proposal distribution (normal)
                   -> Double              -- initial value
                   -> Int                 -- number of samples
                   -> g                   -- random generator
                   -> ([Double], g)       -- (samples, final generator state)
metropolisHastings target proposal x0 nSamples gen0 = 
    let samples = map fst $ take nSamples $ 
                  scanl metropolisStep (x0, gen0)
    in (samples, snd $ last $ take nSamples $ 
        scanl metropolisStep (x0, gen0))
  where
    metropolisStep (x, gen) = 
        let (y, gen1) = nextProposal gen  -- generate candidate
            alpha = min 1.0 (target y / target x)  -- acceptance ratio
            (u, gen2) = random gen1
        in if u <= alpha 
           then (y, gen2)  -- accept
           else (x, gen2)  -- reject

-- Generate next proposal using normal distribution
nextProposal :: (RandomGen g) => g -> (Double, g)
nextProposal gen = 
    let (u, gen1) = random gen
        (v, gen2) = random gen1
        -- Box-Muller transform for normal distribution
        z = sqrt (-2.0 * log u) * cos (2.0 * pi * v)
    in (z + 1.0, gen2)  -- mean=1.0, std=1.0

-- Example: Sampling from a target distribution
-- Let's sample from a mixture of two Gaussians
targetDistribution :: Double -> Double
targetDistribution x = 0.3 * exp (-0.5 * (x - 2.0)^2) + 
                       0.7 * exp (-0.5 * (x + 1.0)^2)

-- Simple normal proposal distribution with std=0.5
proposalDistribution :: Double -> Double
proposalDistribution x = 0.5 * exp (-0.5 * (x - 1.0)^2)

-- Main function to demonstrate the algorithm
main :: IO ()
main = do
    gen <- newStdGen
    let (samples, _) = metropolisHastings targetDistribution 
                       proposalDistribution 
                       0.0  -- initial value
                       1000  -- number of samples
                       gen
    
    putStrLn "First 20 samples from Metropolis-Hastings:"
    mapM_ (putStrLn . show) $ take 20 samples
    
    putStrLn "\nSample statistics:"
    let mean = sum samples / fromIntegral (length samples)
        variance = sum [ (x - mean)^2 | x <- samples ] / fromIntegral (length samples)
    putStrLn $ "Mean: " ++ show mean
    putStrLn $ "Variance: " ++ show variance

-- Alternative implementation with more explicit state handling
mhWithState :: (Double -> Double) -> Double -> Int -> State StdGen [Double]
mhWithState target x0 n = do
    let init = (x0, 0, [])
    let final = scanl' step init [1..n]
    return $ reverse $ map (\(_, _, acc) -> acc) final
  where
    step (x, i, acc) _ = 
        let y = x + 0.5 * (randomR (-1, 1) :: (Double, StdGen))  -- proposal
            alpha = min 1.0 (target y / target x)
            u = fst $ random (mkStdGen i)  -- use i as seed for consistency
        in if u <= alpha 
           then (y, i+1, y:acc)
           else (x, i+1, x:acc)

-- Run the alternative version
mainAlternative :: IO ()
mainAlternative = do
    gen <- newStdGen
    let samples = evalState (mhWithState targetDistribution 0.0 1000) gen
    putStrLn "\nAlternative implementation - First 10 samples:"
    mapM_ (putStrLn . show) $ take 10 samples
```

This Haskell implementation demonstrates the Metropolis-Hastings algorithm with:

1. **Core Function**: `metropolisHastings` that takes a target distribution, proposal distribution, initial value, number of samples, and random generator
2. **Sampling Process**: Generates samples from a mixture of two Gaussians
3. **Proposal Distribution**: Uses a normal distribution with mean 1.0 and std 0.5
4. **Acceptance Criteria**: Computes acceptance ratio and accepts/rejects based on uniform random variable
5. **Two Implementations**: 
   - Functional approach using `scanl`
   - State-based approach using `Control.Monad.State`

The example samples from a mixture of two Gaussian distributions and outputs sample statistics to verify the algorithm works correctly.

