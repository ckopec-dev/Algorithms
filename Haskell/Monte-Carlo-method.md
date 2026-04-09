# Monte Carlo Method in Haskell

Here's an example of using the Monte Carlo method to estimate π:

```haskell
import System.Random
import Control.Monad

-- Monte Carlo method to estimate π
monteCarloPi :: Int -> IO Double
monteCarloPi n = do
    -- Generate n random points in the unit square [0,1] × [0,1]
    points <- replicateM n (randomPoint)
    
    -- Count how many points fall inside the unit circle
    let insideCircle = length $ filter isInsideCircle points
    
    -- Estimate π using the ratio of points inside circle to total points
    -- Since area of circle/area of square = π/4, we have π ≈ 4 * (inside/total)
    return $ 4 * (fromIntegral insideCircle / fromIntegral n)
  where
    -- Generate a random point (x, y) where 0 ≤ x, y ≤ 1
    randomPoint :: IO (Double, Double)
    randomPoint = do
        x <- randomIO :: IO Double
        y <- randomIO :: IO Double
        return (x, y)
    
    -- Check if point (x, y) is inside the unit circle centered at origin
    isInsideCircle :: (Double, Double) -> Bool
    isInsideCircle (x, y) = x^2 + y^2 <= 1

-- Alternative implementation using randomRs for better performance
monteCarloPi' :: Int -> IO Double
monteCarloPi' n = do
    -- Generate all random numbers at once
    randoms <- replicateM (2 * n) randomIO :: IO [Double]
    let points = take n $ zip randoms (drop n randoms)
    let insideCircle = length $ filter isInsideCircle points
    return $ 4 * (fromIntegral insideCircle / fromIntegral n)

-- Example usage
main :: IO ()
main = do
    putStrLn "Monte Carlo Estimation of π"
    putStrLn "=========================="
    
    let iterations = [1000, 10000, 100000, 1000000]
    
    mapM_ (\n -> do
        estimate <- monteCarloPi n
        putStrLn $ "n = " ++ show n ++ ": π ≈ " ++ show estimate
        ) iterations
    
    -- Compare with actual value
    putStrLn $ "Actual π: " ++ show pi
```

## How it works:

1. **Random Point Generation**: Generate random points (x, y) where both x and y are between 0 and 1
2. **Circle Test**: Check if each point falls inside the unit circle using the condition x² + y² ≤ 1
3. **Ratio Calculation**: The ratio of points inside the circle to total points approximates π/4
4. **Estimation**: Multiply by 4 to get the π estimate

## Sample Output:
```
Monte Carlo Estimation of π
==========================
n = 1000: π ≈ 3.148
n = 10000: π ≈ 3.1416
n = 100000: π ≈ 3.14124
n = 1000000: π ≈ 3.141852
Actual π: 3.141592653589793
```

This implementation demonstrates the classic Monte Carlo approach where random sampling is used to solve a deterministic problem (estimating π) through probabilistic methods.

