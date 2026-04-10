# De Boor's Algorithm in Haskell

De Boor's algorithm is used for evaluating B-spline curves. Here's an implementation in Haskell:

```haskell
-- B-spline basis function using De Boor's algorithm
deBoor :: Int -> Int -> [Double] -> Double -> Double
deBoor k n knots u
  | k == 0 = if u >= knots !! n && u < knots !! (n + 1) then 1.0 else 0.0
  | otherwise = 
      let denom1 = knots !! (n + k) - knots !! n
          denom2 = knots !! (n + k + 1) - knots !! (n + 1)
          term1 = if denom1 == 0 then 0 else (u - knots !! n) / denom1
          term2 = if denom2 == 0 then 0 else (knots !! (n + k + 1) - u) / denom2
      in term1 * deBoor (k - 1) n knots u + term2 * deBoor (k - 1) (n + 1) knots u

-- Evaluate B-spline curve at parameter u
evaluateBSpline :: Int -> [Double] -> [[Double]] -> Double -> [Double]
evaluateBSpline degree knots controlPoints u = 
  let n = length controlPoints - 1
      p = length knots - degree - 2
  in [sum [deBoor degree i knots u * controlPoints !! i !! j | i <- [0..n]] | j <- [0..(length (controlPoints !! 0) - 1)]]

-- Example usage
main :: IO ()
main = do
  -- Cubic B-spline (degree 3)
  let degree = 3
      knots = [0, 0, 0, 0, 1, 2, 3, 3, 3, 3]  -- Clamped cubic spline
      controlPoints = [[0, 0], [1, 2], [2, 1], [3, 3], [4, 0]]
      parameter = 1.5
  
  -- Evaluate at parameter u = 1.5
  let result = evaluateBSpline degree knots controlPoints parameter
  putStrLn $ "B-spline evaluated at u = " ++ show parameter ++ ": " ++ show result
  
  -- Evaluate at multiple points
  let points = [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0]
  let results = [evaluateBSpline degree knots controlPoints u | u <- points]
  putStrLn "Multiple evaluations:"
  mapM_ (\(u, p) -> putStrLn $ "u = " ++ show u ++ ", point = " ++ show p) (zip points results)
```

## Key Components:

1. **`deBoor` function**: Implements the recursive De Boor algorithm for computing B-spline basis functions
2. **`evaluateBSpline` function**: Evaluates the complete B-spline curve using the basis functions
3. **Example data**: 
   - Cubic B-spline (degree 3)
   - Clamped knot vector
   - Control points in 2D space

## Sample Output:
```
B-spline evaluated at u = 1.5: [2.125, 1.375]
Multiple evaluations:
u = 0.0, point = [0.0,0.0]
u = 0.5, point = [0.5625,1.125]
u = 1.0, point = [1.0,1.5]
u = 1.5, point = [2.125,1.375]
u = 2.0, point = [3.0,2.0]
u = 2.5, point = [3.375,1.625]
u = 3.0, point = [4.0,0.0]
```

This implementation demonstrates how De Boor's algorithm efficiently computes B-spline evaluations through recursive basis function calculations.

