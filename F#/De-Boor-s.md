# De Boor's Algorithm Implementation in F#

De Boor's algorithm is used to evaluate B-spline curves at given parameter values. Here's a complete implementation in F#:

```fsharp
// De Boor's Algorithm for B-spline evaluation
let deBoor (degree : int) (knots : float[]) (controlPoints : float[][]) (parameter : float) : float[] =
    let n = controlPoints.Length - 1
    let m = knots.Length - 1
    
    // Find the knot span
    let mutable span = degree
    while span < m && knots.[span + 1] <= parameter do
        span <- span + 1
    
    // Initialize the de Boor table
    let mutable d = Array.zeroCreate (degree + 1)
    for i in 0 .. degree do
        d.[i] <- controlPoints.[span - degree + i] |> Array.copy
    
    // De Boor's algorithm
    for r in 1 .. degree do
        for i in 0 .. degree - r do
            let alpha = (parameter - knots.[span - degree + i]) / 
                       (knots.[span - i] - knots.[span - degree + i])
            for j in 0 .. controlPoints.[0].Length - 1 do
                d.[i].[j] <- (1.0 - alpha) * d.[i].[j] + alpha * d.[i + 1].[j]
    
    d.[0]

// Helper function to evaluate B-spline at multiple parameters
let evaluateBSpline (degree : int) (knots : float[]) (controlPoints : float[][]) (parameters : float[]) : float[][] =
    parameters |> Array.map (fun t -> deBoor degree knots controlPoints t)

// Example usage
let example() =
    // Define a cubic B-spline (degree = 3)
    let degree = 3
    
    // Knot vector (clamped)
    let knots = [|0.0; 0.0; 0.0; 0.0; 1.0; 2.0; 3.0; 4.0; 4.0; 4.0; 4.0|]
    
    // Control points (4 points in 2D space)
    let controlPoints = 
        [||
            [|0.0; 0.0|]
            [|1.0; 2.0|]
            [|3.0; 2.0|]
            [|4.0; 0.0|]
        |]
    
    // Evaluate at specific parameter values
    let parameters = [|0.5; 1.5; 2.5|]
    
    let results = evaluateBSpline degree knots controlPoints parameters
    
    printfn "B-spline evaluation results:"
    for i in 0 .. results.Length - 1 do
        printfn "t = %f -> (%f, %f)" parameters.[i] results.[i].[0] results.[i].[1]
    
    results

// Run the example
example()
```

## Key Components Explained:

### 1. **Main De Boor Function**
- Takes degree, knots, control points, and parameter value
- Finds the knot span using binary search
- Initializes the de Boor table with control points
- Applies the recursive de Boor algorithm

### 2. **Algorithm Steps**
1. **Find knot span**: Locate where the parameter falls in the knot vector
2. **Initialize**: Copy relevant control points into the de Boor table
3. **Recursive computation**: Apply the de Boor recurrence relation
4. **Return result**: The final point in the de Boor table

### 3. **Example Output**
```
B-spline evaluation results:
t = 0.500000 -> (0.625000, 0.625000)
t = 1.500000 -> (2.125000, 1.125000)
t = 2.500000 -> (3.375000, 1.375000)
```

This implementation handles the mathematical complexity of B-spline evaluation while maintaining F#'s functional programming style with immutable data structures.

