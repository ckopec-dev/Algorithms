```fsharp
// Monte Carlo Method Example: Estimating Pi
// This implementation uses random sampling to estimate the value of π

open System

// Monte Carlo method to estimate π
let estimatePi n =
    let rand = new Random()
    
    // Generate n random points in the unit square [0,1] x [0,1]
    let points = 
        [for i in 1..n do
            let x = rand.NextDouble()
            let y = rand.NextDouble()
            yield (x, y)]
    
    // Count how many points fall inside the unit circle
    let insideCircle = 
        points 
        |> List.filter (fun (x, y) -> x * x + y * y <= 1.0)
        |> List.length
    
    // π ≈ 4 * (points inside circle / total points)
    4.0 * float insideCircle / float n

// Alternative implementation using a more functional approach
let estimatePiFunctional n =
    let rand = new Random()
    
    // Generate random points and count those inside the circle
    [1..n]
    |> List.map (fun _ -> 
        let x = rand.NextDouble()
        let y = rand.NextDouble()
        if x * x + y * y <= 1.0 then 1 else 0)
    |> List.sum
    |> fun count -> 4.0 * float count / float n

// Example usage and comparison
let demonstrateMonteCarloPi () =
    printfn "Monte Carlo Method for Estimating Pi"
    printfn "=================================="
    
    let sampleSizes = [100; 1000; 10000; 100000; 1000000]
    
    for size in sampleSizes do
        let piEstimate = estimatePi size
        let error = abs (piEstimate - Math.PI)
        printfn "Points: %d | π ≈ %.6f | Error: %.6f" size piEstimate error
    
    printfn "\nActual π: %.6f" Math.PI

// More advanced Monte Carlo example: Estimating integral of sin(x) from 0 to π
let estimateIntegral n =
    let rand = new Random()
    
    // Integrate sin(x) from 0 to π using Monte Carlo
    // We'll use the fact that ∫₀^π sin(x) dx = 2
    let samples = 
        [for i in 1..n do
            let x = rand.NextDouble() * Math.PI  // Random x in [0, π]
            let y = rand.NextDouble()           // Random y in [0, 1]
            yield (x, y)]
    
    // Count points under the curve sin(x)
    let underCurve = 
        samples 
        |> List.filter (fun (x, y) -> y <= Math.Sin(x))
        |> List.length
    
    // Estimate the integral: (π * 1) * (points under curve / total points)
    let integralEstimate = float underCurve / float n * Math.PI
    integralEstimate

// Run the demonstrations
[<EntryPoint>]
let main argv =
    demonstrateMonteCarloPi ()
    
    printfn "\nIntegral Estimation Example:"
    printfn "=========================="
    
    let integralEstimates = [1000; 10000; 100000]
    for size in integralEstimates do
        let estimate = estimateIntegral size
        printfn "Samples: %d | Integral ≈ %.6f | Actual = 2.0" size estimate
    
    printfn "\nPress any key to exit..."
    Console.ReadKey() |> ignore
    0
```

This F# implementation demonstrates two Monte Carlo methods:

1. **Pi Estimation**: Uses random sampling to estimate π by determining the ratio of points inside a unit circle to total points in a unit square.

2. **Integral Estimation**: Estimates the integral of sin(x) from 0 to π using the same principle.

Key features of this implementation:
- Uses `System.Random` for generating random numbers
- Demonstrates functional programming patterns with `List.map`, `List.filter`, and `List.sum`
- Shows how Monte Carlo methods converge to true values as sample size increases
- Includes error analysis to demonstrate convergence
- Provides both procedural and functional approaches to the same problem

The algorithm works by:
1. Generating random points in a defined space
2. Counting how many fall into the region of interest
3. Using probability ratios to estimate the desired value
4. Scaling the result appropriately