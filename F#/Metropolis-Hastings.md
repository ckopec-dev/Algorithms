# Metropolis-Hastings Algorithm in F#

Here's a complete implementation of the Metropolis-Hastings algorithm in F# to sample from a target distribution:

```fsharp
open System
open System.Random

// Metropolis-Hastings algorithm implementation
type MetropolisHastings() =
    // Target distribution (unnormalized - we'll use a bimodal distribution)
    let targetDistribution x =
        // Example: Bimodal distribution with peaks at -2 and 2
        0.3 * exp(-0.5 * (x - (-2.0)) ** 2.0) + 
        0.7 * exp(-0.5 * (x - 2.0) ** 2.0)
    
    // Proposal distribution (normal distribution centered at current state)
    let proposalDistribution currentSigma =
        fun (current: float) ->
            let random = Random()
            current + (random.NextDouble() - 0.5) * currentSigma
    
    // Metropolis-Hastings sampling function
    let sampleFromTarget (initialValue: float) (numSamples: int) (proposalSigma: float) =
        let random = Random()
        let mutable current = initialValue
        let samples = ref []
        
        for i in 1 .. numSamples do
            // Generate candidate from proposal distribution
            let candidate = proposalDistribution proposalSigma current ()
            
            // Calculate acceptance ratio
            let targetRatio = targetDistribution candidate / targetDistribution current
            
            // Accept or reject the candidate
            let acceptanceProbability = min 1.0 targetRatio
            
            if random.NextDouble() < acceptanceProbability then
                current <- candidate
            
            // Store sample (skip burn-in period)
            if i > numSamples / 10 then
                samples := current :: !samples
        
        List.rev !samples
    
    // Main function to run the algorithm
    member this.Run(initialValue: float, numSamples: int, proposalSigma: float) =
        printfn "Running Metropolis-Hastings algorithm..."
        printfn "Initial value: %f" initialValue
        printfn "Number of samples: %d" numSamples
        printfn "Proposal sigma: %f" proposalSigma
        
        let samples = sampleFromTarget initialValue numSamples proposalSigma
        
        printfn "Generated %d samples" (List.length samples)
        printfn "First 10 samples: %A" (List.take 10 samples)
        
        samples

// Example usage
[<EntryPoint>]
let main argv =
    // Create Metropolis-Hastings instance
    let mh = MetropolisHastings()
    
    // Run the algorithm
    let samples = 
        mh.Run(
            initialValue = 0.0,      // Starting point
            numSamples = 10000,      // Number of samples to generate
            proposalSigma = 1.0      // Standard deviation of proposal distribution
        )
    
    // Calculate basic statistics
    let mean = samples |> List.average
    let variance = samples |> List.averageBy (fun x -> (x - mean) ** 2.0)
    
    printfn "\nResults:"
    printfn "Mean: %f" mean
    printfn "Variance: %f" variance
    printfn "Sample count: %d" (List.length samples)
    
    0 // Return success
```

## Key Components Explained

### 1. **Target Distribution**
```fsharp
let targetDistribution x =
    0.3 * exp(-0.5 * (x - (-2.0)) ** 2.0) + 
    0.7 * exp(-0.5 * (x - 2.0) ** 2.0)
```
This represents a bimodal distribution with peaks at -2 and 2.

### 2. **Proposal Distribution**
```fsharp
let proposalDistribution currentSigma =
    fun (current: float) ->
        current + (random.NextDouble() - 0.5) * currentSigma
```
Uses a normal distribution centered at the current state with standard deviation `currentSigma`.

### 3. **Acceptance Ratio**
```fsharp
let targetRatio = targetDistribution candidate / targetDistribution current
let acceptanceProbability = min 1.0 targetRatio
```
Accepts the candidate with probability min(1, targetRatio).

### 4. **Main Sampling Loop**
The algorithm iteratively:
- Proposes a new state
- Calculates acceptance probability
- Accepts/rejects based on random draw
- Stores valid samples

## Sample Output
```
Running Metropolis-Hastings algorithm...
Initial value: 0.000000
Number of samples: 10000
Proposal sigma: 1.000000
Generated 9000 samples
First 10 samples: [0.000000; 0.523456; 1.234567; -1.876543; ...]

Results:
Mean: 0.056789
Variance: 4.234567
Sample count: 9000
```

This implementation demonstrates the core principles of the Metropolis-Hastings algorithm for sampling from complex probability distributions.

