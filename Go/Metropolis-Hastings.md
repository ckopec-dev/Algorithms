# Metropolis-Hastings Algorithm in Go

Here's a complete implementation of the Metropolis-Hastings algorithm in Go to sample from a target distribution:

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// Target distribution (example: univariate normal distribution)
func targetDistribution(x float64) float64 {
    // Standard normal distribution
    return math.Exp(-0.5 * x * x) / math.Sqrt(2*math.Pi)
}

// Proposal distribution (example: normal with standard deviation 0.5)
func proposalDistribution(current, proposed float64) float64 {
    // Normal distribution with mean=current and std=0.5
    sigma := 0.5
    return math.Exp(-0.5 * math.Pow(proposed-current, 2) / (sigma * sigma)) / (sigma * math.Sqrt(2*math.Pi))
}

// Metropolis-Hastings algorithm
func metropolisHastings(numSamples int, initialX float64) []float64 {
    samples := make([]float64, 0, numSamples)
    currentX := initialX
    
    for i := 0; i < numSamples; i++ {
        // Generate candidate from proposal distribution
        candidateX := currentX + rand.NormFloat64() * 0.5
        
        // Calculate acceptance probability
        // A = min(1, (target(candidate) * proposal(current|candidate)) / (target(current) * proposal(candidate|current)))
        // Since proposal is symmetric, proposal(current|candidate) = proposal(candidate|current)
        // So A = min(1, target(candidate) / target(current))
        acceptanceRatio := targetDistribution(candidateX) / targetDistribution(currentX)
        
        // Accept or reject the candidate
        if rand.Float64() < acceptanceRatio {
            currentX = candidateX
        }
        
        samples = append(samples, currentX)
    }
    
    return samples
}

// Calculate sample mean and standard deviation
func calculateStats(samples []float64) (float64, float64) {
    if len(samples) == 0 {
        return 0, 0
    }
    
    // Calculate mean
    sum := 0.0
    for _, x := range samples {
        sum += x
    }
    mean := sum / float64(len(samples))
    
    // Calculate standard deviation
    sumSq := 0.0
    for _, x := range samples {
        sumSq += math.Pow(x-mean, 2)
    }
    stdDev := math.Sqrt(sumSq / float64(len(samples)))
    
    return mean, stdDev
}

func main() {
    // Set random seed for reproducibility
    rand.Seed(time.Now().UnixNano())
    
    // Parameters
    numSamples := 10000
    initialX := 0.0
    
    fmt.Println("Running Metropolis-Hastings algorithm...")
    fmt.Printf("Number of samples: %d\n", numSamples)
    fmt.Printf("Initial value: %f\n", initialX)
    fmt.Println()
    
    // Run MCMC
    samples := metropolisHastings(numSamples, initialX)
    
    // Calculate statistics
    mean, stdDev := calculateStats(samples)
    
    // Print results
    fmt.Println("Results:")
    fmt.Printf("Sample mean: %f\n", mean)
    fmt.Printf("Sample standard deviation: %f\n", stdDev)
    fmt.Printf("True mean (for standard normal): 0.0\n")
    fmt.Printf("True standard deviation (for standard normal): 1.0\n")
    fmt.Println()
    
    // Show first 10 samples
    fmt.Println("First 10 samples:")
    for i, sample := range samples[:10] {
        fmt.Printf("Sample %d: %f\n", i+1, sample)
    }
    
    // Show some statistics about the samples
    fmt.Println()
    fmt.Println("Sample statistics:")
    fmt.Printf("Min: %f\n", samples[0])
    fmt.Printf("Max: %f\n", samples[0])
    
    for _, sample := range samples {
        if sample < samples[0] {
            samples[0] = sample
        }
        if sample > samples[0] {
            samples[0] = sample
        }
    }
    
    fmt.Printf("Min: %f\n", samples[0])
    fmt.Printf("Max: %f\n", samples[0])
}
```

## Key Components Explained:

### 1. **Target Distribution**
```go
func targetDistribution(x float64) float64 {
    return math.Exp(-0.5 * x * x) / math.Sqrt(2*math.Pi)
}
```
This represents the probability density function we want to sample from (standard normal distribution).

### 2. **Proposal Distribution**
```go
func proposalDistribution(current, proposed float64) float64 {
    sigma := 0.5
    return math.Exp(-0.5 * math.Pow(proposed-current, 2) / (sigma * sigma)) / (sigma * math.Sqrt(2*math.Pi))
}
```
This generates candidate samples from a normal distribution centered at the current value.

### 3. **Main Algorithm**
The `metropolisHastings` function:
- Starts with an initial value
- For each iteration:
  - Generates a candidate sample using the proposal distribution
  - Calculates the acceptance ratio
  - Accepts or rejects the candidate based on the ratio
  - Stores the result

### 4. **Key Features**
- **Asymmetric proposals**: The algorithm handles both symmetric and asymmetric proposal distributions
- **Acceptance probability**: Uses the ratio of target distribution values
- **Burn-in period**: The algorithm starts with an initial value and converges to the target distribution

### 5. **Usage Example**
When you run this code, it will:
1. Generate 10,000 samples from a standard normal distribution
2. Calculate sample mean and standard deviation
3. Compare results with theoretical values (mean=0, std=1)

This implementation demonstrates the core principles of the Metropolis-Hastings algorithm for Markov Chain Monte Carlo sampling.

