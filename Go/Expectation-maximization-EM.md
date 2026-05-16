# Expectation-Maximization (EM) Algorithm in Go

Here's a complete implementation of the EM algorithm for Gaussian Mixture Model (GMM) clustering in Go:

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// Point represents a 2D data point
type Point struct {
    X, Y float64
}

// Gaussian represents a Gaussian distribution
type Gaussian struct {
    MeanX, MeanY float64
    StdX, StdY   float64
    Weight       float64
}

// GMM represents a Gaussian Mixture Model
type GMM struct {
    Gaussians []Gaussian
    K         int // number of components
}

// initializeGMM initializes the GMM with random parameters
func (gmm *GMM) initializeGMM(data []Point) {
    rand.Seed(time.Now().UnixNano())
    
    for i := 0; i < gmm.K; i++ {
        gmm.Gaussians[i] = Gaussian{
            MeanX:  rand.Float64() * 10,
            MeanY:  rand.Float64() * 10,
            StdX:   rand.Float64() * 2 + 0.5,
            StdY:   rand.Float64() * 2 + 0.5,
            Weight: 1.0 / float64(gmm.K),
        }
    }
}

// gaussianPDF calculates the probability density function of a 2D Gaussian
func gaussianPDF(x, y, meanX, meanY, stdX, stdY float64) float64 {
    exp1 := math.Exp(-0.5 * math.Pow((x-meanX)/stdX, 2))
    exp2 := math.Exp(-0.5 * math.Pow((y-meanY)/stdY, 2))
    return (1.0 / (2 * math.Pi * stdX * stdY)) * exp1 * exp2
}

// eStep performs the Expectation step
func (gmm *GMM) eStep(data []Point) [][]float64 {
    responsibilities := make([][]float64, len(data))
    
    for i, point := range data {
        responsibilities[i] = make([]float64, gmm.K)
        total := 0.0
        
        for j := 0; j < gmm.K; j++ {
            g := gmm.Gaussians[j]
            prob := g.Weight * gaussianPDF(point.X, point.Y, g.MeanX, g.MeanY, g.StdX, g.StdY)
            responsibilities[i][j] = prob
            total += prob
        }
        
        // Normalize responsibilities
        if total > 0 {
            for j := 0; j < gmm.K; j++ {
                responsibilities[i][j] /= total
            }
        }
    }
    
    return responsibilities
}

// mStep performs the Maximization step
func (gmm *GMM) mStep(data []Point, responsibilities [][]float64) {
    N := len(data)
    
    // Update weights
    for j := 0; j < gmm.K; j++ {
        weightSum := 0.0
        for i := 0; i < N; i++ {
            weightSum += responsibilities[i][j]
        }
        gmm.Gaussians[j].Weight = weightSum / float64(N)
    }
    
    // Update means and standard deviations
    for j := 0; j < gmm.K; j++ {
        var sumX, sumY, sumXX, sumYY float64
        weightSum := 0.0
        
        for i := 0; i < N; i++ {
            weight := responsibilities[i][j]
            weightSum += weight
            sumX += weight * data[i].X
            sumY += weight * data[i].Y
            sumXX += weight * math.Pow(data[i].X, 2)
            sumYY += weight * math.Pow(data[i].Y, 2)
        }
        
        if weightSum > 0 {
            gmm.Gaussians[j].MeanX = sumX / weightSum
            gmm.Gaussians[j].MeanY = sumY / weightSum
            
            // Calculate variances
            gmm.Gaussians[j].StdX = math.Sqrt(sumXX/weightSum - math.Pow(gmm.Gaussians[j].MeanX, 2))
            gmm.Gaussians[j].StdY = math.Sqrt(sumYY/weightSum - math.Pow(gmm.Gaussians[j].MeanY, 2))
        }
    }
}

// logLikelihood calculates the log-likelihood of the data given the model
func (gmm *GMM) logLikelihood(data []Point) float64 {
    logLik := 0.0
    
    for _, point := range data {
        prob := 0.0
        for _, g := range gmm.Gaussians {
            prob += g.Weight * gaussianPDF(point.X, point.Y, g.MeanX, g.MeanY, g.StdX, g.StdY)
        }
        if prob > 0 {
            logLik += math.Log(prob)
        }
    }
    
    return logLik
}

// EMAlgorithm performs the complete EM algorithm
func (gmm *GMM) EMAlgorithm(data []Point, maxIterations int, tolerance float64) {
    prevLogLik := math.Inf(-1)
    
    for iteration := 0; iteration < maxIterations; iteration++ {
        // E-step
        responsibilities := gmm.eStep(data)
        
        // M-step
        gmm.mStep(data, responsibilities)
        
        // Calculate log-likelihood
        currentLogLik := gmm.logLikelihood(data)
        
        fmt.Printf("Iteration %d: Log-likelihood = %.4f\n", iteration+1, currentLogLik)
        
        // Check for convergence
        if math.Abs(currentLogLik-prevLogLik) < tolerance {
            fmt.Printf("Converged after %d iterations\n", iteration+1)
            break
        }
        
        prevLogLik = currentLogLik
    }
}

// generateSampleData creates sample 2D data points
func generateSampleData() []Point {
    data := make([]Point, 100)
    rand.Seed(time.Now().UnixNano())
    
    // Generate 3 clusters
    for i := 0; i < 30; i++ {
        data[i] = Point{
            X: rand.NormFloat64()*2 + 2,
            Y: rand.NormFloat64()*2 + 2,
        }
    }
    
    for i := 30; i < 60; i++ {
        data[i] = Point{
            X: rand.NormFloat64()*2 + 8,
            Y: rand.NormFloat64()*2 + 8,
        }
    }
    
    for i := 60; i < 100; i++ {
        data[i] = Point{
            X: rand.NormFloat64()*2 + 5,
            Y: rand.NormFloat64()*2 + 5,
        }
    }
    
    return data
}

func main() {
    // Generate sample data
    data := generateSampleData()
    
    // Create GMM with 3 components
    gmm := &GMM{
        Gaussians: make([]Gaussian, 3),
        K:         3,
    }
    
    // Initialize GMM
    gmm.initializeGMM(data)
    
    fmt.Println("Initial parameters:")
    for i, g := range gmm.Gaussians {
        fmt.Printf("Component %d: Weight=%.3f, Mean=(%.3f, %.3f), Std=(%.3f, %.3f)\n",
            i+1, g.Weight, g.MeanX, g.MeanY, g.StdX, g.StdY)
    }
    
    fmt.Println("\nStarting EM Algorithm...")
    fmt.Println("========================")
    
    // Run EM algorithm
    gmm.EMAlgorithm(data, 100, 1e-6)
    
    fmt.Println("\nFinal parameters:")
    for i, g := range gmm.Gaussians {
        fmt.Printf("Component %d: Weight=%.3f, Mean=(%.3f, %.3f), Std=(%.3f, %.3f)\n",
            i+1, g.Weight, g.MeanX, g.MeanY, g.StdX, g.StdY)
    }
}
```

## Key Components of the EM Algorithm Implementation:

### 1. **Data Structures**
- `Point`: Represents 2D data points
- `Gaussian`: Represents a 2D Gaussian distribution with mean, standard deviation, and weight
- `GMM`: Contains multiple Gaussian components

### 2. **E-step (Expectation)**
- Calculates responsibility values (posterior probabilities) of each data point belonging to each Gaussian component
- Uses the current model parameters to compute these probabilities

### 3. **M-step (Maximization)**
- Updates the model parameters (means, variances, weights) based on the responsibility values
- Uses weighted averages to update the parameters

### 4. **Convergence Check**
- Monitors the change in log-likelihood between iterations
- Stops when the change falls below a specified tolerance

## How to Run:
```bash
go run em_algorithm.go
```

## Expected Output:
The algorithm will show iterative updates of the Gaussian parameters and convergence when the change in log-likelihood becomes very small, demonstrating the classic EM algorithm behavior of monotonically increasing likelihood.

This implementation demonstrates the core principles of the EM algorithm: alternating between estimating the hidden variables (E-step) and maximizing the likelihood (M-step) until convergence.

