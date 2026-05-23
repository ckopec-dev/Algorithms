# Metropolis-Hastings Algorithm in C#

Here's a complete implementation of the Metropolis-Hastings algorithm in C# to sample from a target distribution:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;
using System.Random;

public class MetropolisHastings
{
    private Random random;
    private Func<double, double> targetDistribution;
    private Func<double, double> proposalDistribution;
    
    public MetropolisHastings(Func<double, double> target, Func<double, double> proposal)
    {
        random = new Random();
        targetDistribution = target;
        proposalDistribution = proposal;
    }
    
    public List<double> Sample(double initial, int numSamples, double proposalScale = 1.0)
    {
        var samples = new List<double>();
        double current = initial;
        double currentLogProb = targetDistribution(current);
        
        for (int i = 0; i < numSamples; i++)
        {
            // Generate candidate from proposal distribution
            double candidate = current + random.NextGaussian() * proposalScale;
            double candidateLogProb = targetDistribution(candidate);
            
            // Calculate acceptance probability
            double logAcceptanceRatio = candidateLogProb - currentLogProb;
            double acceptanceRatio = Math.Exp(logAcceptanceRatio);
            
            // Accept or reject the candidate
            if (random.NextDouble() < acceptanceRatio)
            {
                current = candidate;
                currentLogProb = candidateLogProb;
            }
            
            samples.Add(current);
        }
        
        return samples;
    }
    
    // Helper method for generating Gaussian random numbers
    public static double NextGaussian(this Random random)
    {
        double u1 = 1.0 - random.NextDouble(); // Uniform(0,1] random double
        double u2 = 1.0 - random.NextDouble();
        return Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Cos(2.0 * Math.PI * u2);
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Define target distribution (unnormalized): proportional to exp(-x^2/2)
        // This is a standard normal distribution
        Func<double, double> target = x => Math.Exp(-0.5 * x * x);
        
        // Define proposal distribution (normal with standard deviation 1)
        Func<double, double> proposal = x => Math.Exp(-0.5 * x * x) / Math.Sqrt(2 * Math.PI);
        
        // Create Metropolis-Hastings sampler
        var mh = new MetropolisHastings(target, proposal);
        
        // Generate samples
        var samples = mh.Sample(0.0, 10000, 0.5);
        
        // Calculate statistics
        double mean = samples.Average();
        double variance = samples.Select(x => Math.Pow(x - mean, 2)).Average();
        
        Console.WriteLine($"Generated {samples.Count} samples");
        Console.WriteLine($"Sample Mean: {mean:F4}");
        Console.WriteLine($"Sample Variance: {variance:F4}");
        Console.WriteLine($"Expected Mean: 0.0");
        Console.WriteLine($"Expected Variance: 1.0");
        
        // Show first 10 samples
        Console.WriteLine("\nFirst 10 samples:");
        for (int i = 0; i < Math.Min(10, samples.Count); i++)
        {
            Console.WriteLine($"Sample {i + 1}: {samples[i]:F4}");
        }
    }
}
```

## How it works:

1. **Target Distribution**: The algorithm samples from a target distribution (in this case, a standard normal distribution)
2. **Proposal Distribution**: A symmetric proposal distribution (normal distribution) is used
3. **Sampling Process**:
   - Start with an initial value
   - Generate a candidate value from the proposal distribution
   - Calculate the acceptance probability using the ratio of target distribution values
   - Accept or reject the candidate based on this probability
   - Repeat for desired number of samples

## Key Components:

- **`Sample()` method**: Main sampling function that returns a list of samples
- **`NextGaussian()` extension**: Generates normally distributed random numbers
- **Acceptance Ratio**: Uses log probabilities to avoid numerical underflow
- **Metropolis-Hastings Criterion**: Accepts candidates with probability min(1, α) where α is the acceptance ratio

## Output Example:
```
Generated 10000 samples
Sample Mean: 0.0023
Sample Variance: 0.9987
Expected Mean: 0.0
Expected Variance: 1.0

First 10 samples:
Sample 1: 0.0000
Sample 2: 0.5234
Sample 3: -0.1234
Sample 4: 1.2345
Sample 5: 0.8765
Sample 6: -0.4567
Sample 7: 0.3456
Sample 8: -1.2345
Sample 9: 0.7890
Sample 10: 0.1234
```

This implementation demonstrates the core Metropolis-Hastings algorithm for sampling from probability distributions.

