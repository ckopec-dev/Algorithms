# Expectation-Maximization (EM) Algorithm in C#

Here's a complete implementation of the EM algorithm for Gaussian Mixture Models (GMM) in C#:

```csharp
using System;
using System.Linq;
using System.Collections.Generic;
using MathNet.Numerics.LinearAlgebra;

public class GaussianMixtureModel
{
    public class GmmResult
    {
        public double[][] Means { get; set; }
        public double[][] Covariances { get; set; }
        public double[] Weights { get; set; }
        public double LogLikelihood { get; set; }
    }

    private int numComponents;
    private int numFeatures;
    private double[][] means;
    private double[][] covariances;
    private double[] weights;
    private double tolerance = 1e-6;
    private int maxIterations = 100;

    public GaussianMixtureModel(int numComponents, int numFeatures)
    {
        this.numComponents = numComponents;
        this.numFeatures = numFeatures;
        
        // Initialize parameters randomly
        InitializeParameters();
    }

    private void InitializeParameters()
    {
        means = new double[numComponents][];
        covariances = new double[numComponents][];
        weights = new double[numComponents];

        Random random = new Random(42); // Fixed seed for reproducibility

        for (int i = 0; i < numComponents; i++)
        {
            means[i] = new double[numFeatures];
            covariances[i] = new double[numFeatures];
            
            // Initialize means
            for (int j = 0; j < numFeatures; j++)
            {
                means[i][j] = random.NextDouble() * 10;
            }
            
            // Initialize covariance (diagonal matrix)
            for (int j = 0; j < numFeatures; j++)
            {
                covariances[i][j] = random.NextDouble() * 2 + 0.1;
            }
            
            weights[i] = 1.0 / numComponents;
        }
    }

    public GmmResult Fit(double[][] data)
    {
        double prevLogLikelihood = double.NegativeInfinity;
        double logLikelihood = 0;

        for (int iteration = 0; iteration < maxIterations; iteration++)
        {
            // E-step: Compute responsibilities
            double[][] responsibilities = EStep(data);

            // M-step: Update parameters
            MStep(data, responsibilities);

            // Compute log-likelihood
            logLikelihood = ComputeLogLikelihood(data);
            
            Console.WriteLine($"Iteration {iteration + 1}: Log-likelihood = {logLikelihood:F6}");

            // Check for convergence
            if (Math.Abs(logLikelihood - prevLogLikelihood) < tolerance)
            {
                Console.WriteLine("Converged!");
                break;
            }

            prevLogLikelihood = logLikelihood;
        }

        return new GmmResult
        {
            Means = means,
            Covariances = covariances,
            Weights = weights,
            LogLikelihood = logLikelihood
        };
    }

    private double[][] EStep(double[][] data)
    {
        int numDataPoints = data.Length;
        double[][] responsibilities = new double[numDataPoints][];

        for (int i = 0; i < numDataPoints; i++)
        {
            responsibilities[i] = new double[numComponents];
            double total = 0;

            for (int j = 0; j < numComponents; j++)
            {
                responsibilities[i][j] = weights[j] * MultivariateNormalPdf(data[i], means[j], covariances[j]);
                total += responsibilities[i][j];
            }

            // Normalize
            if (total > 0)
            {
                for (int j = 0; j < numComponents; j++)
                {
                    responsibilities[i][j] /= total;
                }
            }
        }

        return responsibilities;
    }

    private void MStep(double[][] data, double[][] responsibilities)
    {
        int numDataPoints = data.Length;

        // Update weights
        for (int j = 0; j < numComponents; j++)
        {
            double weightSum = 0;
            for (int i = 0; i < numDataPoints; i++)
            {
                weightSum += responsibilities[i][j];
            }
            weights[j] = weightSum / numDataPoints;
        }

        // Update means
        for (int j = 0; j < numComponents; j++)
        {
            double[] newMean = new double[numFeatures];
            double weightSum = 0;

            for (int i = 0; i < numDataPoints; i++)
            {
                double responsibility = responsibilities[i][j];
                weightSum += responsibility;
                
                for (int k = 0; k < numFeatures; k++)
                {
                    newMean[k] += responsibility * data[i][k];
                }
            }

            if (weightSum > 0)
            {
                for (int k = 0; k < numFeatures; k++)
                {
                    means[j][k] = newMean[k] / weightSum;
                }
            }
        }

        // Update covariances
        for (int j = 0; j < numComponents; j++)
        {
            double[][] covariance = new double[numFeatures][];
            for (int k = 0; k < numFeatures; k++)
            {
                covariance[k] = new double[numFeatures];
            }

            double weightSum = 0;

            for (int i = 0; i < numDataPoints; i++)
            {
                double responsibility = responsibilities[i][j];
                weightSum += responsibility;

                double[] diff = new double[numFeatures];
                for (int k = 0; k < numFeatures; k++)
                {
                    diff[k] = data[i][k] - means[j][k];
                }

                for (int k1 = 0; k1 < numFeatures; k1++)
                {
                    for (int k2 = 0; k2 < numFeatures; k2++)
                    {
                        covariance[k1][k2] += responsibility * diff[k1] * diff[k2];
                    }
                }
            }

            if (weightSum > 0)
            {
                for (int k1 = 0; k1 < numFeatures; k1++)
                {
                    for (int k2 = 0; k2 < numFeatures; k2++)
                    {
                        covariance[k1][k2] /= weightSum;
                    }
                }
            }

            // Store diagonal elements only (simplified)
            for (int k = 0; k < numFeatures; k++)
            {
                covariances[j][k] = covariance[k][k];
            }
        }
    }

    private double MultivariateNormalPdf(double[] x, double[] mean, double[] covariance)
    {
        int n = x.Length;
        double exponent = 0;
        double det = 1;

        for (int i = 0; i < n; i++)
        {
            double diff = x[i] - mean[i];
            exponent -= (diff * diff) / (2 * covariance[i]);
            det *= covariance[i];
        }

        double coefficient = 1.0 / Math.Sqrt(Math.Pow(2 * Math.PI, n) * det);
        return coefficient * Math.Exp(exponent);
    }

    private double ComputeLogLikelihood(double[][] data)
    {
        double logLikelihood = 0;
        
        foreach (double[] point in data)
        {
            double sum = 0;
            for (int j = 0; j < numComponents; j++)
            {
                sum += weights[j] * MultivariateNormalPdf(point, means[j], covariances[j]);
            }
            logLikelihood += Math.Log(sum);
        }

        return logLikelihood;
    }
}

// Example usage
class Program
{
    static void Main(string[] args)
    {
        // Generate sample data (2D points)
        double[][] sampleData = new double[][]
        {
            new double[] {1.0, 2.0},
            new double[] {1.5, 1.8},
            new double[] {5.0, 8.0},
            new double[] {8.0, 8.0},
            new double[] {1.2, 0.5},
            new double[] {9.0, 11.0},
            new double[] {8.0, 2.0},
            new double[] {10.0, 2.0},
            new double[] {9.0, 3.0}
        };

        // Create and fit GMM
        Console.WriteLine("Fitting Gaussian Mixture Model...");
        var gmm = new GaussianMixtureModel(2, 2);
        var result = gmm.Fit(sampleData);

        // Display results
        Console.WriteLine("\nFinal Results:");
        Console.WriteLine($"Log-likelihood: {result.LogLikelihood:F6}");
        
        Console.WriteLine("\nMeans:");
        for (int i = 0; i < result.Means.Length; i++)
        {
            Console.WriteLine($"Component {i}: [{string.Join(", ", result.Means[i].Select(x => x.ToString("F3"))) }]");
        }
        
        Console.WriteLine("\nCovariances:");
        for (int i = 0; i < result.Covariances.Length; i++)
        {
            Console.WriteLine($"Component {i}: [{string.Join(", ", result.Covariances[i].Select(x => x.ToString("F3"))) }]"); 
        }
        
        Console.WriteLine("\nWeights:");
        Console.WriteLine($"[{string.Join(", ", result.Weights.Select(w => w.ToString("F3"))) }]");
    }
}
```

## Key Components of the EM Algorithm Implementation:

### 1. **E-step (Expectation)**
- Computes responsibilities (posterior probabilities)
- Calculates how much each data point belongs to each Gaussian component

### 2. **M-step (Maximization)**
- Updates model parameters:
  - Means: weighted average of data points
  - Covariances: weighted variance of data points
  - Weights: proportion of data assigned to each component

### 3. **Convergence Check**
- Stops when log-likelihood change is below tolerance
- Prevents infinite loops with maximum iteration limit

## Usage Example:
The code demonstrates fitting a 2-component GMM to sample 2D data points, showing the iterative process and final parameter estimates.

This implementation provides a solid foundation that can be extended for more complex scenarios like full covariance matrices or different probability distributions.