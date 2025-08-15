```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class AdaBoostExample
{
    // Simple decision stumps (one-level decision trees)
    public class DecisionStump
    {
        public int FeatureIndex { get; set; }
        public double Threshold { get; set; }
        public int Sign { get; set; } // +1 or -1
        public double Weight { get; set; }
        
        public DecisionStump(int featureIndex, double threshold, int sign)
        {
            FeatureIndex = featureIndex;
            Threshold = threshold;
            Sign = sign;
            Weight = 0.0;
        }
        
        public int Predict(double[] features)
        {
            return (features[FeatureIndex] * Sign > Threshold * Sign) ? 1 : -1;
        }
    }
    
    public class AdaBoostClassifier
    {
        private List<DecisionStump> weakClassifiers;
        private double[] sampleWeights;
        private int maxIterations;
        
        public AdaBoostClassifier(int maxIterations = 10)
        {
            this.maxIterations = maxIterations;
            this.weakClassifiers = new List<DecisionStump>();
        }
        
        public void Train(double[][] features, int[] labels)
        {
            int nSamples = features.Length;
            int nFeatures = features[0].Length;
            
            // Initialize sample weights
            sampleWeights = new double[nSamples];
            for (int i = 0; i < nSamples; i++)
            {
                sampleWeights[i] = 1.0 / nSamples;
            }
            
            // AdaBoost iterations
            for (int t = 0; t < maxIterations; t++)
            {
                // Find best weak classifier (decision stump)
                var bestStump = FindBestStump(features, labels);
                
                // Calculate error and classifier weight
                double error = CalculateError(features, labels, bestStump);
                double alpha = 0.5 * Math.Log((1.0 - error) / (error + 1e-10));
                bestStump.Weight = alpha;
                
                // Update sample weights
                UpdateWeights(features, labels, bestStump, alpha);
                
                // Add to ensemble
                weakClassifiers.Add(bestStump);
            }
        }
        
        private DecisionStump FindBestStump(double[][] features, int[] labels)
        {
            int nSamples = features.Length;
            int nFeatures = features[0].Length;
            double bestError = double.MaxValue;
            DecisionStump bestStump = null;
            
            // Try all features and thresholds
            for (int f = 0; f < nFeatures; f++)
            {
                // Get unique values for this feature
                var values = features.Select(row => row[f]).Distinct().OrderBy(x => x).ToArray();
                
                // Try different thresholds between consecutive values
                for (int i = 0; i < values.Length - 1; i++)
                {
                    double threshold = (values[i] + values[i + 1]) / 2.0;
                    
                    // Try both signs (+1 and -1)
                    foreach (int sign in new int[] { 1, -1 })
                    {
                        var stump = new DecisionStump(f, threshold, sign);
                        double error = CalculateError(features, labels, stump);
                        
                        if (error < bestError)
                        {
                            bestError = error;
                            bestStump = stump;
                        }
                    }
                }
            }
            
            return bestStump;
        }
        
        private double CalculateError(double[][] features, int[] labels, DecisionStump stump)
        {
            double error = 0.0;
            for (int i = 0; i < features.Length; i++)
            {
                int prediction = stump.Predict(features[i]);
                if (prediction != labels[i])
                {
                    error += sampleWeights[i];
                }
            }
            return error;
        }
        
        private void UpdateWeights(double[][] features, int[] labels, DecisionStump stump, double alpha)
        {
            double Z = 0.0;
            
            for (int i = 0; i < features.Length; i++)
            {
                int prediction = stump.Predict(features[i]);
                double weight = sampleWeights[i] * Math.Exp(-alpha * labels[i] * prediction);
                sampleWeights[i] = weight;
                Z += weight;
            }
            
            // Normalize weights
            for (int i = 0; i < sampleWeights.Length; i++)
            {
                sampleWeights[i] /= Z;
            }
        }
        
        public int Predict(double[] features)
        {
            double score = 0.0;
            
            foreach (var classifier in weakClassifiers)
            {
                score += classifier.Weight * classifier.Predict(features);
            }
            
            return score >= 0 ? 1 : -1;
        }
    }
    
    // Example usage
    public static void Main()
    {
        // Sample dataset: [feature1, feature2]
        double[][] features = new double[][]
        {
            new double[] { 1.0, 2.0 },
            new double[] { 2.0, 3.0 },
            new double[] { 3.0, 1.0 },
            new double[] { 4.0, 2.0 },
            new double[] { 5.0, 3.0 },
            new double[] { 6.0, 1.0 }
        };
        
        // Labels: +1 or -1
        int[] labels = new int[] { -1, -1, 1, 1, 1, -1 };
        
        // Create and train AdaBoost classifier
        var adaBoost = new AdaBoostClassifier(5);
        adaBoost.Train(features, labels);
        
        // Test predictions
        Console.WriteLine("Testing predictions:");
        for (int i = 0; i < features.Length; i++)
        {
            int prediction = adaBoost.Predict(features[i]);
            Console.WriteLine($"Sample {i + 1}: Features [{string.Join(", ", features[i])}] -> Prediction: {prediction}, Actual: {labels[i]}");
        }
    }
}
```

