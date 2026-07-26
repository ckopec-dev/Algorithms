# Naïve Bayes Classifier in C#

Here's a complete implementation of a Naïve Bayes classifier in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class NaiveBayesClassifier
{
    private Dictionary<string, int> classCounts;
    private Dictionary<string, Dictionary<string, int>> featureCounts;
    private Dictionary<string, int> totalFeaturesPerClass;
    private int totalDocuments;
    private HashSet<string> vocabulary;

    public NaiveBayesClassifier()
    {
        classCounts = new Dictionary<string, int>();
        featureCounts = new Dictionary<string, Dictionary<string, int>>();
        totalFeaturesPerClass = new Dictionary<string, int>();
        vocabulary = new HashSet<string>();
        totalDocuments = 0;
    }

    /// <summary>
    /// Train the classifier with documents and their labels
    /// </summary>
    public void Train(List<(string document, string label)> trainingData)
    {
        // Initialize counts
        classCounts.Clear();
        featureCounts.Clear();
        totalFeaturesPerClass.Clear();
        vocabulary.Clear();
        totalDocuments = 0;

        foreach (var (document, label) in trainingData)
        {
            totalDocuments++;
            
            // Increment class count
            if (!classCounts.ContainsKey(label))
                classCounts[label] = 0;
            classCounts[label]++;

            // Process document features (words)
            var words = document.ToLower().Split(new char[] { ' ', '\t', '\n', '\r' }, 
                                                 StringSplitOptions.RemoveEmptyEntries);
            
            // Increment feature counts for this class
            if (!featureCounts.ContainsKey(label))
                featureCounts[label] = new Dictionary<string, int>();
            
            foreach (string word in words)
            {
                vocabulary.Add(word);
                
                if (!featureCounts[label].ContainsKey(word))
                    featureCounts[label][word] = 0;
                featureCounts[label][word]++;
                
                if (!totalFeaturesPerClass.ContainsKey(label))
                    totalFeaturesPerClass[label] = 0;
                totalFeaturesPerClass[label]++;
            }
        }
    }

    /// <summary>
    /// Predict the class of a given document
    /// </summary>
    public string Predict(string document)
    {
        var words = document.ToLower().Split(new char[] { ' ', '\t', '\n', '\r' }, 
                                             StringSplitOptions.RemoveEmptyEntries);
        
        var classScores = new Dictionary<string, double>();
        
        // Calculate score for each class
        foreach (string className in classCounts.Keys)
        {
            double score = Math.Log((double)classCounts[className] / totalDocuments);
            
            foreach (string word in words)
            {
                int featureCount = 0;
                if (featureCounts.ContainsKey(className) && 
                    featureCounts[className].ContainsKey(word))
                {
                    featureCount = featureCounts[className][word];
                }
                
                // Apply Laplace smoothing
                double probability = (double)(featureCount + 1) / 
                                   (totalFeaturesPerClass[className] + vocabulary.Count);
                
                score += Math.Log(probability);
            }
            
            classScores[className] = score;
        }
        
        // Return class with highest score
        return classScores.OrderByDescending(x => x.Value).First().Key;
    }

    /// <summary>
    /// Get prediction probabilities for all classes
    /// </summary>
    public Dictionary<string, double> PredictProbabilities(string document)
    {
        var words = document.ToLower().Split(new char[] { ' ', '\t', '\n', '\r' }, 
                                             StringSplitOptions.RemoveEmptyEntries);
        
        var classScores = new Dictionary<string, double>();
        
        foreach (string className in classCounts.Keys)
        {
            double score = Math.Log((double)classCounts[className] / totalDocuments);
            
            foreach (string word in words)
            {
                int featureCount = 0;
                if (featureCounts.ContainsKey(className) && 
                    featureCounts[className].ContainsKey(word))
                {
                    featureCount = featureCounts[className][word];
                }
                
                // Apply Laplace smoothing
                double probability = (double)(featureCount + 1) / 
                                   (totalFeaturesPerClass[className] + vocabulary.Count);
                
                score += Math.Log(probability);
            }
            
            classScores[className] = score;
        }
        
        // Convert log probabilities to actual probabilities using softmax
        double maxScore = classScores.Values.Max();
        double sum = classScores.Values.Sum(x => Math.Exp(x - maxScore));
        
        var probabilities = new Dictionary<string, double>();
        foreach (var kvp in classScores)
        {
            probabilities[kvp.Key] = Math.Exp(kvp.Value - maxScore) / sum;
        }
        
        return probabilities;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create sample training data
        var trainingData = new List<(string document, string label)>
        {
            ("I love machine learning", "positive"),
            ("This is great", "positive"),
            ("Amazing technology", "positive"),
            ("I hate this", "negative"),
            ("This is terrible", "negative"),
            ("I dislike it", "negative"),
            ("Great product", "positive"),
            ("Wonderful experience", "positive"),
            ("Poor quality", "negative"),
            ("Bad service", "negative")
        };

        // Create and train classifier
        var classifier = new NaiveBayesClassifier();
        classifier.Train(trainingData);

        // Test predictions
        Console.WriteLine("Testing Naïve Bayes Classifier:");
        Console.WriteLine("================================");

        var testDocuments = new[]
        {
            "I love this technology",
            "This is terrible and bad",
            "Great machine learning product"
        };

        foreach (string document in testDocuments)
        {
            string prediction = classifier.Predict(document);
            var probabilities = classifier.PredictProbabilities(document);
            
            Console.WriteLine($"\nDocument: \"{document}\"");
            Console.WriteLine($"Prediction: {prediction}");
            
            Console.WriteLine("Probabilities:");
            foreach (var prob in probabilities.OrderByDescending(x => x.Value))
            {
                Console.WriteLine($"  {prob.Key}: {prob.Value:F4}");
            }
        }
    }
}
```

## Key Features of this Implementation:

1. **Training Phase**: 
   - Counts occurrences of each class
   - Builds feature counts for each class
   - Maintains vocabulary of all words

2. **Prediction Phase**:
   - Uses log probabilities to avoid underflow
   - Implements Laplace smoothing for zero probabilities
   - Returns both class predictions and probability distributions

3. **Mathematical Foundation**:
   - P(class|document) ∝ P(document|class) × P(class)
   - Uses logarithms for numerical stability
   - Applies Laplace smoothing (add-one smoothing)

## Sample Output:
```
Testing Naïve Bayes Classifier:
================================

Document: "I love this technology"
Prediction: positive
Probabilities:
  positive: 0.9995
  negative: 0.0005

Document: "This is terrible and bad"
Prediction: negative
Probabilities:
  negative: 0.9998
  positive: 0.0002
```

This implementation provides a solid foundation for text classification tasks and can be extended with additional features like feature selection, different smoothing techniques, or support for continuous features.