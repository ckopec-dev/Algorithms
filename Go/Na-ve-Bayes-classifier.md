# Naïve Bayes Classifier in Go

Here's a complete implementation of a Naïve Bayes classifier in Go:

```go
package main

import (
    "fmt"
    "math"
    "strings"
)

// Feature represents a single feature with its possible values
type Feature struct {
    Name     string
    Values   []string
}

// Document represents a training sample with features and class
type Document struct {
    Features map[string]string
    Class    string
}

// NaiveBayesClassifier implements the Naïve Bayes algorithm
type NaiveBayesClassifier struct {
    features     []Feature
    classes      []string
    classCounts  map[string]int
    featureCounts map[string]map[string]int
    totalDocs    int
}

// NewNaiveBayesClassifier creates a new classifier instance
func NewNaiveBayesClassifier(features []Feature) *NaiveBayesClassifier {
    return &NaiveBayesClassifier{
        features:     features,
        classes:      []string{},
        classCounts:  make(map[string]int),
        featureCounts: make(map[string]map[string]int),
        totalDocs:    0,
    }
}

// Train the classifier with training data
func (nb *NaiveBayesClassifier) Train(documents []Document) {
    // Initialize class counts
    nb.classCounts = make(map[string]int)
    nb.featureCounts = make(map[string]map[string]int)
    
    for _, doc := range documents {
        // Count documents per class
        nb.classCounts[doc.Class]++
        nb.totalDocs++
        
        // Initialize feature counts for this class if not exists
        if _, exists := nb.featureCounts[doc.Class]; !exists {
            nb.featureCounts[doc.Class] = make(map[string]int)
        }
        
        // Count feature values for each class
        for featureName, featureValue := range doc.Features {
            if _, exists := nb.featureCounts[doc.Class][featureName]; !exists {
                nb.featureCounts[doc.Class][featureName] = make(map[string]int)
            }
            nb.featureCounts[doc.Class][featureName][featureValue]++
        }
    }
    
    // Get all unique classes
    for class := range nb.classCounts {
        nb.classes = append(nb.classes, class)
    }
}

// Predict the class for a given document
func (nb *NaiveBayesClassifier) Predict(features map[string]string) string {
    maxProb := -1.0
    predictedClass := ""
    
    for _, class := range nb.classes {
        // Calculate P(class)
        classProb := float64(nb.classCounts[class]) / float64(nb.totalDocs)
        
        // Calculate P(features|class)
        likelihood := 1.0
        for featureName, featureValue := range features {
            // Get count of this feature value for this class
            featureCount := 0
            if classFeatures, exists := nb.featureCounts[class]; exists {
                if featureValues, exists := classFeatures[featureName]; exists {
                    if count, exists := featureValues[featureValue]; exists {
                        featureCount = count
                    }
                }
            }
            
            // Apply Laplace smoothing
            totalFeatureValues := 0
            if classFeatures, exists := nb.featureCounts[class]; exists {
                if featureValues, exists := classFeatures[featureName]; exists {
                    for _, count := range featureValues {
                        totalFeatureValues += count
                    }
                }
            }
            
            // Get total possible values for this feature
            totalPossibleValues := 0
            for _, feature := range nb.features {
                if feature.Name == featureName {
                    totalPossibleValues = len(feature.Values)
                    break
                }
            }
            
            // Apply Laplace smoothing: (count + 1) / (total + vocabulary_size)
            smoothedProb := float64(featureCount+1) / float64(totalFeatureValues+totalPossibleValues)
            likelihood *= smoothedProb
        }
        
        // Calculate P(class|features) = P(features|class) * P(class)
        prob := classProb * likelihood
        
        if prob > maxProb {
            maxProb = prob
            predictedClass = class
        }
    }
    
    return predictedClass
}

// Calculate probability of a specific class given features
func (nb *NaiveBayesClassifier) Probability(features map[string]string, targetClass string) float64 {
    classProb := float64(nb.classCounts[targetClass]) / float64(nb.totalDocs)
    
    likelihood := 1.0
    for featureName, featureValue := range features {
        featureCount := 0
        if classFeatures, exists := nb.featureCounts[targetClass]; exists {
            if featureValues, exists := classFeatures[featureName]; exists {
                if count, exists := featureValues[featureValue]; exists {
                    featureCount = count
                }
            }
        }
        
        totalFeatureValues := 0
        if classFeatures, exists := nb.featureCounts[targetClass]; exists {
            if featureValues, exists := classFeatures[featureName]; exists {
                for _, count := range featureValues {
                    totalFeatureValues += count
                }
            }
        }
        
        totalPossibleValues := 0
        for _, feature := range nb.features {
            if feature.Name == featureName {
                totalPossibleValues = len(feature.Values)
                break
            }
        }
        
        smoothedProb := float64(featureCount+1) / float64(totalFeatureValues+totalPossibleValues)
        likelihood *= smoothedProb
    }
    
    return classProb * likelihood
}

func main() {
    // Define features with their possible values
    features := []Feature{
        {"Outlook", []string{"Sunny", "Overcast", "Rainy"}},
        {"Temperature", []string{"Hot", "Mild", "Cool"}},
        {"Humidity", []string{"High", "Normal"}},
        {"Wind", []string{"Strong", "Weak"}},
    }
    
    // Create classifier
    classifier := NewNaiveBayesClassifier(features)
    
    // Training data
    trainingData := []Document{
        {Features: map[string]string{"Outlook": "Sunny", "Temperature": "Hot", "Humidity": "High", "Wind": "Weak"}, Class: "No"},
        {Features: map[string]string{"Outlook": "Sunny", "Temperature": "Hot", "Humidity": "High", "Wind": "Strong"}, Class: "No"},
        {Features: map[string]string{"Outlook": "Overcast", "Temperature": "Hot", "Humidity": "High", "Wind": "Weak"}, Class: "Yes"},
        {Features: map[string]string{"Outlook": "Rainy", "Temperature": "Mild", "Humidity": "High", "Wind": "Weak"}, Class: "Yes"},
        {Features: map[string]string{"Outlook": "Rainy", "Temperature": "Cool", "Humidity": "Normal", "Wind": "Weak"}, Class: "Yes"},
        {Features: map[string]string{"Outlook": "Rainy", "Temperature": "Cool", "Humidity": "Normal", "Wind": "Strong"}, Class: "No"},
        {Features: map[string]string{"Outlook": "Overcast", "Temperature": "Cool", "Humidity": "Normal", "Wind": "Strong"}, Class: "Yes"},
        {Features: map[string]string{"Outlook": "Sunny", "Temperature": "Mild", "Humidity": "High", "Wind": "Weak"}, Class: "No"},
        {Features: map[string]string{"Outlook": "Sunny", "Temperature": "Cool", "Humidity": "Normal", "Wind": "Weak"}, Class: "Yes"},
        {Features: map[string]string{"Outlook": "Rainy", "Temperature": "Mild", "Humidity": "Normal", "Wind": "Weak"}, Class: "Yes"},
        {Features: map[string]string{"Outlook": "Sunny", "Temperature": "Mild", "Humidity": "Normal", "Wind": "Strong"}, Class: "Yes"},
        {Features: map[string]string{"Outlook": "Overcast", "Temperature": "Mild", "Humidity": "High", "Wind": "Strong"}, Class: "Yes"},
        {Features: map[string]string{"Outlook": "Overcast", "Temperature": "Hot", "Humidity": "Normal", "Wind": "Weak"}, Class: "Yes"},
        {Features: map[string]string{"Outlook": "Rainy", "Temperature": "Mild", "Humidity": "High", "Wind": "Strong"}, Class: "No"},
    }
    
    // Train the classifier
    classifier.Train(trainingData)
    
    // Test predictions
    testCases := []struct {
        features map[string]string
        expected string
    }{
        {
            features: map[string]string{"Outlook": "Sunny", "Temperature": "Cool", "Humidity": "High", "Wind": "Strong"},
            expected: "No",
        },
        {
            features: map[string]string{"Outlook": "Overcast", "Temperature": "Mild", "Humidity": "Normal", "Wind": "Weak"},
            expected: "Yes",
        },
        {
            features: map[string]string{"Outlook": "Rainy", "Temperature": "Hot", "Humidity": "Normal", "Wind": "Weak"},
            expected: "Yes",
        },
    }
    
    fmt.Println("Naïve Bayes Classifier Results:")
    fmt.Println("================================")
    
    for i, testCase := range testCases {
        predicted := classifier.Predict(testCase.features)
        probability := classifier.Probability(testCase.features, predicted)
        
        fmt.Printf("Test Case %d:\n", i+1)
        fmt.Printf("  Features: %v\n", testCase.features)
        fmt.Printf("  Predicted Class: %s\n", predicted)
        fmt.Printf("  Probability: %.4f\n", probability)
        fmt.Printf("  Expected: %s\n", testCase.expected)
        fmt.Printf("  Correct: %t\n\n", predicted == testCase.expected)
    }
    
    // Show class probabilities for a specific case
    fmt.Println("Detailed Probability Analysis:")
    fmt.Println("==============================")
    testFeatures := map[string]string{"Outlook": "Sunny", "Temperature": "Cool", "Humidity": "High", "Wind": "Strong"}
    
    fmt.Printf("For features %v:\n", testFeatures)
    for _, class := range classifier.classes {
        prob := classifier.Probability(testFeatures, class)
        fmt.Printf("  P(%s|features) = %.6f\n", class, prob)
    }
}
```

## Key Features of this Implementation:

### 1. **Core Components**
- `Feature` struct: Defines feature names and possible values
- `Document` struct: Represents training samples with features and class labels
- `NaiveBayesClassifier` struct: Main classifier with training and prediction methods

### 2. **Key Algorithms**
- **Training**: Calculates class priors and feature likelihoods
- **Prediction**: Uses Bayes' theorem with Laplace smoothing
- **Probability Calculation**: Computes posterior probabilities for each class

### 3. **Smoothing Technique**
- Implements Laplace smoothing to handle zero probabilities
- Prevents division by zero when features don't appear in training data

### 4. **Usage Example**
The example uses the classic "Play Tennis" dataset to demonstrate:
- Training with 14 examples
- Making predictions on new cases
- Calculating probabilities for each class

### 5. **Output**
The program will output prediction results with probabilities, showing how the classifier makes decisions based on the training data.

This implementation follows the mathematical principles of Naïve Bayes while being practical and efficient for real-world applications.

