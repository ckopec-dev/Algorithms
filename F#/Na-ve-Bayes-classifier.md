# Naïve Bayes Classifier in F#

Here's a complete implementation of a Naïve Bayes classifier in F#:

```fsharp
open System
open System.Collections.Generic

// Define a record type for our data point
type DataPoint = {
    Features: Map<string, float>
    Label: string
}

// Naïve Bayes Classifier implementation
type NaiveBayesClassifier() =
    let classPriorProbs = Dictionary<string, float>()
    let featureLikelihoods = Dictionary<string, Map<string, Map<float, float>>>()
    let totalSamples = ref 0
    let classCounts = Dictionary<string, int>()
    let featureValues = Dictionary<string, Set<float>>()

    // Train the classifier
    member this.Train(dataPoints: DataPoint list) =
        // Reset internal state
        classPriorProbs.Clear()
        featureLikelihoods.Clear()
        classCounts.Clear()
        featureValues.Clear()
        totalSamples := 0

        // Count samples per class
        dataPoints |> List.iter (fun dp ->
            let label = dp.Label
            classCounts.[label] <- 
                if classCounts.ContainsKey(label) then classCounts.[label] + 1
                else 1
            totalSamples := !totalSamples + 1
        )

        // Calculate prior probabilities
        classPriorProbs.Clear()
        classCounts |> Seq.iter (fun kvp ->
            let classLabel = kvp.Key
            let count = kvp.Value
            classPriorProbs.[classLabel] <- float count / float !totalSamples
        )

        // Calculate feature likelihoods
        dataPoints |> List.iter (fun dp ->
            let label = dp.Label
            dp.Features |> Map.iter (fun featureName featureValue ->
                if not (featureLikelihoods.ContainsKey(featureName)) then
                    featureLikelihoods.[featureName] <- Map.empty

                let classLikelihoods = featureLikelihoods.[featureName]
                if not (classLikelihoods.ContainsKey(label)) then
                    featureLikelihoods.[featureName] <- 
                        classLikelihoods.Add(label, Map.empty)

                let valueLikelihoods = featureLikelihoods.[featureName].[label]
                if not (valueLikelihoods.ContainsKey(featureValue)) then
                    featureLikelihoods.[featureName] <- 
                        classLikelihoods.Add(label, 
                            valueLikelihoods.Add(featureValue, 0.0))
                
                // Update feature values set
                if not (featureValues.ContainsKey(featureName)) then
                    featureValues.[featureName] <- Set.empty
                featureValues.[featureName] <- 
                    featureValues.[featureName] |> Set.add featureValue
            )
        )

        // Calculate likelihood probabilities
        dataPoints |> List.iter (fun dp ->
            let label = dp.Label
            dp.Features |> Map.iter (fun featureName featureValue ->
                let classLikelihoods = featureLikelihoods.[featureName]
                let valueLikelihoods = classLikelihoods.[label]
                let currentCount = valueLikelihoods.[featureValue]
                let newCount = currentCount + 1.0
                featureLikelihoods.[featureName] <- 
                    classLikelihoods.Add(label, 
                        valueLikelihoods.Add(featureValue, newCount))
            )
        )

    // Predict the class for a new data point
    member this.Predict(features: Map<string, float>) =
        let classScores = Dictionary<string, float>()

        // Calculate score for each class
        classPriorProbs |> Seq.iter (fun kvp ->
            let classLabel = kvp.Key
            let priorProb = kvp.Value
            let score = log priorProb
            
            // Calculate likelihood for each feature
            features |> Map.iter (fun featureName featureValue ->
                if featureLikelihoods.ContainsKey(featureName) then
                    let classLikelihoods = featureLikelihoods.[featureName]
                    if classLikelihoods.ContainsKey(classLabel) then
                        let valueLikelihoods = classLikelihoods.[classLabel]
                        
                        // Handle unknown feature values using Laplace smoothing
                        let totalCount = 
                            valueLikelihoods |> Map.values |> Seq.sum
                        let valueCount = 
                            if valueLikelihoods.ContainsKey(featureValue) then
                                valueLikelihoods.[featureValue]
                            else 0.0
                        
                        // Add 1 for Laplace smoothing
                        let smoothedProb = 
                            (valueCount + 1.0) / (totalCount + float (Set.count (featureValues.[featureName])))
                        
                        let logProb = log smoothedProb
                        let score = score + logProb
                        
                        classScores.[classLabel] <- score
                    else
                        classScores.[classLabel] <- score
                else
                    classScores.[classLabel] <- score
            )
        )

        // Return class with highest score
        if classScores.Count > 0 then
            classScores |> Seq.maxBy snd |> fst
        else
            "Unknown"

// Example usage
[<EntryPoint>]
let main argv =
    // Create sample training data
    let trainingData = [
        { Features = Map.ofList [("temperature", 25.0); ("humidity", 60.0)]; Label = "sunny" }
        { Features = Map.ofList [("temperature", 30.0); ("humidity", 70.0)]; Label = "sunny" }
        { Features = Map.ofList [("temperature", 20.0); ("humidity", 80.0)]; Label = "rainy" }
        { Features = Map.ofList [("temperature", 15.0); ("humidity", 90.0)]; Label = "rainy" }
        { Features = Map.ofList [("temperature", 28.0); ("humidity", 50.0)]; Label = "sunny" }
        { Features = Map.ofList [("temperature", 18.0); ("humidity", 75.0)]; Label = "rainy" }
        { Features = Map.ofList [("temperature", 22.0); ("humidity", 65.0)]; Label = "sunny" }
        { Features = Map.ofList [("temperature", 12.0); ("humidity", 85.0)]; Label = "rainy" }
    ]

    // Create and train the classifier
    let classifier = NaiveBayesClassifier()
    classifier.Train(trainingData)

    // Test prediction
    let testPoint = Map.ofList [("temperature", 23.0); ("humidity", 70.0)]
    let prediction = classifier.Predict(testPoint)
    
    printfn "Prediction for temperature=23.0, humidity=70.0: %s" prediction
    
    // Test with another point
    let testPoint2 = Map.ofList [("temperature", 15.0); ("humidity", 85.0)]
    let prediction2 = classifier.Predict(testPoint2)
    
    printfn "Prediction for temperature=15.0, humidity=85.0: %s" prediction2
    
    0 // return an integer exit code
```

## Key Features of this Implementation:

1. **Data Structure**: Uses F# records and maps for clean data representation
2. **Training Phase**: Calculates prior probabilities and likelihoods from training data
3. **Prediction Phase**: Uses log probabilities to avoid numerical underflow
4. **Laplace Smoothing**: Handles unknown feature values gracefully
5. **Naïve Assumption**: Assumes feature independence (classic Naïve Bayes)
6. **Log Probability**: Uses logarithms for numerical stability

## How it Works:

1. **Training**: 
   - Calculates prior probability for each class
   - Builds likelihood tables for each feature-class combination
   - Uses Laplace smoothing to handle unseen values

2. **Prediction**:
   - For each class, calculates the posterior probability
   - Uses Bayes' theorem: P(class|features) ∝ P(class) × P(features|class)
   - Returns the class with highest probability

This implementation demonstrates the core concepts of Naïve Bayes classification in a functional F# style while maintaining good performance and numerical stability.

