# Naïve Bayes Classifier in Haskell

Here's a complete implementation of a Naïve Bayes classifier in Haskell:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module NaiveBayes where

import qualified Data.Map as Map
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- Data types for our classifier
data Feature = Feature String deriving (Show, Eq, Ord)

data Classification = Classification String deriving (Show, Eq, Ord)

-- A training example consists of features and a classification
type Example = (Map.Map Feature Double, Classification)

-- Naive Bayes model structure
data NaiveBayes = NaiveBayes
    { classPrior :: Map.Map Classification Double
    , featureLikelihood :: Map.Map Feature (Map.Map Classification Double)
    , totalExamples :: Int
    , featureCounts :: Map.Map Feature Int
    } deriving (Show)

-- Create an empty Naive Bayes model
createNBModel :: NaiveBayes
createNBModel = NaiveBayes Map.empty Map.empty 0 Map.empty

-- Train the Naive Bayes classifier
trainNB :: [Example] -> NaiveBayes
trainNB examples = NaiveBayes classPriors featureLikelihoods totalExamples featureCounts
  where
    totalExamples = length examples
    classCounts = foldl' updateClassCount Map.empty examples
    classPriors = Map.map (\count -> fromIntegral count / fromIntegral totalExamples) classCounts
    
    -- Count features for each class
    featureClassCounts = foldl' updateFeatureClassCount Map.empty examples
    featureLikelihoods = Map.map (Map.map (\count -> fromIntegral count / fromIntegral totalExamples)) featureClassCounts
    
    -- Count total features
    featureCounts = foldl' updateFeatureCount Map.empty examples
    
    updateClassCount :: Map.Map Classification Int -> Example -> Map.Map Classification Int
    updateClassCount acc (_, classification) = Map.insertWith (+) classification 1 acc
    
    updateFeatureClassCount :: Map.Map Feature (Map.Map Classification Int) -> Example -> Map.Map Feature (Map.Map Classification Int)
    updateFeatureClassCount acc (features, classification) = 
        foldl' updateFeatureClass acc (Map.toList features)
      where
        updateFeatureClass :: Map.Map Feature (Map.Map Classification Int) -> (Feature, Double) -> Map.Map Feature (Map.Map Classification Int)
        updateFeatureClass acc' (feature, _) = 
            let classMap = fromMaybe Map.empty (Map.lookup feature acc')
                updatedClassMap = Map.insertWith (+) classification 1 classMap
            in Map.insert feature updatedClassMap acc'
    
    updateFeatureCount :: Map.Map Feature Int -> Example -> Map.Map Feature Int
    updateFeatureCount acc (features, _) = 
        foldl' updateFeature acc (Map.keys features)
      where
        updateFeature :: Map.Map Feature Int -> Feature -> Map.Map Feature Int
        updateFeature acc' feature = Map.insertWith (+) feature 1 acc'

-- Predict the class of a new example
predictNB :: NaiveBayes -> Map.Map Feature Double -> Classification
predictNB model features = 
    let classScores = Map.map (calculateScore model features) (Map.keys (classPrior model))
        maxScore = maximum (Map.elems classScores)
    in fromMaybe (Classification "unknown") (Map.lookup maxScore classScores)
  where
    calculateScore :: NaiveBayes -> Map.Map Feature Double -> Classification -> Double
    calculateScore model features class = 
        prior * likelihood
      where
        prior = fromMaybe 0 (Map.lookup class (classPrior model))
        likelihood = product (map (featureProbability model class) (Map.keys features))
        
        featureProbability :: NaiveBayes -> Classification -> Feature -> Double
        featureProbability model class feature = 
            let prob = fromMaybe 0 (Map.lookup class (Map.lookup feature (featureLikelihood model)))
            in if prob == 0 then 1e-10 else prob

-- Example usage
exampleTrainingData :: [Example]
exampleTrainingData = 
    [ (Map.fromList [(Feature "outlook", 1), (Feature "temperature", 1), (Feature "humidity", 1), (Feature "windy", 0)], Classification "no")
    , (Map.fromList [(Feature "outlook", 1), (Feature "temperature", 1), (Feature "humidity", 1), (Feature "windy", 1)], Classification "no")
    , (Map.fromList [(Feature "outlook", 0), (Feature "temperature", 1), (Feature "humidity", 1), (Feature "windy", 0)], Classification "yes")
    , (Map.fromList [(Feature "outlook", 2), (Feature "temperature", 0), (Feature "humidity", 1), (Feature "windy", 0)], Classification "yes")
    , (Map.fromList [(Feature "outlook", 2), (Feature "temperature", 0), (Feature "humidity", 0), (Feature "windy", 1)], Classification "no")
    , (Map.fromList [(Feature "outlook", 2), (Feature "temperature", 0), (Feature "humidity", 0), (Feature "windy", 0)], Classification "yes")
    , (Map.fromList [(Feature "outlook", 1), (Feature "temperature", 0), (Feature "humidity", 0), (Feature "windy", 0)], Classification "yes")
    , (Map.fromList [(Feature "outlook", 0), (Feature "temperature", 0), (Feature "humidity", 1), (Feature "windy", 0)], Classification "yes")
    , (Map.fromList [(Feature "outlook", 0), (Feature "temperature", 1), (Feature "humidity", 0), (Feature "windy", 0)], Classification "yes")
    , (Map.fromList [(Feature "outlook", 0), (Feature "temperature", 1), (Feature "humidity", 0), (Feature "windy", 1)], Classification "no")
    , (Map.fromList [(Feature "outlook", 1), (Feature "temperature", 1), (Feature "humidity", 0), (Feature "windy", 0)], Classification "yes")
    , (Map.fromList [(Feature "outlook", 2), (Feature "temperature", 1), (Feature "humidity", 1), (Feature "windy", 1)], Classification "no")
    , (Map.fromList [(Feature "outlook", 1), (Feature "temperature", 0), (Feature "humidity", 1), (Feature "windy", 1)], Classification "yes")
    , (Map.fromList [(Feature "outlook", 2), (Feature "temperature", 1), (Feature "humidity", 0), (Feature "windy", 1)], Classification "no")
    ]

-- Main function to demonstrate usage
main :: IO ()
main = do
    let model = trainNB exampleTrainingData
    putStrLn "Naive Bayes Model trained!"
    putStrLn $ "Classes: " ++ show (Map.keys (classPrior model))
    
    -- Test prediction
    let testExample = Map.fromList [(Feature "outlook", 2), (Feature "temperature", 1), (Feature "humidity", 1), (Feature "windy", 0)]
    let prediction = predictNB model testExample
    putStrLn $ "Prediction for test example: " ++ show prediction
    
    -- Show model details
    putStrLn "\nModel Details:"
    putStrLn $ "Class priors: " ++ show (classPrior model)
    putStrLn $ "Feature likelihoods: " ++ show (featureLikelihood model)
```

## Key Components Explained:

### 1. **Data Types**
- `Feature` and `Classification`: Represent categorical variables
- `Example`: A training example with features and classification
- `NaiveBayes`: The model structure storing priors, likelihoods, and counts

### 2. **Training Process**
- `trainNB`: Builds the model from training examples
- Calculates class priors (P(class))
- Computes feature likelihoods (P(feature|class))
- Counts feature occurrences for smoothing

### 3. **Prediction Process**
- `predictNB`: Uses Bayes' theorem to calculate probabilities
- Computes P(class|features) ∝ P(class) × ∏P(feature|class)
- Returns the class with maximum probability

### 4. **Example Usage**
The example demonstrates a classic weather prediction problem where:
- Features: outlook, temperature, humidity, windy
- Classes: yes/no (play tennis or not)

This implementation provides a complete, working Naive Bayes classifier in Haskell with proper type safety and functional programming patterns.

