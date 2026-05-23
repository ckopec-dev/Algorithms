# Gradient Boosting Machines (GBM) in Haskell

Here's an example implementation of a basic Gradient Boosting Machine algorithm in Haskell:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module GBM where

import Data.List (foldl')
import Data.Vector (Vector, (!), (//), fromList, toList)
import qualified Data.Vector as V
import Data.Function (on)

-- Define a simple decision tree node
data Tree = Tree
  { treeDepth :: Int
  , treeSplitFeature :: Int
  , treeSplitValue :: Double
  , treeLeft :: Maybe Tree
  , treeRight :: Maybe Tree
  , treePrediction :: Double
  } deriving (Show, Eq)

-- Define a weak learner (simple decision stump)
data WeakLearner = WeakLearner
  { learnerFeature :: Int
  , learnerThreshold :: Double
  , learnerLeftPrediction :: Double
  , learnerRightPrediction :: Double
  } deriving (Show, Eq)

-- Simple linear regression model for demonstration
data LinearModel = LinearModel
  { modelSlope :: Double
  , modelIntercept :: Double
  } deriving (Show, Eq)

-- Training data type
type FeatureVector = Vector Double
type TrainingData = [(FeatureVector, Double)]

-- Simple decision stump learner
trainStump :: TrainingData -> WeakLearner
trainStump dataPoints = 
  let (bestFeature, bestThreshold, leftPred, rightPred) = 
        findBestStump dataPoints
  in WeakLearner bestFeature bestThreshold leftPred rightPred

-- Find the best stump (simplest tree)
findBestStump :: TrainingData -> (Int, Double, Double, Double)
findBestStump dataPoints = 
  let features = [0..(length (head dataPoints) - 2)]
      best = foldl' updateBest (0, 0, 0, 0, 1e10) features
  in (fst3 best, snd3 best, thd3 best, fourth3 best)
  where
    updateBest (bestFeat, bestThresh, leftPred, rightPred, bestErr) feature = 
      let (thresh, lPred, rPred, err) = findBestThreshold dataPoints feature
      in if err < bestErr 
         then (feature, thresh, lPred, rPred, err)
         else (bestFeat, bestThresh, leftPred, rightPred, bestErr)

-- Find best threshold for a specific feature
findBestThreshold :: TrainingData -> Int -> (Double, Double, Double, Double)
findBestThreshold dataPoints feature = 
  let sortedData = sortDataByFeature dataPoints feature
      (bestThresh, leftPred, rightPred, err) = 
        foldl' updateThreshold (0, 0, 0, 1e10) (zipWith (\x y -> (x, y)) sortedData (tail sortedData))
  in (bestThresh, leftPred, rightPred, err)
  where
    updateThreshold (thresh, lPred, rPred, err) (x, y) = 
      let currentErr = calculateError x y
      in if currentErr < err 
         then (threshold x y, predictLeft x, predictRight y, currentErr)
         else (thresh, lPred, rPred, err)

-- Simple prediction function
predictStump :: WeakLearner -> FeatureVector -> Double
predictStump learner features = 
  let featureValue = features ! learnerFeature learner
  in if featureValue <= learnerThreshold learner
     then learnerLeftPrediction learner
     else learnerRightPrediction learner

-- Gradient Boosting Machine implementation
data GBM = GBM
  { gbmTrees :: [WeakLearner]
  , gbmLearningRate :: Double
  , gbmNumIterations :: Int
  } deriving (Show, Eq)

-- Train a GBM model
trainGBM :: TrainingData -> Int -> Double -> GBM
trainGBM dataPoints numIterations learningRate = 
  let initialPrediction = averageTarget dataPoints
      gbm = GBM [] learningRate numIterations
  in trainGBMIteratively gbm dataPoints initialPrediction

-- Simple iterative training
trainGBMIteratively :: GBM -> TrainingData -> Double -> GBM
trainGBMIteratively gbm dataPoints initialPred = 
  let trees = trainTrees dataPoints initialPred (gbmNumIterations gbm) (gbmLearningRate gbm)
  in gbm { gbmTrees = trees }

-- Train multiple trees
trainTrees :: TrainingData -> Double -> Int -> Double -> [WeakLearner]
trainTrees dataPoints initialPred numTrees learningRate = 
  let (trees, _) = 
        foldl' (\(accTrees, currentData) _ -> 
          let residual = calculateResiduals currentData
              stump = trainStump residual
              updatedData = updateTrainingData currentData stump learningRate
          in (accTrees ++ [stump], updatedData)
        ) ([], dataPoints) [1..numTrees]
  in trees

-- Calculate residuals (errors)
calculateResiduals :: TrainingData -> TrainingData
calculateResiduals dataPoints = 
  map (\(features, target) -> 
    let prediction = predictGBM (GBM [] 1.0 1) features  -- Simple prediction
    in (features, target - prediction)
  ) dataPoints

-- Simple prediction function for GBM
predictGBM :: GBM -> FeatureVector -> Double
predictGBM gbm features = 
  let trees = gbmTrees gbm
      learningRate = gbmLearningRate gbm
      predictions = map (\tree -> learningRate * predictStump tree features) trees
  in sum predictions

-- Helper functions
averageTarget :: TrainingData -> Double
averageTarget dataPoints = 
  let targets = map snd dataPoints
  in sum targets / fromIntegral (length targets)

sortDataByFeature :: TrainingData -> Int -> [Double]
sortDataByFeature dataPoints feature = 
  let featureValues = map (\(features, _) -> features ! feature) dataPoints
  in sort featureValues

-- Simple threshold calculation
threshold :: (FeatureVector, Double) -> (FeatureVector, Double) -> Double
threshold (f1, t1) (f2, t2) = (f1 ! feature + f2 ! feature) / 2
  where feature = 0  -- Simplified for example

-- Prediction helpers
predictLeft :: (FeatureVector, Double) -> Double
predictLeft (_, target) = target

predictRight :: (FeatureVector, Double) -> Double
predictRight (_, target) = target

-- Simple error calculation
calculateError :: (FeatureVector, Double) -> (FeatureVector, Double) -> Double
calculateError (f1, t1) (f2, t2) = abs (t1 - t2)

-- Utility functions for tuple operations
fst3 :: (a, b, c, d) -> a
fst3 (a, _, _, _) = a

snd3 :: (a, b, c, d) -> b
snd3 (_, b, _, _) = b

thd3 :: (a, b, c, d) -> c
thd3 (_, _, c, _) = c

fourth3 :: (a, b, c, d) -> d
fourth3 (_, _, _, d) = d

-- Example usage
exampleUsage :: IO ()
exampleUsage = do
  -- Sample training data: (features, target)
  let trainingData = 
        [ (fromList [1.0, 2.0], 3.0)
        , (fromList [2.0, 3.0], 5.0)
        , (fromList [3.0, 4.0], 7.0)
        , (fromList [4.0, 5.0], 9.0)
        , (fromList [5.0, 6.0], 11.0)
        ]
  
  -- Train GBM model
  let gbmModel = trainGBM trainingData 3 0.1
  
  -- Make prediction
  let testFeatures = fromList [2.5, 3.5]
      prediction = predictGBM gbmModel testFeatures
  
  putStrLn $ "GBM Model: " ++ show gbmModel
  putStrLn $ "Prediction for [2.5, 3.5]: " ++ show prediction
```

## Key Components Explained

### 1. **Data Structures**
- `Tree`: Represents decision tree nodes
- `WeakLearner`: Simple decision stumps (base learners)
- `GBM`: Main GBM model with trees, learning rate, and iteration count

### 2. **Core Functions**
- `trainStump`: Creates simple decision stumps
- `trainGBM`: Main training function that builds the ensemble
- `predictGBM`: Makes predictions using the trained model
- `calculateResiduals`: Computes errors for next iteration

### 3. **Gradient Boosting Process**
1. Start with initial prediction (average of targets)
2. For each iteration:
   - Calculate residuals (errors)
   - Train weak learner on residuals
   - Add learner to ensemble with learning rate
3. Combine all learners for final prediction

### 4. **Usage Example**
The example shows training on simple 2D data and making predictions on new data points.

This implementation demonstrates the core concepts of GBM in Haskell, though a production implementation would include more sophisticated features like cross-validation, regularization, and better tree building algorithms.

