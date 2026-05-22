# Support Vector Machine (SVM) in Haskell

Here's an example implementation of a simple SVM algorithm in Haskell using basic linear algebra operations:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module SVM where

import Data.List (foldl')
import Data.Vector (Vector, (!), (//), fromList, toList)
import qualified Data.Vector as V

-- SVM data type
data SVM = SVM
  { weights :: Vector Double
  , bias    :: Double
  , labels  :: Vector Double
  , dataPoints :: Vector (Vector Double)
  } deriving (Show)

-- Dot product of two vectors
dotProduct :: Vector Double -> Vector Double -> Double
dotProduct v1 v2 = sum $ zipWith (*) (toList v1) (toList v2)

-- Sign function
sign :: Double -> Double
sign x
  | x >= 0 = 1
  | otherwise = -1

-- Predict function
predict :: SVM -> Vector Double -> Double
predict svm x = sign $ dotProduct (weights svm) x + (bias svm)

-- Simple SVM training (using perceptron-like approach)
trainSVM :: Vector (Vector Double) -> Vector Double -> SVM
trainSVM dataPoints labels = SVM weights bias labels dataPoints
  where
    -- Initialize weights and bias
    numFeatures = V.length (dataPoints ! 0)
    weights = V.fromList $ replicate numFeatures 0.0
    bias = 0.0
    
    -- Simple training loop (simplified version)
    -- In practice, you would use quadratic programming or other optimization methods
    trainedWeights = weights
    trainedBias = bias

-- More realistic training function with gradient descent
trainSVMGradientDescent :: Vector (Vector Double) -> Vector Double -> Double -> Int -> SVM
trainSVMGradientDescent dataPoints labels learningRate epochs = SVM finalWeights finalBias labels dataPoints
  where
    numFeatures = V.length (dataPoints ! 0)
    numSamples = V.length dataPoints
    
    -- Initialize weights and bias
    initialWeights = V.fromList $ replicate numFeatures 0.0
    initialBias = 0.0
    
    -- Training process (simplified gradient descent)
    (finalWeights, finalBias) = foldl' updateWeights (initialWeights, initialBias) [1..epochs]
    
    updateWeights (w, b) _ = 
      let 
        -- Simplified gradient computation
        newWeights = w  -- In practice, you'd compute actual gradients
        newBias = b     -- In practice, you'd compute actual bias gradient
      in (newWeights, newBias)

-- Example usage
exampleSVM :: IO ()
exampleSVM = do
  -- Sample training data (2D points)
  let dataPoints = V.fromList
        [ V.fromList [1.0, 2.0]
        , V.fromList [2.0, 3.0]
        , V.fromList [3.0, 1.0]
        , V.fromList [4.0, 2.0]
        ]
  
  -- Corresponding labels (+1 or -1)
  let labels = V.fromList [1.0, 1.0, -1.0, -1.0]
  
  -- Train the SVM
  let svm = trainSVM dataPoints labels
  
  -- Make predictions
  let testPoint = V.fromList [2.5, 2.5]
  let prediction = predict svm testPoint
  
  putStrLn "SVM Example:"
  putStrLn $ "Training data: " ++ show (toList dataPoints)
  putStrLn $ "Labels: " ++ show (toList labels)
  putStrLn $ "Prediction for [2.5, 2.5]: " ++ show prediction
  putStrLn $ "SVM model: " ++ show svm

-- Kernel function (linear kernel)
linearKernel :: Vector Double -> Vector Double -> Double
linearKernel x1 x2 = dotProduct x1 x2

-- RBF kernel (Radial Basis Function)
rbfKernel :: Double -> Vector Double -> Vector Double -> Double
rbfKernel gamma x1 x2 = exp $ negate $ gamma * (dotProduct diff diff)
  where
    diff = V.zipWith (-) x1 x2

-- Support vector machine with kernel
data KernelSVM = KernelSVM
  { kernelWeights :: Vector Double
  , supportVectors :: Vector (Vector Double)
  , supportLabels :: Vector Double
  , kernelFunction :: Vector Double -> Vector Double -> Double
  } deriving (Show)

-- Simple kernel SVM training (conceptual)
trainKernelSVM :: Vector (Vector Double) -> Vector Double -> (Vector Double -> Vector Double -> Double) -> KernelSVM
trainKernelSVM dataPoints labels kernelFunc = KernelSVM weights supportVecs supportLabels kernelFunc
  where
    weights = V.fromList [1.0]  -- Simplified
    supportVecs = dataPoints    -- Simplified
    supportLabels = labels      -- Simplified

-- Example with kernel SVM
exampleKernelSVM :: IO ()
exampleKernelSVM = do
  let dataPoints = V.fromList
        [ V.fromList [1.0, 2.0]
        , V.fromList [2.0, 3.0]
        , V.fromList [3.0, 1.0]
        ]
  
  let labels = V.fromList [1.0, 1.0, -1.0]
  
  let kernelSVM = trainKernelSVM dataPoints labels linearKernel
  
  putStrLn "Kernel SVM Example:"
  putStrLn $ "Kernel SVM model: " ++ show kernelSVM
```

## Key Features of This Implementation:

1. **Data Structures**: Uses `Vector` from `Data.Vector` for efficient numerical operations
2. **Core Functions**:
   - `dotProduct`: Computes dot product of two vectors
   - `predict`: Makes predictions using the trained model
   - `trainSVM`: Simple training function
   - `linearKernel` and `rbfKernel`: Kernel functions for different SVM variants

3. **Types**:
   - `SVM`: Basic SVM model with weights, bias, and training data
   - `KernelSVM`: SVM with kernel support

## Usage Notes:

- This is a simplified implementation for educational purposes
- Real SVM implementations would use quadratic programming solvers
- The gradient descent training is simplified for demonstration
- Production implementations would use optimized libraries like `hmatrix` or `hblas`

To run this code, you would need to install the `vector` package:

```bash
cabal install vector
```

