# CART Decision Tree Algorithm in Visual Basic

Here's a complete implementation of a Classification and Regression Tree (CART) algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class CartNode
    Public Property FeatureIndex As Integer
    Public Property Threshold As Double
    Public Property LeftChild As CartNode
    Public Property RightChild As CartNode
    Public Property Prediction As Double
    Public Property IsLeaf As Boolean

    Public Sub New()
        IsLeaf = False
        Prediction = 0.0
        FeatureIndex = -1
        Threshold = 0.0
        LeftChild = Nothing
        RightChild = Nothing
    End Sub
End Class

Public Class CartDecisionTree
    Private _root As CartNode
    Private _maxDepth As Integer
    Private _minSamplesSplit As Integer

    Public Sub New(maxDepth As Integer = 10, minSamplesSplit As Integer = 2)
        _maxDepth = maxDepth
        _minSamplesSplit = minSamplesSplit
    End Sub

    Public Sub Fit(X As List(Of List(Of Double)), y As List(Of Double))
        _root = BuildTree(X, y, 0)
    End Sub

    Public Function Predict(X As List(Of Double)) As Double
        Return PredictRecursive(_root, X)
    End Function

    Public Function PredictBatch(XList As List(Of List(Of Double))) As List(Of Double)
        Dim predictions As New List(Of Double)
        For Each x In XList
            predictions.Add(Predict(x))
        Next
        Return predictions
    End Function

    Private Function BuildTree(X As List(Of List(Of Double)), y As List(Of Double), depth As Integer) As CartNode
        Dim node As New CartNode()
        
        ' Check stopping criteria
        If depth >= _maxDepth OrElse X.Count < _minSamplesSplit Then
            node.IsLeaf = True
            node.Prediction = CalculateMean(y)
            Return node
        End If

        ' Find best split
        Dim bestSplit As SplitResult = FindBestSplit(X, y)
        
        ' If no good split found, make leaf
        If bestSplit.Gain <= 0 Then
            node.IsLeaf = True
            node.Prediction = CalculateMean(y)
            Return node
        End If

        ' Split the data
        Dim leftX As List(Of List(Of Double)) = New List(Of List(Of Double))()
        Dim leftY As List(Of Double) = New List(Of Double)()
        Dim rightX As List(Of List(Of Double)) = New List(Of List(Of Double))()
        Dim rightY As List(Of Double) = New List(Of Double)()

        For i As Integer = 0 To X.Count - 1
            If X(i)(bestSplit.FeatureIndex) <= bestSplit.Threshold Then
                leftX.Add(X(i))
                leftY.Add(y(i))
            Else
                rightX.Add(X(i))
                rightY.Add(y(i))
            End If
        Next

        ' Check if splits are valid
        If leftX.Count < _minSamplesSplit OrElse rightX.Count < _minSamplesSplit Then
            node.IsLeaf = True
            node.Prediction = CalculateMean(y)
            Return node
        End If

        ' Recursively build left and right subtrees
        node.FeatureIndex = bestSplit.FeatureIndex
        node.Threshold = bestSplit.Threshold
        node.LeftChild = BuildTree(leftX, leftY, depth + 1)
        node.RightChild = BuildTree(rightX, rightY, depth + 1)

        Return node
    End Function

    Private Function FindBestSplit(X As List(Of List(Of Double)), y As List(Of Double)) As SplitResult
        Dim bestGain As Double = -1
        Dim bestFeatureIndex As Integer = -1
        Dim bestThreshold As Double = 0.0

        Dim nFeatures As Integer = X(0).Count
        Dim nSamples As Integer = X.Count

        For featureIndex As Integer = 0 To nFeatures - 1
            Dim featureValues As New List(Of Double)
            For i As Integer = 0 To nSamples - 1
                featureValues.Add(X(i)(featureIndex))
            Next

            Dim uniqueValues As List(Of Double) = featureValues.Distinct().ToList()
            uniqueValues.Sort()

            For i As Integer = 0 To uniqueValues.Count - 2
                Dim threshold As Double = (uniqueValues(i) + uniqueValues(i + 1)) / 2.0

                Dim leftY As List(Of Double) = New List(Of Double)()
                Dim rightY As List(Of Double) = New List(Of Double)()

                For j As Integer = 0 To nSamples - 1
                    If X(j)(featureIndex) <= threshold Then
                        leftY.Add(y(j))
                    Else
                        rightY.Add(y(j))
                    End If
                Next

                If leftY.Count > 0 AndAlso rightY.Count > 0 Then
                    Dim gain As Double = CalculateInformationGain(y, leftY, rightY)
                    If gain > bestGain Then
                        bestGain = gain
                        bestFeatureIndex = featureIndex
                        bestThreshold = threshold
                    End If
                End If
            Next
        Next

        Return New SplitResult With {
            .Gain = bestGain,
            .FeatureIndex = bestFeatureIndex,
            .Threshold = bestThreshold
        }
    End Function

    Private Function CalculateInformationGain(parentY As List(Of Double), leftY As List(Of Double), rightY As List(Of Double)) As Double
        Dim parentVariance As Double = CalculateVariance(parentY)
        Dim leftVariance As Double = CalculateVariance(leftY)
        Dim rightVariance As Double = CalculateVariance(rightY)

        Dim weightedVariance As Double = (leftY.Count * leftVariance + rightY.Count * rightVariance) / (leftY.Count + rightY.Count)
        Dim informationGain As Double = parentVariance - weightedVariance

        Return informationGain
    End Function

    Private Function CalculateVariance(y As List(Of Double)) As Double
        If y.Count = 0 Then Return 0.0

        Dim mean As Double = y.Average()
        Dim sum As Double = 0.0

        For Each value In y
            sum += (value - mean) * (value - mean)
        Next

        Return sum / y.Count
    End Function

    Private Function CalculateMean(y As List(Of Double)) As Double
        If y.Count = 0 Then Return 0.0
        Return y.Average()
    End Function

    Private Function PredictRecursive(node As CartNode, x As List(Of Double)) As Double
        If node.IsLeaf Then
            Return node.Prediction
        End If

        If x(node.FeatureIndex) <= node.Threshold Then
            Return PredictRecursive(node.LeftChild, x)
        Else
            Return PredictRecursive(node.RightChild, x)
        End If
    End Function
End Class

Public Class SplitResult
    Public Property Gain As Double
    Public Property FeatureIndex As Integer
    Public Property Threshold As Double
End Class

' Example usage
Module Program
    Sub Main()
        ' Sample dataset
        Dim X As New List(Of List(Of Double)) From {
            New List(Of Double) From {1.0, 2.0},
            New List(Of Double) From {2.0, 3.0},
            New List(Of Double) From {3.0, 1.0},
            New List(Of Double) From {4.0, 4.0},
            New List(Of Double) From {5.0, 2.0}
        }

        Dim y As New List(Of Double) From {1.0, 2.0, 3.0, 4.0, 5.0}

        ' Create and train the CART model
        Dim cart As New CartDecisionTree(maxDepth:=5, minSamplesSplit:=2)
        cart.Fit(X, y)

        ' Make predictions
        Dim testSample As New List(Of Double) From {2.5, 2.5}
        Dim prediction As Double = cart.Predict(testSample)
        
        Console.WriteLine($"Prediction for {String.Join(", ", testSample)}: {prediction}")
        
        ' Make multiple predictions
        Dim testSamples As New List(Of List(Of Double)) From {
            New List(Of Double) From {1.5, 1.5},
            New List(Of Double) From {3.5, 3.5}
        }
        
        Dim predictions As List(Of Double) = cart.PredictBatch(testSamples)
        
        For i As Integer = 0 To predictions.Count - 1
            Console.WriteLine($"Prediction for {String.Join(", ", testSamples(i))}: {predictions(i)}")
        Next
    End Sub
End Module
```

## Key Features of this CART Implementation:

### 1. **Core Components:**
- `CartNode` class representing tree nodes
- `CartDecisionTree` class implementing the main algorithm
- `SplitResult` class for storing split information

### 2. **Algorithm Steps:**
- **Tree Building**: Recursively splits data based on feature values
- **Best Split Selection**: Finds optimal feature and threshold using information gain
- **Stopping Criteria**: Depth limit and minimum samples required
- **Prediction**: Traverses the tree to make predictions

### 3. **Key Methods:**
- `Fit()`: Trains the decision tree
- `Predict()`: Makes single predictions
- `PredictBatch()`: Makes multiple predictions
- `BuildTree()`: Recursively builds the tree structure
- `FindBestSplit()`: Determines optimal splits

### 4. **Features:**
- Handles regression tasks (predicting continuous values)
- Uses variance reduction for split quality
- Implements pruning with depth and sample limits
- Supports both single and batch predictions

### 5. **Usage Example:**
The code includes a complete example showing how to:
- Create sample data
- Train the CART model
- Make predictions on new data points

This implementation follows the standard CART algorithm principles while being adapted for Visual Basic .NET syntax and structure.

