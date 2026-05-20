# Gradient Boosting Machines (GBM) in Visual Basic

Here's an example implementation of a basic Gradient Boosting Machine algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class GradientBoostingMachine
    Private _trees As List(Of DecisionTree)
    Private _learningRate As Double
    Private _nEstimators As Integer
    Private _maxDepth As Integer
    
    Public Sub New(learningRate As Double = 0.1, nEstimators As Integer = 100, maxDepth As Integer = 3)
        _trees = New List(Of DecisionTree)()
        _learningRate = learningRate
        _nEstimators = nEstimators
        _maxDepth = maxDepth
    End Sub
    
    Public Sub Fit(X As Double()(), y As Double())
        Dim residuals As Double() = y.ToArray()
        
        For i As Integer = 0 To _nEstimators - 1
            ' Create and train a decision tree on the residuals
            Dim tree As New DecisionTree(_maxDepth)
            tree.Fit(X, residuals)
            
            ' Add tree to ensemble
            _trees.Add(tree)
            
            ' Update residuals
            Dim predictions As Double() = Predict(X)
            For j As Integer = 0 To residuals.Length - 1
                residuals(j) = y(j) - predictions(j)
            Next
        Next
    End Sub
    
    Public Function Predict(X As Double()()) As Double()
        Dim predictions As Double() = New Double(X.Length - 1) {}
        
        For i As Integer = 0 To X.Length - 1
            Dim sum As Double = 0.0
            For Each tree As DecisionTree In _trees
                sum += _learningRate * tree.Predict(X(i))
            Next
            predictions(i) = sum
        Next
        
        Return predictions
    End Function
End Class

Public Class DecisionTree
    Private _maxDepth As Integer
    Private _root As TreeNode
    
    Public Sub New(maxDepth As Integer)
        _maxDepth = maxDepth
    End Sub
    
    Public Sub Fit(X As Double()(), y As Double())
        _root = BuildTree(X, y, 0)
    End Sub
    
    Private Function BuildTree(X As Double()(), y As Double(), depth As Integer) As TreeNode
        If depth >= _maxDepth OrElse y.Distinct().Count() = 1 Then
            Return New TreeNode() With {.Value = y.Average()}
        End If
        
        Dim bestSplit As SplitInfo = FindBestSplit(X, y)
        If bestSplit.SplitValue = Double.MinValue Then
            Return New TreeNode() With {.Value = y.Average()}
        End If
        
        Dim leftIndices As List(Of Integer) = New List(Of Integer)()
        Dim rightIndices As List(Of Integer) = New List(Of Integer)()
        
        For i As Integer = 0 To X.Length - 1
            If X(i)(bestSplit.FeatureIndex) <= bestSplit.SplitValue Then
                leftIndices.Add(i)
            Else
                rightIndices.Add(i)
            End If
        Next
        
        Dim leftX As Double()() = leftIndices.Select(Function(i) X(i)).ToArray()
        Dim leftY As Double() = leftIndices.Select(Function(i) y(i)).ToArray()
        Dim rightX As Double()() = rightIndices.Select(Function(i) X(i)).ToArray()
        Dim rightY As Double() = rightIndices.Select(Function(i) y(i)).ToArray()
        
        Dim leftChild As TreeNode = BuildTree(leftX, leftY, depth + 1)
        Dim rightChild As TreeNode = BuildTree(rightX, rightY, depth + 1)
        
        Return New TreeNode() With {
            .FeatureIndex = bestSplit.FeatureIndex,
            .SplitValue = bestSplit.SplitValue,
            .LeftChild = leftChild,
            .RightChild = rightChild
        }
    End Function
    
    Private Function FindBestSplit(X As Double()(), y As Double()) As SplitInfo
        Dim bestError As Double = Double.MaxValue
        Dim bestSplit As SplitInfo = New SplitInfo()
        
        For featureIndex As Integer = 0 To X(0).Length - 1
            Dim featureValues As Double() = X.Select(Function(x) x(featureIndex)).ToArray()
            Dim uniqueValues As Double() = featureValues.Distinct().OrderBy(Function(x) x).ToArray()
            
            For i As Integer = 0 To uniqueValues.Length - 2
                Dim splitValue As Double = (uniqueValues(i) + uniqueValues(i + 1)) / 2.0
                
                Dim leftY As Double() = X.Where(Function(x, j) x(featureIndex) <= splitValue).Select(Function(x, j) y(j)).ToArray()
                Dim rightY As Double() = X.Where(Function(x, j) x(featureIndex) > splitValue).Select(Function(x, j) y(j)).ToArray()
                
                If leftY.Length > 0 AndAlso rightY.Length > 0 Then
                    Dim error As Double = CalculateMSE(leftY) * leftY.Length + CalculateMSE(rightY) * rightY.Length
                    If error < bestError Then
                        bestError = error
                        bestSplit = New SplitInfo() With {
                            .FeatureIndex = featureIndex,
                            .SplitValue = splitValue
                        }
                    End If
                End If
            Next
        Next
        
        Return bestSplit
    End Function
    
    Private Function CalculateMSE(values As Double()) As Double
        Dim mean As Double = values.Average()
        Return values.Sum(Function(x) (x - mean) * (x - mean)) / values.Length
    End Function
    
    Public Function Predict(x As Double()) As Double
        Return PredictNode(_root, x)
    End Function
    
    Private Function PredictNode(node As TreeNode, x As Double()) As Double
        If node.Value.HasValue Then
            Return node.Value.Value
        End If
        
        If x(node.FeatureIndex) <= node.SplitValue Then
            Return PredictNode(node.LeftChild, x)
        Else
            Return PredictNode(node.RightChild, x)
        End If
    End Function
End Class

Public Class TreeNode
    Public Property FeatureIndex As Integer
    Public Property SplitValue As Double
    Public Property LeftChild As TreeNode
    Public Property RightChild As TreeNode
    Public Property Value As Double?
End Class

Public Class SplitInfo
    Public Property FeatureIndex As Integer
    Public Property SplitValue As Double
End Class

' Example usage:
Public Class Program
    Public Shared Sub Main()
        ' Sample data
        Dim X As Double()() = {
            {1.0, 2.0},
            {2.0, 3.0},
            {3.0, 4.0},
            {4.0, 5.0},
            {5.0, 6.0}
        }
        
        Dim y As Double() = {3.0, 5.0, 7.0, 9.0, 11.0}
        
        ' Create and train GBM
        Dim gbm As New GradientBoostingMachine(learningRate: = 0.1, nEstimators: = 5, maxDepth: = 2)
        gbm.Fit(X, y)
        
        ' Make predictions
        Dim predictions As Double() = gbm.Predict(X)
        
        ' Display results
        Console.WriteLine("Predictions:")
        For i As Integer = 0 To predictions.Length - 1
            Console.WriteLine($"Sample {i}: Predicted = {predictions(i):F2}, Actual = {y(i):F2}")
        Next
    End Sub
End Class
```

## Key Components Explained:

### 1. **GradientBoostingMachine Class**
- Main class implementing the GBM algorithm
- Contains ensemble of decision trees
- Handles training and prediction processes

### 2. **DecisionTree Class**
- Implements a single decision tree
- Uses recursive splitting to build the tree
- Handles feature selection and splitting

### 3. **Core GBM Process**
- **Iterative Training**: Builds trees sequentially
- **Residuals**: Each tree learns from the errors of previous trees
- **Ensemble Prediction**: Combines predictions with learning rate

### 4. **Key Parameters**
- `learningRate`: Controls step size of gradient descent
- `nEstimators`: Number of trees to build
- `maxDepth`: Maximum depth of each tree

### 5. **Algorithm Steps**
1. Initialize with original target values
2. For each iteration:
   - Train a decision tree on current residuals
   - Add tree to ensemble
   - Update residuals
3. Make final prediction by combining all trees

This implementation provides a basic but functional GBM algorithm in Visual Basic .NET that can be extended with additional features like regularization, different loss functions, or more sophisticated tree building techniques.

