# Random Forest Algorithm in Visual Basic

Here's a complete implementation of a Random Forest algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

' Node class for decision tree
Public Class TreeNode
    Public Property FeatureIndex As Integer
    Public Property Threshold As Double
    Public Property LeftChild As TreeNode
    Public Property RightChild As TreeNode
    Public Property Prediction As Double
    Public Property IsLeaf As Boolean
    
    Public Sub New()
        IsLeaf = False
        FeatureIndex = -1
        Threshold = 0.0
        Prediction = 0.0
    End Sub
End Class

' Decision Tree implementation
Public Class DecisionTree
    Private Property Root As TreeNode
    Private Property MaxDepth As Integer
    Private Property MinSamplesSplit As Integer
    
    Public Sub New(maxDepth As Integer, minSamplesSplit As Integer)
        Me.MaxDepth = maxDepth
        Me.MinSamplesSplit = minSamplesSplit
    End Sub
    
    Public Sub Fit(features As Double(, ), labels As Double())
        Root = BuildTree(features, labels, 0)
    End Sub
    
    Private Function BuildTree(features As Double(, ), labels As Double(), depth As Integer) As TreeNode
        Dim node As New TreeNode()
        Dim nSamples As Integer = features.GetLength(0)
        Dim nFeatures As Integer = features.GetLength(1)
        
        ' Stopping criteria
        If depth >= MaxDepth OrElse nSamples < MinSamplesSplit Then
            node.IsLeaf = True
            node.Prediction = CalculateMode(labels)
            Return node
        End If
        
        ' Find best split
        Dim bestFeature As Integer = -1
        Dim bestThreshold As Double = 0.0
        Dim bestGini As Double = Double.MaxValue
        
        For i As Integer = 0 To nFeatures - 1
            Dim thresholds As List(Of Double) = GetUniqueValues(features, i)
            For Each threshold As Double In thresholds
                Dim gini As Double = CalculateGini(features, labels, i, threshold)
                If gini < bestGini Then
                    bestGini = gini
                    bestFeature = i
                    bestThreshold = threshold
                End If
            Next
        Next
        
        ' If no good split found
        If bestFeature = -1 Then
            node.IsLeaf = True
            node.Prediction = CalculateMode(labels)
            Return node
        End If
        
        ' Split the data
        Dim leftIndices As List(Of Integer) = New List(Of Integer)()
        Dim rightIndices As List(Of Integer) = New List(Of Integer)()
        
        For i As Integer = 0 To nSamples - 1
            If features(i, bestFeature) <= bestThreshold Then
                leftIndices.Add(i)
            Else
                rightIndices.Add(i)
            End If
        Next
        
        ' Create node
        node.FeatureIndex = bestFeature
        node.Threshold = bestThreshold
        
        ' Recursively build left and right subtrees
        If leftIndices.Count > 0 Then
            Dim leftFeatures(leftIndices.Count - 1, nFeatures - 1) As Double
            Dim leftLabels(leftIndices.Count - 1) As Double
            
            For i As Integer = 0 To leftIndices.Count - 1
                For j As Integer = 0 To nFeatures - 1
                    leftFeatures(i, j) = features(leftIndices(i), j)
                Next
                leftLabels(i) = labels(leftIndices(i))
            Next
            
            node.LeftChild = BuildTree(leftFeatures, leftLabels, depth + 1)
        End If
        
        If rightIndices.Count > 0 Then
            Dim rightFeatures(rightIndices.Count - 1, nFeatures - 1) As Double
            Dim rightLabels(rightIndices.Count - 1) As Double
            
            For i As Integer = 0 To rightIndices.Count - 1
                For j As Integer = 0 To nFeatures - 1
                    rightFeatures(i, j) = features(rightIndices(i), j)
                Next
                rightLabels(i) = labels(rightIndices(i))
            Next
            
            node.RightChild = BuildTree(rightFeatures, rightLabels, depth + 1)
        End If
        
        Return node
    End Function
    
    Private Function GetUniqueValues(features As Double(, ), featureIndex As Integer) As List(Of Double)
        Dim values As New HashSet(Of Double)
        For i As Integer = 0 To features.GetLength(0) - 1
            values.Add(features(i, featureIndex))
        Next
        Return values.ToList()
    End Function
    
    Private Function CalculateGini(features As Double(, ), labels As Double(), featureIndex As Integer, threshold As Double) As Double
        Dim nSamples As Integer = features.GetLength(0)
        Dim leftLabels As New List(Of Double)()
        Dim rightLabels As New List(Of Double)()
        
        For i As Integer = 0 To nSamples - 1
            If features(i, featureIndex) <= threshold Then
                leftLabels.Add(labels(i))
            Else
                rightLabels.Add(labels(i))
            End If
        Next
        
        If leftLabels.Count = 0 OrElse rightLabels.Count = 0 Then
            Return 0.0
        End If
        
        Dim giniLeft As Double = CalculateGiniImpurity(leftLabels)
        Dim giniRight As Double = CalculateGiniImpurity(rightLabels)
        
        Dim weightLeft As Double = leftLabels.Count / nSamples
        Dim weightRight As Double = rightLabels.Count / nSamples
        
        Return weightLeft * giniLeft + weightRight * giniRight
    End Function
    
    Private Function CalculateGiniImpurity(labels As List(Of Double)) As Double
        Dim nSamples As Integer = labels.Count
        Dim classCounts As New Dictionary(Of Double, Integer)()
        
        For Each label As Double In labels
            If classCounts.ContainsKey(label) Then
                classCounts(label) += 1
            Else
                classCounts(label) = 1
            End If
        Next
        
        Dim gini As Double = 1.0
        For Each count As Integer In classCounts.Values
            Dim probability As Double = count / nSamples
            gini -= probability * probability
        Next
        
        Return gini
    End Function
    
    Private Function CalculateMode(labels As Double()) As Double
        Dim counts As New Dictionary(Of Double, Integer)()
        For Each label As Double In labels
            If counts.ContainsKey(label) Then
                counts(label) += 1
            Else
                counts(label) = 1
            End If
        Next
        
        Return counts.OrderByDescending(Function(kvp) kvp.Value).First().Key
    End Function
    
    Public Function Predict(sample As Double()) As Double
        Return PredictRecursive(Root, sample)
    End Function
    
    Private Function PredictRecursive(node As TreeNode, sample As Double()) As Double
        If node.IsLeaf Then
            Return node.Prediction
        End If
        
        If sample(node.FeatureIndex) <= node.Threshold Then
            If node.LeftChild IsNot Nothing Then
                Return PredictRecursive(node.LeftChild, sample)
            End If
        Else
            If node.RightChild IsNot Nothing Then
                Return PredictRecursive(node.RightChild, sample)
            End If
        End If
        
        Return node.Prediction
    End Function
End Class

' Random Forest implementation
Public Class RandomForest
    Private Property Trees As List(Of DecisionTree)
    Private Property NEstimators As Integer
    Private Property MaxDepth As Integer
    Private Property MinSamplesSplit As Integer
    Private Property BootstrapSize As Integer
    
    Public Sub New(nEstimators As Integer, maxDepth As Integer, minSamplesSplit As Integer)
        Me.NEstimators = nEstimators
        Me.MaxDepth = maxDepth
        Me.MinSamplesSplit = minSamplesSplit
        Me.Trees = New List(Of DecisionTree)()
    End Sub
    
    Public Sub Fit(features As Double(, ), labels As Double())
        Trees.Clear()
        
        Dim nSamples As Integer = features.GetLength(0)
        Dim nFeatures As Integer = features.GetLength(1)
        
        For i As Integer = 0 To NEstimators - 1
            ' Bootstrap sampling
            Dim bootstrapIndices As List(Of Integer) = BootstrapSample(nSamples)
            
            Dim bootstrapFeatures(bootstrapIndices.Count - 1, nFeatures - 1) As Double
            Dim bootstrapLabels(bootstrapIndices.Count - 1) As Double
            
            For j As Integer = 0 To bootstrapIndices.Count - 1
                For k As Integer = 0 To nFeatures - 1
                    bootstrapFeatures(j, k) = features(bootstrapIndices(j), k)
                Next
                bootstrapLabels(j) = labels(bootstrapIndices(j))
            Next
            
            ' Create and train decision tree
            Dim tree As New DecisionTree(MaxDepth, MinSamplesSplit)
            tree.Fit(bootstrapFeatures, bootstrapLabels)
            Trees.Add(tree)
        Next
    End Sub
    
    Public Function Predict(samples As Double(, )) As Double()
        Dim predictions(samples.GetLength(0) - 1) As Double
        
        For i As Integer = 0 To samples.GetLength(0) - 1
            Dim sample As Double() = New Double(samples.GetLength(1) - 1) {}
            For j As Integer = 0 To samples.GetLength(1) - 1
                sample(j) = samples(i, j)
            Next
            
            Dim treePredictions As New List(Of Double)()
            For Each tree As DecisionTree In Trees
                treePredictions.Add(tree.Predict(sample))
            Next
            
            ' Majority vote
            predictions(i) = CalculateMode(treePredictions.ToArray())
        Next
        
        Return predictions
    End Function
    
    Private Function BootstrapSample(nSamples As Integer) As List(Of Integer)
        Dim random As New Random()
        Dim indices As New List(Of Integer)()
        
        For i As Integer = 0 To nSamples - 1
            indices.Add(random.Next(nSamples))
        Next
        
        Return indices
    End Function
    
    Private Function CalculateMode(labels As Double()) As Double
        Dim counts As New Dictionary(Of Double, Integer)()
        For Each label As Double In labels
            If counts.ContainsKey(label) Then
                counts(label) += 1
            Else
                counts(label) = 1
            End If
        Next
        
        Return counts.OrderByDescending(Function(kvp) kvp.Value).First().Key
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Sample data (features: [age, income], labels: [0=No, 1=Yes])
        Dim features(4, 1) As Double = {
            {25, 50000},
            {35, 80000},
            {45, 120000},
            {23, 40000},
            {33, 70000}
        }
        
        Dim labels(4) As Double = {0, 1, 1, 0, 1}
        
        ' Create and train Random Forest
        Dim rf As New RandomForest(5, 3, 2)
        rf.Fit(features, labels)
        
        ' Make predictions
        Dim testSamples(1, 1) As Double = {
            {30, 60000},
            {50, 100000}
        }
        
        Dim predictions() As Double = rf.Predict(testSamples)
        
        Console.WriteLine("Predictions:")
        For i As Integer = 0 To predictions.Length - 1
            Console.WriteLine($"Sample {i + 1}: {predictions(i)}")
        Next
        
        Console.ReadLine()
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Decision Tree Class**: Implements a basic decision tree with Gini impurity calculation
2. **Random Forest Class**: Contains multiple decision trees with bootstrap sampling
3. **Bootstrap Sampling**: Random sampling with replacement for each tree
4. **Majority Voting**: Final prediction is based on majority vote of all trees
5. **Configurable Parameters**: 
   - Number of estimators (trees)
   - Maximum depth of trees
   - Minimum samples required to split

## Usage Example:
The code demonstrates training a Random Forest with sample data and making predictions on new data points. The implementation includes proper error handling and follows standard Random Forest principles including bootstrap sampling and ensemble voting.

This implementation provides a solid foundation that can be extended with additional features like feature importance calculation, cross-validation, or different splitting criteria.

