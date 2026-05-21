# Decision Tree Learning Algorithm in Visual Basic

Here's a complete implementation of a Decision Tree Learning algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class DecisionTreeNode
    Public Property FeatureName As String
    Public Property FeatureValue As String
    Public Property IsLeaf As Boolean = False
    Public Property Prediction As String
    Public Property Children As Dictionary(Of String, DecisionTreeNode)
    
    Public Sub New()
        Children = New Dictionary(Of String, DecisionTreeNode)()
    End Sub
End Class

Public Class DecisionTree
    Private _root As DecisionTreeNode
    Private _features As List(Of String)
    Private _targetValues As List(Of String)
    
    Public Sub New(features As List(Of String), targetValues As List(Of String))
        _features = features
        _targetValues = targetValues
    End Sub
    
    ' Main decision tree learning algorithm
    Public Sub Train(data As List(Of Dictionary(Of String, String)), targetAttribute As String)
        _root = BuildTree(data, targetAttribute)
    End Sub
    
    Private Function BuildTree(data As List(Of Dictionary(Of String, String)), targetAttribute As String) As DecisionTreeNode
        Dim node As New DecisionTreeNode()
        
        ' Get all target values for current data
        Dim targetValues As List(Of String) = data.Select(Function(row) row(targetAttribute)).ToList()
        
        ' If all target values are the same, create a leaf node
        If targetValues.Distinct().Count() = 1 Then
            node.IsLeaf = True
            node.Prediction = targetValues(0)
            Return node
        End If
        
        ' If no features left, return leaf with most common target value
        If _features.Count = 0 Then
            node.IsLeaf = True
            node.Prediction = GetMostCommonValue(targetValues)
            Return node
        End If
        
        ' Find best feature to split on
        Dim bestFeature As String = GetBestFeature(data, targetAttribute)
        node.FeatureName = bestFeature
        
        ' Get unique values for the best feature
        Dim featureValues As List(Of String) = data.Select(Function(row) row(bestFeature)).Distinct().ToList()
        
        ' Create child nodes for each feature value
        For Each value As String In featureValues
            ' Filter data for this feature value
            Dim subset As List(Of Dictionary(Of String, String)) = data.Where(Function(row) row(bestFeature) = value).ToList()
            
            If subset.Count = 0 Then
                ' Create leaf with most common target value
                Dim leafNode As New DecisionTreeNode()
                leafNode.IsLeaf = True
                leafNode.Prediction = GetMostCommonValue(targetValues)
                node.Children(value) = leafNode
            Else
                ' Remove the feature from remaining features
                Dim remainingFeatures As List(Of String) = _features.Where(Function(f) f <> bestFeature).ToList()
                
                ' Create subtree
                Dim childNode As DecisionTreeNode = BuildTree(subset, targetAttribute)
                node.Children(value) = childNode
            End If
        Next
        
        Return node
    End Function
    
    Private Function GetBestFeature(data As List(Of Dictionary(Of String, String)), targetAttribute As String) As String
        Dim bestFeature As String = ""
        Dim bestGain As Double = -1
        
        For Each feature As String In _features
            If feature = targetAttribute Then Continue For
            
            Dim gain As Double = CalculateInformationGain(data, feature, targetAttribute)
            If gain > bestGain Then
                bestGain = gain
                bestFeature = feature
            End If
        Next
        
        Return bestFeature
    End Function
    
    Private Function CalculateInformationGain(data As List(Of Dictionary(Of String, String)), feature As String, targetAttribute As String) As Double
        Dim totalEntropy As Double = CalculateEntropy(data, targetAttribute)
        Dim weightedEntropy As Double = 0
        
        Dim featureValues As List(Of String) = data.Select(Function(row) row(feature)).Distinct().ToList()
        
        For Each value As String In featureValues
            Dim subset As List(Of Dictionary(Of String, String)) = data.Where(Function(row) row(feature) = value).ToList()
            
            If subset.Count > 0 Then
                Dim subsetEntropy As Double = CalculateEntropy(subset, targetAttribute)
                weightedEntropy += (subset.Count / data.Count) * subsetEntropy
            End If
        Next
        
        Return totalEntropy - weightedEntropy
    End Function
    
    Private Function CalculateEntropy(data As List(Of Dictionary(Of String, String)), targetAttribute As String) As Double
        Dim entropy As Double = 0
        Dim targetValues As List(Of String) = data.Select(Function(row) row(targetAttribute)).ToList()
        Dim total As Integer = targetValues.Count
        
        Dim uniqueValues As List(Of String) = targetValues.Distinct().ToList()
        
        For Each value As String In uniqueValues
            Dim count As Integer = targetValues.Count(Function(v) v = value)
            Dim probability As Double = count / total
            
            If probability > 0 Then
                entropy -= probability * Math.Log(probability, 2)
            End If
        Next
        
        Return entropy
    End Function
    
    Private Function GetMostCommonValue(values As List(Of String)) As String
        Dim grouped As Dictionary(Of String, Integer) = values.GroupBy(Function(v) v).ToDictionary(Function(g) g.Key, Function(g) g.Count())
        Return grouped.OrderByDescending(Function(kvp) kvp.Value).First().Key
    End Function
    
    ' Make prediction for a single instance
    Public Function Predict(instance As Dictionary(Of String, String)) As String
        Return PredictRecursive(_root, instance)
    End Function
    
    Private Function PredictRecursive(node As DecisionTreeNode, instance As Dictionary(Of String, String)) As String
        If node.IsLeaf Then
            Return node.Prediction
        End If
        
        Dim featureValue As String = instance(node.FeatureName)
        
        If node.Children.ContainsKey(featureValue) Then
            Return PredictRecursive(node.Children(featureValue), instance)
        Else
            ' If feature value not found, return most common value
            Return node.Children.Values.First().Prediction
        End If
    End Function
    
    ' Print the decision tree
    Public Sub PrintTree()
        PrintTreeRecursive(_root, 0)
    End Sub
    
    Private Sub PrintTreeRecursive(node As DecisionTreeNode, depth As Integer)
        Dim indent As String = New String("  ", depth)
        
        If node.IsLeaf Then
            Console.WriteLine($"{indent}Predict: {node.Prediction}")
        Else
            Console.WriteLine($"{indent}{node.FeatureName}")
            For Each kvp As KeyValuePair(Of String, DecisionTreeNode) In node.Children
                Console.WriteLine($"{indent}  {kvp.Key} ->")
                PrintTreeRecursive(kvp.Value, depth + 2)
            Next
        End If
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Define features and target values
        Dim features As New List(Of String) From {"Outlook", "Temperature", "Humidity", "Wind"}
        Dim targetValues As New List(Of String) From {"Yes", "No"}
        
        ' Create sample training data
        Dim trainingData As New List(Of Dictionary(Of String, String)) From {
            New Dictionary(Of String, String) From {{"Outlook", "Sunny"}, {"Temperature", "Hot"}, {"Humidity", "High"}, {"Wind", "Weak"}, {"Play", "No"}},
            New Dictionary(Of String, String) From {{"Outlook", "Sunny"}, {"Temperature", "Hot"}, {"Humidity", "High"}, {"Wind", "Strong"}, {"Play", "No"}},
            New Dictionary(Of String, String) From {{"Outlook", "Overcast"}, {"Temperature", "Hot"}, {"Humidity", "High"}, {"Wind", "Weak"}, {"Play", "Yes"}},
            New Dictionary(Of String, String) From {{"Outlook", "Rain"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Weak"}, {"Play", "Yes"}},
            New Dictionary(Of String, String) From {{"Outlook", "Rain"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Weak"}, {"Play", "Yes"}},
            New Dictionary(Of String, String) From {{"Outlook", "Rain"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Strong"}, {"Play", "No"}},
            New Dictionary(Of String, String) From {{"Outlook", "Overcast"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Strong"}, {"Play", "Yes"}},
            New Dictionary(Of String, String) From {{"Outlook", "Sunny"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Weak"}, {"Play", "No"}},
            New Dictionary(Of String, String) From {{"Outlook", "Sunny"}, {"Temperature", "Cool"}, {"Humidity", "Normal"}, {"Wind", "Weak"}, {"Play", "Yes"}},
            New Dictionary(Of String, String) From {{"Outlook", "Rain"}, {"Temperature", "Mild"}, {"Humidity", "Normal"}, {"Wind", "Weak"}, {"Play", "Yes"}},
            New Dictionary(Of String, String) From {{"Outlook", "Sunny"}, {"Temperature", "Mild"}, {"Humidity", "Normal"}, {"Wind", "Strong"}, {"Play", "Yes"}},
            New Dictionary(Of String, String) From {{"Outlook", "Overcast"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Strong"}, {"Play", "Yes"}},
            New Dictionary(Of String, String) From {{"Outlook", "Overcast"}, {"Temperature", "Hot"}, {"Humidity", "Normal"}, {"Wind", "Weak"}, {"Play", "Yes"}},
            New Dictionary(Of String, String) From {{"Outlook", "Rain"}, {"Temperature", "Mild"}, {"Humidity", "High"}, {"Wind", "Strong"}, {"Play", "No"}}
        }
        
        ' Create and train the decision tree
        Dim tree As New DecisionTree(features, targetValues)
        tree.Train(trainingData, "Play")
        
        ' Print the tree structure
        Console.WriteLine("Decision Tree:")
        tree.PrintTree()
        
        ' Make predictions
        Console.WriteLine(vbNewLine & "Predictions:")
        
        Dim testInstance As New Dictionary(Of String, String) From {
            {"Outlook", "Sunny"},
            {"Temperature", "Cool"},
            {"Humidity", "High"},
            {"Wind", "Strong"}
        }
        
        Dim prediction As String = tree.Predict(testInstance)
        Console.WriteLine($"Instance: {String.Join(", ", testInstance.Select(Function(kvp) $"{kvp.Key}={kvp.Value}"))}")
        Console.WriteLine($"Prediction: {prediction}")
        
        Console.ReadKey()
    End Sub
End Module
```

## Key Components of the Implementation:

### 1. **DecisionTreeNode Class**
- Represents a node in the decision tree
- Contains feature name, feature value, leaf status, prediction, and child nodes

### 2. **DecisionTree Class**
- **BuildTree()**: Recursively builds the decision tree using ID3 algorithm
- **GetBestFeature()**: Selects the best feature to split on using information gain
- **CalculateInformationGain()**: Computes information gain for feature selection
- **CalculateEntropy()**: Calculates entropy for information gain computation
- **Predict()**: Makes predictions for new instances
- **PrintTree()**: Visualizes the decision tree structure

### 3. **Algorithm Steps**:
1. **Base Cases**: 
   - If all target values are the same → create leaf node
   - If no features left → create leaf with most common target value
2. **Feature Selection**: Choose best feature using information gain
3. **Recursive Splitting**: Create child nodes for each feature value
4. **Tree Construction**: Continue recursively until base cases are met

### 4. **Key Features**:
- Uses ID3 algorithm with information gain
- Handles missing values gracefully
- Provides tree visualization
- Supports prediction for new instances
- Includes entropy calculation for information gain

This implementation demonstrates the core concepts of decision tree learning in Visual Basic .NET, showing how to build, train, and use a decision tree classifier.

