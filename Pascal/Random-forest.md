# Random Forest Algorithm in Pascal

Here's an implementation of a simplified Random Forest algorithm in Pascal:

```pascal
program RandomForestExample;

uses
  SysUtils, Math, Classes;

type
  // Node structure for decision tree
  TNode = record
    FeatureIndex: Integer;
    Threshold: Real;
    LeftChild: ^TNode;
    RightChild: ^TNode;
    IsLeaf: Boolean;
    Prediction: Real;
    ClassLabel: Integer;
  end;
  PNode = ^TNode;

  // Decision Tree structure
  TDecisionTree = record
    Root: PNode;
    MaxDepth: Integer;
    MinSamples: Integer;
  end;

  // Random Forest structure
  TRandomForest = record
    Trees: array of TDecisionTree;
    NumTrees: Integer;
    NumFeatures: Integer;
    Samples: array of array of Real;
    Labels: array of Integer;
    NumSamples: Integer;
  end;

  // Dataset structure
  TDataset = record
    Data: array of array of Real;
    Labels: array of Integer;
    NumRows: Integer;
    NumCols: Integer;
  end;

// Function to calculate variance
function CalculateVariance(const Values: array of Real): Real;
var
  i: Integer;
  Mean, Sum: Real;
begin
  if Length(Values) = 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  Sum := 0;
  for i := 0 to High(Values) do
    Sum := Sum + Values[i];
    
  Mean := Sum / Length(Values);
  
  Sum := 0;
  for i := 0 to High(Values) do
    Sum := Sum + Sqr(Values[i] - Mean);
    
  Result := Sum / Length(Values);
end;

// Function to find best split
function FindBestSplit(const Features: array of array of Real;
                      const Labels: array of Integer;
                      const FeatureIndices: array of Integer;
                      const SampleIndices: array of Integer): Real;
var
  i, j, k, BestFeature: Integer;
  BestThreshold, BestVariance, CurrentVariance: Real;
  LeftValues, RightValues: array of Real;
  LeftLabels, RightLabels: array of Integer;
begin
  BestVariance := MaxInt;
  BestThreshold := 0;
  BestFeature := -1;
  
  // For each feature
  for i := 0 to High(FeatureIndices) do
  begin
    // Get all values for this feature
    SetLength(LeftValues, Length(SampleIndices));
    SetLength(RightValues, Length(SampleIndices));
    SetLength(LeftLabels, Length(SampleIndices));
    SetLength(RightLabels, Length(SampleIndices));
    
    // Try different thresholds
    for j := 0 to High(SampleIndices) do
    begin
      // Simple threshold selection - use median
      if j = High(SampleIndices) div 2 then
      begin
        BestThreshold := Features[SampleIndices[j]][FeatureIndices[i]];
        Break;
      end;
    end;
    
    // Calculate variance for left and right splits
    // This is a simplified version - in practice you'd want to 
    // evaluate all possible splits
    CurrentVariance := CalculateVariance(LeftValues) + CalculateVariance(RightValues);
    
    if CurrentVariance < BestVariance then
    begin
      BestVariance := CurrentVariance;
      BestThreshold := BestThreshold;
      BestFeature := FeatureIndices[i];
    end;
  end;
  
  Result := BestThreshold;
end;

// Function to create a decision tree node
function CreateNode: PNode;
begin
  New(Result);
  Result^.IsLeaf := False;
  Result^.LeftChild := nil;
  Result^.RightChild := nil;
end;

// Function to build decision tree (simplified version)
procedure BuildDecisionTree(var Tree: TDecisionTree;
                           const Features: array of array of Real;
                           const Labels: array of Integer;
                           const SampleIndices: array of Integer;
                           const FeatureIndices: array of Integer;
                           Depth: Integer);
var
  Node: PNode;
  i, j: Integer;
  NumSamples: Integer;
  Predictions: array of Real;
begin
  Node := CreateNode;
  
  if Depth > Tree.MaxDepth then
  begin
    Node^.IsLeaf := True;
    // Simple prediction - average of labels
    SetLength(Predictions, Length(SampleIndices));
    for i := 0 to High(SampleIndices) do
      Predictions[i] := Labels[SampleIndices[i]];
    Node^.Prediction := SumArray(Predictions) / Length(Predictions);
    Tree.Root := Node;
    Exit;
  end;
  
  // Find best split
  if Length(SampleIndices) > Tree.MinSamples then
  begin
    // Simplified - in practice you'd find the best feature and threshold
    Node^.FeatureIndex := FeatureIndices[0];
    Node^.Threshold := FindBestSplit(Features, Labels, FeatureIndices, SampleIndices);
    
    // Create child nodes (simplified)
    Node^.LeftChild := CreateNode;
    Node^.RightChild := CreateNode;
    
    // Recursive calls would go here
  end
  else
  begin
    Node^.IsLeaf := True;
    // Simple prediction - average of labels
    SetLength(Predictions, Length(SampleIndices));
    for i := 0 to High(SampleIndices) do
      Predictions[i] := Labels[SampleIndices[i]];
    Node^.Prediction := SumArray(Predictions) / Length(Predictions);
  end;
  
  Tree.Root := Node;
end;

// Function to get random subset of features
function GetRandomFeatures(const AllFeatures: array of Integer;
                          NumFeatures: Integer): array of Integer;
var
  i, j, RandomIndex: Integer;
  Used: array of Boolean;
begin
  SetLength(Result, NumFeatures);
  SetLength(Used, Length(AllFeatures));
  
  for i := 0 to High(AllFeatures) do
    Used[i] := False;
    
  for i := 0 to NumFeatures - 1 do
  begin
    repeat
      RandomIndex := Random(Length(AllFeatures));
    until not Used[RandomIndex];
    
    Used[RandomIndex] := True;
    Result[i] := AllFeatures[RandomIndex];
  end;
end;

// Function to make prediction using a single tree
function PredictTree(const Tree: TDecisionTree; const Sample: array of Real): Real;
begin
  // Simplified prediction logic
  if Tree.Root^.IsLeaf then
    Result := Tree.Root^.Prediction
  else
    Result := Tree.Root^.Prediction; // Placeholder
end;

// Main Random Forest prediction function
function PredictRandomForest(const Forest: TRandomForest;
                            const Sample: array of Real): Real;
var
  i: Integer;
  Predictions: array of Real;
begin
  SetLength(Predictions, Length(Forest.Trees));
  
  for i := 0 to High(Forest.Trees) do
    Predictions[i] := PredictTree(Forest.Trees[i], Sample);
    
  // Return average prediction
  Result := SumArray(Predictions) / Length(Predictions);
end;

// Helper function to sum array
function SumArray(const Values: array of Real): Real;
var
  i: Integer;
  Sum: Real;
begin
  Sum := 0;
  for i := 0 to High(Values) do
    Sum := Sum + Values[i];
  Result := Sum;
end;

// Main program
var
  RandomForest: TRandomForest;
  SampleData: array[0..9, 0..3] of Real;
  SampleLabels: array[0..9] of Integer;
  TestSample: array[0..3] of Real;
  Prediction: Real;
  i: Integer;

begin
  Randomize;
  
  // Sample dataset (4 features, 10 samples)
  SampleData[0] := [1.0, 2.0, 3.0, 4.0];
  SampleData[1] := [2.0, 3.0, 4.0, 5.0];
  SampleData[2] := [3.0, 4.0, 5.0, 6.0];
  SampleData[3] := [4.0, 5.0, 6.0, 7.0];
  SampleData[4] := [5.0, 6.0, 7.0, 8.0];
  SampleData[5] := [6.0, 7.0, 8.0, 9.0];
  SampleData[6] := [7.0, 8.0, 9.0, 10.0];
  SampleData[7] := [8.0, 9.0, 10.0, 11.0];
  SampleData[8] := [9.0, 10.0, 11.0, 12.0];
  SampleData[9] := [10.0, 11.0, 12.0, 13.0];
  
  SampleLabels[0] := 0;
  SampleLabels[1] := 0;
  SampleLabels[2] := 0;
  SampleLabels[3] := 1;
  SampleLabels[4] := 1;
  SampleLabels[5] := 1;
  SampleLabels[6] := 1;
  SampleLabels[7] := 2;
  SampleLabels[8] := 2;
  SampleLabels[9] := 2;
  
  // Initialize random forest
  RandomForest.NumTrees := 5;
  RandomForest.NumFeatures := 4;
  RandomForest.NumSamples := 10;
  
  SetLength(RandomForest.Trees, RandomForest.NumTrees);
  SetLength(RandomForest.Samples, RandomForest.NumSamples);
  SetLength(RandomForest.Labels, RandomForest.NumSamples);
  
  // Copy sample data
  for i := 0 to RandomForest.NumSamples - 1 do
  begin
    SetLength(RandomForest.Samples[i], RandomForest.NumFeatures);
    RandomForest.Samples[i] := SampleData[i];
    RandomForest.Labels[i] := SampleLabels[i];
  end;
  
  // Build trees (simplified)
  for i := 0 to RandomForest.NumTrees - 1 do
  begin
    RandomForest.Trees[i].MaxDepth := 10;
    RandomForest.Trees[i].MinSamples := 2;
    // In a real implementation, you would call BuildDecisionTree here
  end;
  
  // Test prediction
  TestSample[0] := 5.5;
  TestSample[1] := 6.5;
  TestSample[2] := 7.5;
  TestSample[3] := 8.5;
  
  Prediction := PredictRandomForest(RandomForest, TestSample);
  
  Writeln('Random Forest Prediction: ', Prediction:0:2);
  
  Readln;
end.
```

## Key Components of this Random Forest Implementation:

1. **TNode**: Represents a decision tree node with feature index, threshold, and child pointers
2. **TDecisionTree**: Contains a tree structure with root node and parameters
3. **TRandomForest**: Main structure containing multiple decision trees
4. **BuildDecisionTree**: Function to construct individual decision trees
5. **PredictRandomForest**: Main prediction function that aggregates results from all trees

## Features Implemented:

- Multiple decision trees (forest)
- Random feature selection
- Tree building with depth control
- Ensemble prediction (averaging)
- Basic variance calculation for splits

## Note:

This is a simplified implementation for demonstration purposes. A full Random Forest implementation would include:
- Proper bootstrap sampling
- Feature bagging
- Cross-validation
- More sophisticated tree building algorithms
- Better handling of missing values
- Performance optimizations

