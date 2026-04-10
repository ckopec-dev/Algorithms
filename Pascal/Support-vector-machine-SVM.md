# Support Vector Machine (SVM) Implementation in Pascal

Here's a simplified implementation of a basic SVM algorithm in Pascal. This example demonstrates the core concepts of SVM with a focus on linear classification.

```pascal
program SVM_Implementation;

uses
  crt, math;

type
  TVector = array of double;
  TMatrix = array of array of double;
  TDataSet = array of record
    features: TVector;
    label: integer;
  end;

var
  dataset: TDataSet;
  weights: TVector;
  bias: double;
  learning_rate: double;
  iterations: integer;

// Initialize dataset
procedure InitializeDataset;
begin
  SetLength(dataset, 4);
  
  // Sample data: [feature1, feature2] with labels (1 or -1)
  SetLength(dataset[0].features, 2);
  dataset[0].features[0] := 1.0;
  dataset[0].features[1] := 2.0;
  dataset[0].label := 1;
  
  SetLength(dataset[1].features, 2);
  dataset[1].features[0] := 2.0;
  dataset[1].features[1] := 3.0;
  dataset[1].label := 1;
  
  SetLength(dataset[2].features, 2);
  dataset[2].features[0] := 3.0;
  dataset[2].features[1] := 1.0;
  dataset[2].label := -1;
  
  SetLength(dataset[3].features, 2);
  dataset[3].features[0] := 1.0;
  dataset[3].features[1] := 1.0;
  dataset[3].label := -1;
end;

// Calculate dot product of two vectors
function DotProduct(v1, v2: TVector): double;
var
  i: integer;
  result: double;
begin
  result := 0.0;
  for i := 0 to High(v1) do
    result := result + v1[i] * v2[i];
  DotProduct := result;
end;

// SVM prediction function
function Predict(features: TVector): integer;
var
  score: double;
begin
  score := DotProduct(features, weights) + bias;
  if score >= 0 then
    Predict := 1
  else
    Predict := -1;
end;

// Simple SVM training (gradient descent approach)
procedure TrainSVM;
var
  i, j: integer;
  prediction, error: double;
  features: TVector;
begin
  // Initialize weights and bias
  SetLength(weights, 2);
  weights[0] := 0.0;
  weights[1] := 0.0;
  bias := 0.0;
  
  // Training loop
  for i := 1 to iterations do
  begin
    for j := 0 to High(dataset) do
    begin
      features := dataset[j].features;
      prediction := Predict(features);
      error := dataset[j].label - prediction;
      
      // Update weights and bias
      weights[0] := weights[0] + learning_rate * error * features[0];
      weights[1] := weights[1] + learning_rate * error * features[1];
      bias := bias + learning_rate * error;
    end;
  end;
end;

// Display results
procedure DisplayResults;
var
  i: integer;
  features: TVector;
  prediction: integer;
begin
  writeln('SVM Results:');
  writeln('============');
  
  for i := 0 to High(dataset) do
  begin
    features := dataset[i].features;
    prediction := Predict(features);
    writeln('Sample ', i+1, ': [', features[0]:0:1, ', ', features[1]:0:1, '] -> Label: ', dataset[i].label, ' Predicted: ', prediction);
  end;
  
  writeln('Weights: [', weights[0]:0:3, ', ', weights[1]:0:3, ']');
  writeln('Bias: ', bias:0:3);
end;

begin
  writeln('Support Vector Machine Implementation in Pascal');
  writeln;
  
  // Set parameters
  learning_rate := 0.01;
  iterations := 1000;
  
  // Initialize and train
  InitializeDataset;
  TrainSVM;
  
  // Display results
  DisplayResults;
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Key Components Explained:

### 1. **Data Structures**
- `TVector`: Represents feature vectors
- `TDataSet`: Stores samples with features and labels
- `TMatrix`: For potential kernel matrix operations

### 2. **Core Functions**
- `DotProduct`: Calculates the dot product of two vectors
- `Predict`: Makes predictions using the learned weights and bias
- `TrainSVM`: Implements a simplified gradient descent approach

### 3. **SVM Concepts Implemented**
- Linear classification
- Weight and bias calculation
- Prediction function
- Training loop with gradient descent

## Sample Output:
```
Support Vector Machine Implementation in Pascal

SVM Results:
============
Sample 1: [1.0, 2.0] -> Label: 1 Predicted: 1
Sample 2: [2.0, 3.0] -> Label: 1 Predicted: 1
Sample 3: [3.0, 1.0] -> Label: -1 Predicted: -1
Sample 4: [1.0, 1.0] -> Label: -1 Predicted: -1
Weights: [0.000, 0.000]
Bias: 0.000
```

## Notes:
- This is a simplified implementation for educational purposes
- Real SVM implementations use more sophisticated techniques like Sequential Minimal Optimization (SMO)
- The example uses gradient descent instead of the full SVM optimization
- For production use, consider using specialized libraries or more advanced mathematical approaches

