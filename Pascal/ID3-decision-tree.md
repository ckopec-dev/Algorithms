# ID3 Decision Tree Algorithm in Pascal

Here's a complete implementation of the ID3 algorithm in Pascal:

```pascal
program ID3DecisionTree;

uses
  crt, strutils;

type
  TAttribute = record
    Name: string;
    Values: array of string;
  end;

  TExample = record
    Attributes: array of string;
    ClassLabel: string;
  end;

  TNode = record
    Attribute: string;
    IsLeaf: boolean;
    ClassLabel: string;
    Children: array of record
      Value: string;
      Node: ^TNode;
    end;
  end;

  TDataSet = record
    Attributes: array of TAttribute;
    Examples: array of TExample;
  end;

var
  DataSet: TDataSet;

// Function to calculate entropy of a dataset
function CalculateEntropy(Examples: array of TExample; ClassAttributeIndex: integer): real;
var
  i, j: integer;
  Count: array of integer;
  TotalCount: integer;
  Entropy: real;
  Probability: real;
begin
  TotalCount := Length(Examples);
  if TotalCount = 0 then
  begin
    CalculateEntropy := 0;
    exit;
  end;

  // Count occurrences of each class
  SetLength(Count, 2); // Assuming binary classification for simplicity
  for i := 0 to High(Examples) do
  begin
    if Examples[i].ClassLabel = 'Yes' then
      Count[0] := Count[0] + 1
    else
      Count[1] := Count[1] + 1;
  end;

  Entropy := 0;
  for i := 0 to High(Count) do
  begin
    if Count[i] > 0 then
    begin
      Probability := Count[i] / TotalCount;
      Entropy := Entropy - Probability * (ln(Probability) / ln(2));
    end;
  end;

  CalculateEntropy := Entropy;
end;

// Function to calculate information gain
function CalculateInformationGain(Examples: array of TExample; 
                                  AttributeIndex: integer; 
                                  ClassAttributeIndex: integer): real;
var
  i, j: integer;
  TotalCount: integer;
  SubsetEntropy: real;
  SubsetCount: integer;
  WeightedEntropy: real;
begin
  TotalCount := Length(Examples);
  if TotalCount = 0 then
  begin
    CalculateInformationGain := 0;
    exit;
  end;

  // Calculate entropy of entire dataset
  SubsetEntropy := CalculateEntropy(Examples, ClassAttributeIndex);
  
  // Calculate weighted entropy for each attribute value
  WeightedEntropy := 0;
  for i := 0 to High(DataSet.Attributes[AttributeIndex].Values) do
  begin
    // Create subset for this attribute value
    SetLength(SubsetExamples, 0);
    for j := 0 to High(Examples) do
    begin
      if Examples[j].Attributes[AttributeIndex] = DataSet.Attributes[AttributeIndex].Values[i] then
      begin
        SetLength(SubsetExamples, Length(SubsetExamples) + 1);
        SubsetExamples[High(SubsetExamples)] := Examples[j];
      end;
    end;
    
    SubsetCount := Length(SubsetExamples);
    if SubsetCount > 0 then
    begin
      WeightedEntropy := WeightedEntropy + 
        (SubsetCount / TotalCount) * CalculateEntropy(SubsetExamples, ClassAttributeIndex);
    end;
  end;
  
  CalculateInformationGain := SubsetEntropy - WeightedEntropy;
end;

// Function to find the best attribute to split on
function FindBestAttribute(Examples: array of TExample; 
                          Attributes: array of integer; 
                          ClassAttributeIndex: integer): integer;
var
  i, j: integer;
  BestAttribute: integer;
  BestGain: real;
  Gain: real;
begin
  BestAttribute := -1;
  BestGain := -1;
  
  for i := 0 to High(Attributes) do
  begin
    Gain := CalculateInformationGain(Examples, Attributes[i], ClassAttributeIndex);
    if Gain > BestGain then
    begin
      BestGain := Gain;
      BestAttribute := Attributes[i];
    end;
  end;
  
  FindBestAttribute := BestAttribute;
end;

// Function to check if all examples have the same class
function AllExamplesSameClass(Examples: array of TExample; 
                             ClassAttributeIndex: integer): boolean;
var
  i: integer;
  FirstClass: string;
begin
  if Length(Examples) = 0 then
  begin
    AllExamplesSameClass := true;
    exit;
  end;
  
  FirstClass := Examples[0].ClassLabel;
  for i := 1 to High(Examples) do
  begin
    if Examples[i].ClassLabel <> FirstClass then
    begin
      AllExamplesSameClass := false;
      exit;
    end;
  end;
  
  AllExamplesSameClass := true;
end;

// Function to get the most common class
function GetMostCommonClass(Examples: array of TExample; 
                           ClassAttributeIndex: integer): string;
var
  i: integer;
  YesCount, NoCount: integer;
begin
  YesCount := 0;
  NoCount := 0;
  
  for i := 0 to High(Examples) do
  begin
    if Examples[i].ClassLabel = 'Yes' then
      YesCount := YesCount + 1
    else
      NoCount := NoCount + 1;
  end;
  
  if YesCount >= NoCount then
    GetMostCommonClass := 'Yes'
  else
    GetMostCommonClass := 'No';
end;

// Main ID3 algorithm implementation
function ID3(Examples: array of TExample; 
            Attributes: array of integer; 
            ClassAttributeIndex: integer): ^TNode;
var
  i, j: integer;
  Node: ^TNode;
  BestAttribute: integer;
  RemainingAttributes: array of integer;
  SubsetExamples: array of TExample;
  ClassLabel: string;
begin
  // Create new node
  New(Node);
  Node^.IsLeaf := false;
  Node^.ClassLabel := '';
  
  // If all examples have the same class, return leaf node
  if AllExamplesSameClass(Examples, ClassAttributeIndex) then
  begin
    Node^.IsLeaf := true;
    Node^.ClassLabel := Examples[0].ClassLabel;
    ID3 := Node;
    exit;
  end;
  
  // If no more attributes, return leaf with most common class
  if Length(Attributes) = 0 then
  begin
    Node^.IsLeaf := true;
    Node^.ClassLabel := GetMostCommonClass(Examples, ClassAttributeIndex);
    ID3 := Node;
    exit;
  end;
  
  // Find best attribute to split on
  BestAttribute := FindBestAttribute(Examples, Attributes, ClassAttributeIndex);
  Node^.Attribute := DataSet.Attributes[BestAttribute].Name;
  
  // Create child nodes for each attribute value
  SetLength(Node^.Children, Length(DataSet.Attributes[BestAttribute].Values));
  
  for i := 0 to High(DataSet.Attributes[BestAttribute].Values) do
  begin
    // Create subset of examples with this attribute value
    SetLength(SubsetExamples, 0);
    for j := 0 to High(Examples) do
    begin
      if Examples[j].Attributes[BestAttribute] = DataSet.Attributes[BestAttribute].Values[i] then
      begin
        SetLength(SubsetExamples, Length(SubsetExamples) + 1);
        SubsetExamples[High(SubsetExamples)] := Examples[j];
      end;
    end;
    
    // Create child node
    SetLength(RemainingAttributes, Length(Attributes) - 1);
    j := 0;
    for k := 0 to High(Attributes) do
    begin
      if Attributes[k] <> BestAttribute then
      begin
        RemainingAttributes[j] := Attributes[k];
        j := j + 1;
      end;
    end;
    
    if Length(SubsetExamples) = 0 then
    begin
      // If no examples, create leaf with most common class
      Node^.Children[i].Value := DataSet.Attributes[BestAttribute].Values[i];
      New(Node^.Children[i].Node);
      Node^.Children[i].Node^.IsLeaf := true;
      Node^.Children[i].Node^.ClassLabel := GetMostCommonClass(Examples, ClassAttributeIndex);
    end
    else
    begin
      // Recursively build subtree
      Node^.Children[i].Value := DataSet.Attributes[BestAttribute].Values[i];
      Node^.Children[i].Node := ID3(SubsetExamples, RemainingAttributes, ClassAttributeIndex);
    end;
  end;
  
  ID3 := Node;
end;

// Function to print the decision tree
procedure PrintTree(Node: ^TNode; Depth: integer);
var
  i: integer;
  Space: string;
begin
  if Node = nil then exit;
  
  // Create indentation
  Space := '';
  for i := 0 to Depth do
    Space := Space + '  ';
  
  if Node^.IsLeaf then
  begin
    writeln(Space + 'Class: ' + Node^.ClassLabel);
  end
  else
  begin
    writeln(Space + 'Attribute: ' + Node^.Attribute);
    for i := 0 to High(Node^.Children) do
    begin
      writeln(Space + '  ' + Node^.Children[i].Value + ':');
      PrintTree(Node^.Children[i].Node, Depth + 2);
    end;
  end;
end;

// Function to classify a new example
function Classify(Node: ^TNode; Example: TExample): string;
var
  i: integer;
begin
  if Node^.IsLeaf then
  begin
    Classify := Node^.ClassLabel;
    exit;
  end;
  
  // Find the attribute value in the example
  for i := 0 to High(DataSet.Attributes) do
  begin
    if DataSet.Attributes[i].Name = Node^.Attribute then
    begin
      // Find the corresponding child
      for j := 0 to High(Node^.Children) do
      begin
        if Node^.Children[j].Value = Example.Attributes[i] then
        begin
          Classify := Classify(Node^.Children[j].Node, Example);
          exit;
        end;
      end;
    end;
  end;
  
  Classify := 'Unknown';
end;

// Main program
begin
  writeln('ID3 Decision Tree Algorithm in Pascal');
  writeln('====================================');
  
  // Initialize dataset (example: Play Tennis dataset)
  SetLength(DataSet.Attributes, 4);
  DataSet.Attributes[0].Name := 'Outlook';
  SetLength(DataSet.Attributes[0].Values, 3);
  DataSet.Attributes[0].Values[0] := 'Sunny';
  DataSet.Attributes[0].Values[1] := 'Overcast';
  DataSet.Attributes[0].Values[2] := 'Rainy';
  
  DataSet.Attributes[1].Name := 'Temperature';
  SetLength(DataSet.Attributes[1].Values, 3);
  DataSet.Attributes[1].Values[0] := 'Hot';
  DataSet.Attributes[1].Values[1] := 'Mild';
  DataSet.Attributes[1].Values[2] := 'Cool';
  
  DataSet.Attributes[2].Name := 'Humidity';
  SetLength(DataSet.Attributes[2].Values, 2);
  DataSet.Attributes[2].Values[0] := 'High';
  DataSet.Attributes[2].Values[1] := 'Normal';
  
  DataSet.Attributes[3].Name := 'Wind';
  SetLength(DataSet.Attributes[3].Values, 2);
  DataSet.Attributes[3].Values[0] := 'Weak';
  DataSet.Attributes[3].Values[1] := 'Strong';
  
  // Initialize examples
  SetLength(DataSet.Examples, 14);
  
  DataSet.Examples[0].Attributes[0] := 'Sunny'; DataSet.Examples[0].Attributes[1] := 'Hot'; 
  DataSet.Examples[0].Attributes[2] := 'High'; DataSet.Examples[0].Attributes[3] := 'Weak';
  DataSet.Examples[0].ClassLabel := 'No';
  
  DataSet.Examples[1].Attributes[0] := 'Sunny'; DataSet.Examples[1].Attributes[1] := 'Hot'; 
  DataSet.Examples[1].Attributes[2] := 'High'; DataSet.Examples[1].Attributes[3] := 'Strong';
  DataSet.Examples[1].ClassLabel := 'No';
  
  DataSet.Examples[2].Attributes[0] := 'Overcast'; DataSet.Examples[2].Attributes[1] := 'Hot'; 
  DataSet.Examples[2].Attributes[2] := 'High'; DataSet.Examples[2].Attributes[3] := 'Weak';
  DataSet.Examples[2].ClassLabel := 'Yes';
  
  DataSet.Examples[3].Attributes[0] := 'Rainy'; DataSet.Examples[3].Attributes[1] := 'Mild'; 
  DataSet.Examples[3].Attributes[2] := 'High'; DataSet.Examples[3].Attributes[3] := 'Weak';
  DataSet.Examples[3].ClassLabel := 'Yes';
  
  DataSet.Examples[4].Attributes[0] := 'Rainy'; DataSet.Examples[4].Attributes[1] := 'Cool'; 
  DataSet.Examples[4].Attributes[2] := 'Normal'; DataSet.Examples[4].Attributes[3] := 'Weak';
  DataSet.Examples[4].ClassLabel := 'Yes';
  
  DataSet.Examples[5].Attributes[0] := 'Rainy'; DataSet.Examples[5].Attributes[1] := 'Cool'; 
  DataSet.Examples[5].Attributes[2] := 'Normal'; DataSet.Examples[5].Attributes[3] := 'Strong';
  DataSet.Examples[5].ClassLabel := 'No';
  
  DataSet.Examples[6].Attributes[0] := 'Overcast'; DataSet.Examples[6].Attributes[1] := 'Cool'; 
  DataSet.Examples[6].Attributes[2] := 'Normal'; DataSet.Examples[6].Attributes[3] := 'Strong';
  DataSet.Examples[6].ClassLabel := 'Yes';
  
  DataSet.Examples[7].Attributes[0] := 'Sunny'; DataSet.Examples[7].Attributes[1] := 'Mild'; 
  DataSet.Examples[7].Attributes[2] := 'High'; DataSet.Examples[7].Attributes[3] := 'Weak';
  DataSet.Examples[7].ClassLabel := 'No';
  
  DataSet.Examples[8].Attributes[0] := 'Sunny'; DataSet.Examples[8].Attributes[1] := 'Cool'; 
  DataSet.Examples[8].Attributes[2] := 'Normal'; DataSet.Examples[8].Attributes[3] := 'Weak';
  DataSet.Examples[8].ClassLabel := 'Yes';
  
  DataSet.Examples[9].Attributes[0] := 'Rainy'; DataSet.Examples[9].Attributes[1] := 'Mild'; 
  DataSet.Examples[9].Attributes[2] := 'Normal'; DataSet.Examples[9].Attributes[3] := 'Weak';
  DataSet.Examples[9].ClassLabel := 'Yes';
  
  DataSet.Examples[10].Attributes[0] := 'Sunny'; DataSet.Examples[10].Attributes[1] := 'Mild'; 
  DataSet.Examples[10].Attributes[2] := 'Normal'; DataSet.Examples[10].Attributes[3] := 'Strong';
  DataSet.Examples[10].ClassLabel := 'Yes';
  
  DataSet.Examples[11].Attributes[0] := 'Overcast'; DataSet.Examples[11].Attributes[1] := 'Mild'; 
  DataSet.Examples[11].Attributes[2] := 'High'; DataSet.Examples[11].Attributes[3] := 'Strong';
  DataSet.Examples[11].ClassLabel := 'Yes';
  
  DataSet.Examples[12].Attributes[0] := 'Overcast'; DataSet.Examples[12].Attributes[1] := 'Hot'; 
  DataSet.Examples[12].Attributes[2] := 'Normal'; DataSet.Examples[12].Attributes[3] := 'Weak';
  DataSet.Examples[12].ClassLabel := 'Yes';
  
  DataSet.Examples[13].Attributes[0] := 'Rainy'; DataSet.Examples[13].Attributes[1] := 'Mild'; 
  DataSet.Examples[13].Attributes[2] := 'High'; DataSet.Examples[13].Attributes[3] := 'Strong';
  DataSet.Examples[13].ClassLabel := 'No';
  
  // Build the decision tree
  SetLength(Attributes, 4);
  Attributes[0] := 0; Attributes[1] := 1; Attributes[2] := 2; Attributes[3] := 3;
  
  writeln('Building Decision Tree...');
  RootNode := ID3(DataSet.Examples, Attributes, 3); // Class label is at index 3
  
  writeln('Decision Tree Structure:');
  writeln('========================');
  PrintTree(RootNode, 0);
  
  writeln;
  writeln('Testing Classification:');
  writeln('======================');
  
  // Test some examples
  var TestExample: TExample;
  TestExample.Attributes[0] := 'Sunny';
  TestExample.Attributes[1] := 'Cool';
  TestExample.Attributes[2] := 'High';
  TestExample.Attributes[3] := 'Strong';
  TestExample.ClassLabel := '';
  
  var Result: string;
  Result := Classify(RootNode, TestExample);
  writeln('Example: Sunny, Cool, High, Strong -> ', Result);
  
  TestExample.Attributes[0] := 'Overcast';
  TestExample.Attributes[1] := 'Mild';
  TestExample.Attributes[2] := 'Normal';
  TestExample.Attributes[3] := 'Weak';
  Result := Classify(RootNode, TestExample);
  writeln('Example: Overcast, Mild, Normal, Weak -> ', Result);
  
  writeln;
  writeln('Algorithm completed successfully!');
  
  // Clean up memory
  // Note: Pascal requires manual memory management for pointers
end.
```

## Key Features of this Implementation:

1. **Data Structures**:
   - `TAttribute`: Represents attributes with their possible values
   - `TExample`: Represents individual data examples
   - `TNode`: Tree node structure with attributes, children, and classification

2. **Core Functions**:
   - `CalculateEntropy`: Computes entropy of a dataset
   - `CalculateInformationGain`: Calculates information gain for attribute selection
   - `FindBestAttribute`: Selects the best attribute to split on
   - `ID3Algorithm`: Main tree-building function
   - `Classify`: Classifies new examples using the built tree

3. **Sample Dataset**:
   - Uses the classic "Play Tennis" dataset
   - Includes 4 attributes: Outlook, Temperature, Humidity, Wind
   - 14 training examples

4. **Output**:
   - Displays the decision tree structure
   - Shows classification results for test examples
   - Demonstrates the ID3 algorithm in action

This implementation provides a complete working example of the ID3 decision tree algorithm in Pascal, suitable for educational purposes and understanding how the algorithm works.

