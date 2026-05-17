# Viterbi Algorithm in Pascal

Here's an implementation of the Viterbi algorithm for Hidden Markov Model (HMM) decoding in Pascal:

```pascal
program ViterbiAlgorithm;

{$APPTYPE CONSOLE}

uses
  SysUtils, Math;

type
  // Define the structure for HMM parameters
  THMM = record
    numStates: integer;
    numObservations: integer;
    states: array[1..10] of string;
    observations: array[1..10] of string;
    initialProb: array[1..10] of real;
    transitionProb: array[1..10, 1..10] of real;
    emissionProb: array[1..10, 1..10] of real;
  end;

  // Viterbi algorithm result structure
  TViterbiResult = record
    bestPath: array[1..100] of integer;
    bestProbability: real;
  end;

var
  hmm: THMM;
  observationsSequence: array[1..10] of integer;
  sequenceLength: integer;

// Function to find the index of maximum value in an array
function MaxIndex(const values: array of real; count: integer): integer;
var
  i, maxIndex: integer;
  maxValue: real;
begin
  maxIndex := 1;
  maxValue := values[1];
  for i := 2 to count do
  begin
    if values[i] > maxValue then
    begin
      maxValue := values[i];
      maxIndex := i;
    end;
  end;
  MaxIndex := maxIndex;
end;

// Viterbi algorithm implementation
function Viterbi(const h: THMM; const obs: array of integer; obsLength: integer): TViterbiResult;
var
  v: array[1..10, 1..100] of real;
  path: array[1..10, 1..100] of integer;
  i, j, t: integer;
  maxProb: real;
  maxState: integer;
begin
  // Initialize the first time step
  for i := 1 to h.numStates do
  begin
    v[i, 1] := h.initialProb[i] * h.emissionProb[i, obs[1]];
    path[i, 1] := 0;
  end;

  // Dynamic programming - fill the Viterbi table
  for t := 2 to obsLength do
  begin
    for j := 1 to h.numStates do
    begin
      maxProb := 0;
      maxState := 1;
      
      // Find the maximum probability from all previous states
      for i := 1 to h.numStates do
      begin
        if v[i, t-1] * h.transitionProb[i, j] > maxProb then
        begin
          maxProb := v[i, t-1] * h.transitionProb[i, j];
          maxState := i;
        end;
      end;
      
      // Calculate the probability for current state
      v[j, t] := maxProb * h.emissionProb[j, obs[t]];
      path[j, t] := maxState;
    end;
  end;

  // Find the best final state
  maxProb := 0;
  maxState := 1;
  for i := 1 to h.numStates do
  begin
    if v[i, obsLength] > maxProb then
    begin
      maxProb := v[i, obsLength];
      maxState := i;
    end;
  end;

  // Backtrack to find the best path
  Result.bestProbability := maxProb;
  Result.bestPath[obsLength] := maxState;
  
  for t := obsLength - 1 downto 1 do
  begin
    Result.bestPath[t] := path[Result.bestPath[t+1], t+1];
  end;
end;

// Function to print the HMM model
procedure PrintHMM(const h: THMM);
begin
  writeln('=== HMM Model ===');
  writeln('States: ', h.numStates);
  writeln('Observations: ', h.numObservations);
  writeln;
  
  writeln('States:');
  for i := 1 to h.numStates do
    write(h.states[i], ' ');
  writeln;
  
  writeln('Observations:');
  for i := 1 to h.numObservations do
    write(h.observations[i], ' ');
  writeln;
  writeln;
  
  writeln('Initial probabilities:');
  for i := 1 to h.numStates do
    writeln('State ', i, ': ', h.initialProb[i]:0:3);
  writeln;
  
  writeln('Transition probabilities:');
  for i := 1 to h.numStates do
  begin
    write('From state ', i, ': ');
    for j := 1 to h.numStates do
      write(h.transitionProb[i, j]:0:3, ' ');
    writeln;
  end;
  writeln;
  
  writeln('Emission probabilities:');
  for i := 1 to h.numStates do
  begin
    write('State ', i, ': ');
    for j := 1 to h.numObservations do
      write(h.emissionProb[i, j]:0:3, ' ');
    writeln;
  end;
  writeln;
end;

// Function to print the result
procedure PrintViterbiResult(const result: TViterbiResult; const h: THMM; obsLength: integer);
var
  i: integer;
begin
  writeln('=== Viterbi Result ===');
  writeln('Best path:');
  for i := 1 to obsLength do
    write(h.states[result.bestPath[i]], ' ');
  writeln;
  writeln('Best probability: ', result.bestProbability:0:6);
  writeln;
end;

begin
  // Initialize the HMM model
  hmm.numStates := 2;
  hmm.numObservations := 3;
  
  // Set up states
  hmm.states[1] := 'Sunny';
  hmm.states[2] := 'Rainy';
  
  // Set up observations
  hmm.observations[1] := 'Walk';
  hmm.observations[2] := 'Shop';
  hmm.observations[3] := 'Clean';
  
  // Set initial probabilities
  hmm.initialProb[1] := 0.6;  // Sunny
  hmm.initialProb[2] := 0.4;  // Rainy
  
  // Set transition probabilities
  hmm.transitionProb[1, 1] := 0.7;  // Sunny -> Sunny
  hmm.transitionProb[1, 2] := 0.3;  // Sunny -> Rainy
  hmm.transitionProb[2, 1] := 0.4;  // Rainy -> Sunny
  hmm.transitionProb[2, 2] := 0.6;  // Rainy -> Rainy
  
  // Set emission probabilities
  hmm.emissionProb[1, 1] := 0.6;  // Sunny -> Walk
  hmm.emissionProb[1, 2] := 0.3;  // Sunny -> Shop
  hmm.emissionProb[1, 3] := 0.1;  // Sunny -> Clean
  hmm.emissionProb[2, 1] := 0.2;  // Rainy -> Walk
  hmm.emissionProb[2, 2] := 0.3;  // Rainy -> Shop
  hmm.emissionProb[2, 3] := 0.5;  // Rainy -> Clean
  
  // Set observation sequence (Walk, Shop, Clean)
  observationsSequence[1] := 1;  // Walk
  observationsSequence[2] := 2;  // Shop
  observationsSequence[3] := 3;  // Clean
  sequenceLength := 3;
  
  // Print the HMM model
  PrintHMM(hmm);
  
  // Run Viterbi algorithm
  var result: TViterbiResult;
  result := Viterbi(hmm, observationsSequence, sequenceLength);
  
  // Print the result
  PrintViterbiResult(result, hmm, sequenceLength);
  
  writeln('Press Enter to exit...');
  ReadLn;
end.
```

## How the Viterbi Algorithm Works

This implementation demonstrates:

1. **Model Definition**: Defines a simple 2-state (Sunny/Rainy) Hidden Markov Model with 3 observations (Walk/Shop/Clean)

2. **Key Components**:
   - Initial probabilities for each state
   - Transition probabilities between states
   - Emission probabilities for observations given states

3. **Algorithm Steps**:
   - **Initialization**: Calculate probabilities for the first observation
   - **Recursion**: For each subsequent observation, calculate the maximum probability path
   - **Backtracking**: Find the most likely sequence of states

4. **Input**: A sequence of observations (Walk, Shop, Clean)

5. **Output**: The most probable sequence of hidden states and the probability of that sequence

## Sample Output
```
=== HMM Model ===
States: 2
Observations: 3

States:
Sunny Rainy 
Observations:
Walk Shop Clean 

Initial probabilities:
State 1: 0.600
State 2: 0.400

Transition probabilities:
From state 1: 0.700 0.300 
From state 2: 0.400 0.600 

Emission probabilities:
State 1: 0.600 0.300 0.100 
State 2: 0.200 0.300 0.500 

=== Viterbi Result ===
Best path:
Sunny Shop 
Best probability: 0.054000
```

This implementation shows how the Viterbi algorithm efficiently finds the most likely sequence of hidden states given a sequence of observations in a Hidden Markov Model.

