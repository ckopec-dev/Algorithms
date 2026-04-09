# Grover's Search Algorithm in Pascal

Here's an implementation of Grover's search algorithm in Pascal. This example demonstrates the quantum search algorithm that can find a specific item in an unsorted database quadratically faster than classical algorithms.

```pascal
program GroversSearch;

uses
  SysUtils, Math;

type
  TOracle = function(index: integer): boolean of object;
  TState = array of double;

var
  n: integer;  // Number of qubits
  N: integer;  // Size of search space (2^n)
  iterations: integer;
  oracle: TOracle;

// Oracle function - this represents the function that identifies the target
function TargetOracle(index: integer): boolean;
begin
  // Example: Find the target at index 5
  TargetOracle := (index = 5);
end;

// Initialize quantum state - all qubits in superposition
procedure InitializeState(var state: TState);
var
  i: integer;
begin
  SetLength(state, N);
  for i := 0 to N-1 do
    state[i] := 1.0 / sqrt(N);
end;

// Apply the oracle (black box function)
procedure ApplyOracle(var state: TState; oracle: TOracle);
var
  i: integer;
begin
  for i := 0 to N-1 do
  begin
    if oracle(i) then
      state[i] := -state[i];  // Flip the sign for target
  end;
end;

// Apply the diffusion operator (inversion about mean)
procedure ApplyDiffusion(var state: TState);
var
  i, j: integer;
  mean: double;
begin
  // Calculate the mean
  mean := 0;
  for i := 0 to N-1 do
    mean := mean + state[i];
  mean := mean / N;

  // Apply inversion about mean
  for i := 0 to N-1 do
    state[i] := 2 * mean - state[i];
end;

// Apply Grover iteration
procedure GroverIteration(var state: TState);
begin
  ApplyOracle(state, oracle);
  ApplyDiffusion(state);
end;

// Measure the quantum state (simplified - just return the index with highest amplitude)
function MeasureState(const state: TState): integer;
var
  i, maxIndex: integer;
  maxAmplitude: double;
begin
  maxAmplitude := -1;
  maxIndex := 0;
  
  for i := 0 to N-1 do
  begin
    if abs(state[i]) > maxAmplitude then
    begin
      maxAmplitude := abs(state[i]);
      maxIndex := i;
    end;
  end;
  
  MeasureState := maxIndex;
end;

// Main Grover's search algorithm
function GroversSearch(targetOracle: TOracle): integer;
var
  state: TState;
  i: integer;
begin
  // Initialize the quantum state
  InitializeState(state);
  
  // Calculate number of iterations (approximately π/4 * sqrt(N))
  iterations := Round(PI / 4 * sqrt(N));
  
  // Perform Grover iterations
  for i := 1 to iterations do
  begin
    GroverIteration(state);
  end;
  
  // Measure the final state
  GroversSearch := MeasureState(state);
end;

// Main program
begin
  n := 3;  // 3 qubits = 8 possible states (0 to 7)
  N := 1 shl n;  // 2^n
  
  writeln('Grover''s Search Algorithm');
  writeln('========================');
  writeln('Number of qubits: ', n);
  writeln('Search space size: ', N);
  writeln('Target index (hardcoded): 5');
  writeln;
  
  // Set the oracle function
  oracle := TargetOracle;
  
  // Run Grover's search
  var result := GroversSearch(oracle);
  
  writeln('Search result: Index ', result);
  writeln('Target index: 5');
  
  if result = 5 then
    writeln('SUCCESS: Target found!')
  else
    writeln('FAILURE: Target not found');
    
  writeln;
  writeln('Expected iterations: ', Round(PI / 4 * sqrt(N)));
  writeln('Actual iterations: ', iterations);
end.
```

## Explanation

This Pascal implementation of Grover's search algorithm includes:

1. **Quantum State Representation**: Uses an array of doubles to represent quantum amplitudes
2. **Oracle Function**: A black-box function that identifies the target item (in this case, index 5)
3. **Grover Iterations**: Alternates between oracle application and diffusion operator
4. **Measurement**: Determines the most probable result from the quantum state

## Key Components

- **InitializeState**: Creates an equal superposition of all possible states
- **ApplyOracle**: Marks the target state by flipping its amplitude sign
- **ApplyDiffusion**: Amplifies the amplitude of the target state
- **GroverIteration**: Single iteration combining oracle and diffusion
- **MeasureState**: Returns the index with highest probability amplitude

## Expected Output
```
Grover's Search Algorithm
========================
Number of qubits: 3
Search space size: 8
Target index (hardcoded): 5

Search result: Index 5
Target index: 5
SUCCESS: Target found!

Expected iterations: 2
Actual iterations: 2
```

This implementation demonstrates the core concepts of Grover's algorithm, though it's a simplified classical simulation rather than a true quantum computation.

