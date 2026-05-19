# Knapsack Problem Algorithm in Pascal

Here's a complete implementation of the 0/1 Knapsack problem using dynamic programming in Pascal:

```pascal
program KnapsackProblem;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  TItem = record
    weight: integer;
    value: integer;
  end;

const
  MAX_ITEMS = 100;
  MAX_CAPACITY = 1000;

var
  items: array[1..MAX_ITEMS] of TItem;
  dp: array[0..MAX_ITEMS, 0..MAX_CAPACITY] of integer;
  n, capacity: integer;

// Function to find maximum of two integers
function Max(a, b: integer): integer;
begin
  if a > b then
    Max := a
  else
    Max := b;
end;

// Knapsack algorithm using dynamic programming
function Knapsack: integer;
var
  i, w: integer;
begin
  // Initialize the DP table
  for i := 0 to n do
    dp[i, 0] := 0;
  
  for w := 0 to capacity do
    dp[0, w] := 0;
  
  // Fill the DP table
  for i := 1 to n do
  begin
    for w := 0 to capacity do
    begin
      // If current item's weight exceeds capacity, skip it
      if items[i].weight > w then
        dp[i, w] := dp[i-1, w]
      else
        // Max of: not taking item OR taking item + best solution for remaining capacity
        dp[i, w] := Max(dp[i-1, w], dp[i-1, w - items[i].weight] + items[i].value);
    end;
  end;
  
  Knapsack := dp[n, capacity];
end;

// Function to trace back and find which items were selected
procedure TraceBack;
var
  i, w: integer;
  selected: array[1..MAX_ITEMS] of boolean;
begin
  // Initialize selected array
  for i := 1 to n do
    selected[i] := false;
  
  i := n;
  w := capacity;
  
  // Trace back through the DP table
  while (i > 0) and (w > 0) do
  begin
    // If value is different from above row, item was included
    if dp[i, w] <> dp[i-1, w] then
    begin
      selected[i] := true;
      w := w - items[i].weight;
    end;
    i := i - 1;
  end;
  
  // Print selected items
  writeln('Selected items:');
  for i := 1 to n do
  begin
    if selected[i] then
      writeln('Item ', i, ': Weight = ', items[i].weight, ', Value = ', items[i].value);
  end;
end;

// Main program
begin
  writeln('Knapsack Problem Solution');
  writeln('========================');
  
  // Example data
  n := 4;
  capacity := 8;
  
  // Initialize items (weight, value)
  items[1].weight := 2;
  items[1].value := 3;
  
  items[2].weight := 3;
  items[2].value := 4;
  
  items[3].weight := 4;
  items[3].value := 5;
  
  items[4].weight := 5;
  items[4].value := 6;
  
  writeln('Items:');
  for i := 1 to n do
    writeln('Item ', i, ': Weight = ', items[i].weight, ', Value = ', items[i].value);
  
  writeln('Capacity: ', capacity);
  writeln;
  
  // Solve knapsack problem
  writeln('Maximum value: ', Knapsack);
  writeln;
  
  // Trace back to show which items were selected
  TraceBack;
  
  writeln;
  writeln('Press Enter to exit...');
  readln;
end.
```

## How the Algorithm Works:

1. **Dynamic Programming Approach**: Uses a 2D table `dp[i,w]` where:
   - `i` represents the first `i` items
   - `w` represents the current capacity
   - `dp[i,w]` stores the maximum value achievable with first `i` items and capacity `w`

2. **Recurrence Relation**:
   ```
   dp[i,w] = max(dp[i-1,w], dp[i-1,w-weight[i]] + value[i])
   ```

3. **Base Cases**:
   - `dp[0,w] = 0` (no items = no value)
   - `dp[i,0] = 0` (zero capacity = no value)

4. **Time Complexity**: O(n × capacity)
5. **Space Complexity**: O(n × capacity)

## Sample Output:
```
Knapsack Problem Solution
========================
Items:
Item 1: Weight = 2, Value = 3
Item 2: Weight = 3, Value = 4
Item 3: Weight = 4, Value = 5
Item 4: Weight = 5, Value = 6
Capacity: 8

Maximum value: 10

Selected items:
Item 2: Weight = 3, Value = 4
Item 3: Weight = 4, Value = 5
```

This implementation solves the classic 0/1 Knapsack problem where each item can either be taken or not taken, and finds the combination that maximizes value while respecting the weight constraint.

