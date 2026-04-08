# Vogel's Approximation Method in Pascal

Here's a complete implementation of Vogel's Approximation Method (VAM) for solving transportation problems in Pascal:

```pascal
program VogelsApproximationMethod;

uses crt;

const
  MAX_SIZE = 10;

type
  TMatrix = array[1..MAX_SIZE, 1..MAX_SIZE] of integer;
  TSupply = array[1..MAX_SIZE] of integer;
  TDemand = array[1..MAX_SIZE] of integer;

var
  cost: TMatrix;
  supply: TSupply;
  demand: TDemand;
  allocation: TMatrix;
  rowSupply: TSupply;
  colDemand: TDemand;
  rows, cols: integer;
  totalCost: integer;

// Function to calculate penalty for a row
function CalculateRowPenalty(r: integer): integer;
var
  min1, min2: integer;
  i: integer;
begin
  min1 := maxint;
  min2 := maxint;
  
  for i := 1 to cols do
  begin
    if (allocation[r, i] = 0) and (cost[r, i] < min1) then
    begin
      min2 := min1;
      min1 := cost[r, i];
    end
    else if (allocation[r, i] = 0) and (cost[r, i] < min2) then
      min2 := cost[r, i];
  end;
  
  if min1 = maxint then
    CalculateRowPenalty := 0
  else
    CalculateRowPenalty := min2 - min1;
end;

// Function to calculate penalty for a column
function CalculateColPenalty(c: integer): integer;
var
  min1, min2: integer;
  i: integer;
begin
  min1 := maxint;
  min2 := maxint;
  
  for i := 1 to rows do
  begin
    if (allocation[i, c] = 0) and (cost[i, c] < min1) then
    begin
      min2 := min1;
      min1 := cost[i, c];
    end
    else if (allocation[i, c] = 0) and (cost[i, c] < min2) then
      min2 := cost[i, c];
  end;
  
  if min1 = maxint then
    CalculateColPenalty := 0
  else
    CalculateColPenalty := min2 - min1;
end;

// Function to find maximum penalty
function FindMaxPenalty: integer;
var
  maxPenalty, rowPenalty, colPenalty: integer;
  i: integer;
  maxRow, maxCol: integer;
begin
  maxPenalty := -1;
  maxRow := 0;
  maxCol := 0;
  
  // Check row penalties
  for i := 1 to rows do
  begin
    if rowSupply[i] > 0 then
    begin
      rowPenalty := CalculateRowPenalty(i);
      if rowPenalty > maxPenalty then
      begin
        maxPenalty := rowPenalty;
        maxRow := i;
        maxCol := 0;
      end;
    end;
  end;
  
  // Check column penalties
  for i := 1 to cols do
  begin
    if colDemand[i] > 0 then
    begin
      colPenalty := CalculateColPenalty(i);
      if colPenalty > maxPenalty then
      begin
        maxPenalty := colPenalty;
        maxRow := 0;
        maxCol := i;
      end;
    end;
  end;
  
  FindMaxPenalty := maxPenalty;
end;

// Function to get the row with maximum penalty
function GetMaxRow: integer;
var
  maxPenalty, rowPenalty: integer;
  i, maxRow: integer;
begin
  maxPenalty := -1;
  maxRow := 0;
  
  for i := 1 to rows do
  begin
    if rowSupply[i] > 0 then
    begin
      rowPenalty := CalculateRowPenalty(i);
      if rowPenalty > maxPenalty then
      begin
        maxPenalty := rowPenalty;
        maxRow := i;
      end;
    end;
  end;
  
  GetMaxRow := maxRow;
end;

// Function to get the column with maximum penalty
function GetMaxCol: integer;
var
  maxPenalty, colPenalty: integer;
  i, maxCol: integer;
begin
  maxPenalty := -1;
  maxCol := 0;
  
  for i := 1 to cols do
  begin
    if colDemand[i] > 0 then
    begin
      colPenalty := CalculateColPenalty(i);
      if colPenalty > maxPenalty then
      begin
        maxPenalty := colPenalty;
        maxCol := i;
      end;
    end;
  end;
  
  GetMaxCol := maxCol;
end;

// Function to get minimum cost cell in a row
function GetMinCostInRow(row: integer): integer;
var
  minCost, minCol: integer;
  i: integer;
begin
  minCost := maxint;
  minCol := 0;
  
  for i := 1 to cols do
  begin
    if (allocation[row, i] = 0) and (cost[row, i] < minCost) then
    begin
      minCost := cost[row, i];
      minCol := i;
    end;
  end;
  
  GetMinCostInRow := minCol;
end;

// Function to get minimum cost cell in a column
function GetMinCostInCol(col: integer): integer;
var
  minCost, minRow: integer;
  i: integer;
begin
  minCost := maxint;
  minRow := 0;
  
  for i := 1 to rows do
  begin
    if (allocation[i, col] = 0) and (cost[i, col] < minCost) then
    begin
      minCost := cost[i, col];
      minRow := i;
    end;
  end;
  
  GetMinCostInCol := minRow;
end;

// Main VAM algorithm
procedure VogelsApproximationMethod;
var
  maxPenalty, maxRow, maxCol, minCol, minRow, allocationValue: integer;
  i, j: integer;
begin
  writeln('Vogel''s Approximation Method Algorithm');
  writeln('=====================================');
  
  // Initialize allocation matrix
  for i := 1 to rows do
    for j := 1 to cols do
      allocation[i, j] := 0;
  
  // Copy supply and demand
  for i := 1 to rows do
    rowSupply[i] := supply[i];
  
  for i := 1 to cols do
    colDemand[i] := demand[i];
  
  // Main loop
  while true do
  begin
    maxPenalty := FindMaxPenalty;
    
    // If no penalty found, break
    if maxPenalty = -1 then
      break;
    
    // Get row or column with maximum penalty
    maxRow := GetMaxRow;
    maxCol := GetMaxCol;
    
    // If maxRow has penalty, allocate in that row
    if maxRow > 0 then
    begin
      minCol := GetMinCostInRow(maxRow);
      allocationValue := min(rowSupply[maxRow], colDemand[minCol]);
      
      allocation[maxRow, minCol] := allocationValue;
      rowSupply[maxRow] := rowSupply[maxRow] - allocationValue;
      colDemand[minCol] := colDemand[minCol] - allocationValue;
      
      writeln('Allocated ', allocationValue, ' units from supply ', maxRow, ' to demand ', minCol);
    end
    else
    begin
      minRow := GetMinCostInCol(maxCol);
      allocationValue := min(rowSupply[minRow], colDemand[maxCol]);
      
      allocation[minRow, maxCol] := allocationValue;
      rowSupply[minRow] := rowSupply[minRow] - allocationValue;
      colDemand[maxCol] := colDemand[maxCol] - allocationValue;
      
      writeln('Allocated ', allocationValue, ' units from supply ', minRow, ' to demand ', maxCol);
    end;
  end;
  
  // Calculate total cost
  totalCost := 0;
  for i := 1 to rows do
    for j := 1 to cols do
      totalCost := totalCost + (allocation[i, j] * cost[i, j]);
  
  writeln('Total transportation cost: ', totalCost);
end;

// Display the transportation table
procedure DisplayTransportationTable;
var
  i, j: integer;
begin
  writeln;
  writeln('Transportation Table:');
  writeln('====================');
  
  write('    ');
  for j := 1 to cols do
    write('D', j:2, '    ');
  writeln('Supply');
  
  for i := 1 to rows do
  begin
    write('S', i:2, ' ');
    for j := 1 to cols do
      write(allocation[i, j]:4, ' ');
    writeln('|', supply[i]:4);
  end;
  
  write('Demand ');
  for j := 1 to cols do
    write(demand[j]:4, ' ');
  writeln;
end;

// Display cost matrix
procedure DisplayCostMatrix;
var
  i, j: integer;
begin
  writeln;
  writeln('Cost Matrix:');
  writeln('============');
  
  write('    ');
  for j := 1 to cols do
    write('D', j:2, ' ');
  writeln;
  
  for i := 1 to rows do
  begin
    write('S', i:2, ' ');
    for j := 1 to cols do
      write(cost[i, j]:4, ' ');
    writeln;
  end;
end;

// Input data
procedure InputData;
var
  i, j: integer;
begin
  writeln('Enter number of supply points (rows): ');
  readln(rows);
  writeln('Enter number of demand points (columns): ');
  readln(cols);
  
  writeln('Enter supply values:');
  for i := 1 to rows do
  begin
    write('Supply S', i, ': ');
    readln(supply[i]);
  end;
  
  writeln('Enter demand values:');
  for j := 1 to cols do
  begin
    write('Demand D', j, ': ');
    readln(demand[j]);
  end;
  
  writeln('Enter cost matrix:');
  for i := 1 to rows do
  begin
    for j := 1 to cols do
    begin
      write('Cost S', i, ' to D', j, ': ');
      readln(cost[i, j]);
    end;
  end;
end;

// Main program
begin
  clrscr;
  
  InputData;
  DisplayCostMatrix;
  VogelsApproximationMethod;
  DisplayTransportationTable;
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Example Usage

Here's an example of how to use this program with a sample transportation problem:

**Sample Input:**
```
Number of supply points: 3
Number of demand points: 4

Supply values:
S1: 300
S2: 400
S3: 500

Demand values:
D1: 250
D2: 350
D3: 400
D4: 200

Cost matrix:
S1 to D1: 8
S1 to D2: 6
S1 to D3: 10
S1 to D4: 9

S2 to D1: 9
S2 to D2: 12
S2 to D3: 13
S2 to D4: 7

S3 to D1: 14
S3 to D2: 9
S3 to D3: 16
S3 to D4: 5
```

## Key Features of the Implementation

1. **Penalty Calculation**: Computes row and column penalties for each unallocated cell
2. **Maximum Penalty Selection**: Identifies the highest penalty to prioritize allocation
3. **Minimum Cost Allocation**: Allocates to the cell with minimum cost in the selected row/column
4. **Supply/Demand Tracking**: Maintains current supply and demand values
5. **Cost Calculation**: Computes the total transportation cost
6. **Output Display**: Shows the transportation table and final cost

## Algorithm Steps

1. Calculate penalties for all rows and columns
2. Select the row or column with maximum penalty
3. Allocate to the minimum cost cell in that row/column
4. Update supply/demand values
5. Repeat until all allocations are made
6. Calculate total transportation cost

This implementation provides a complete working solution for solving transportation problems using Vogel's Approximation Method.

