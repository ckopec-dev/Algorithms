# Subset Sum Algorithm in Pascal

Here's an implementation of the subset sum problem using dynamic programming in Pascal:

```pascal
program SubsetSum;

{$APPTYPE CONSOLE}

uses
  SysUtils;

function subsetSum(arr: array of integer; n, sum: integer): boolean;
var
  i, j: integer;
  dp: array of array of boolean;
begin
  // Create DP table
  SetLength(dp, n + 1, sum + 1);
  
  // Initialize base cases
  for i := 0 to n do
    dp[i][0] := true;  // Sum of 0 is always possible with empty subset
  
  for j := 1 to sum do
    dp[0][j] := false; // Sum > 0 with empty array is impossible
  
  // Fill the DP table
  for i := 1 to n do
  begin
    for j := 1 to sum do
    begin
      // If current element is greater than sum, skip it
      if arr[i-1] > j then
        dp[i][j] := dp[i-1][j]
      else
        // Either include or exclude the current element
        dp[i][j] := dp[i-1][j] or dp[i-1][j - arr[i-1]];
    end;
  end;
  
  subsetSum := dp[n][sum];
end;

procedure printSubset(arr: array of integer; n, sum: integer);
var
  i, j: integer;
  dp: array of array of boolean;
  result: array of integer;
  resultCount: integer;
begin
  SetLength(dp, n + 1, sum + 1);
  
  // Initialize base cases
  for i := 0 to n do
    dp[i][0] := true;
  
  for j := 1 to sum do
    dp[0][j] := false;
  
  // Fill the DP table
  for i := 1 to n do
  begin
    for j := 1 to sum do
    begin
      if arr[i-1] > j then
        dp[i][j] := dp[i-1][j]
      else
        dp[i][j] := dp[i-1][j] or dp[i-1][j - arr[i-1]];
    end;
  end;
  
  // If no subset found
  if not dp[n][sum] then
  begin
    writeln('No subset with sum ', sum, ' found');
    exit;
  end;
  
  // Backtrack to find the subset
  resultCount := 0;
  i := n;
  j := sum;
  
  while (i > 0) and (j > 0) do
  begin
    // If current element is not included in sum
    if dp[i-1][j] then
      i := i - 1
    else
    begin
      // Include this element in subset
      SetLength(result, resultCount + 1);
      result[resultCount] := arr[i-1];
      resultCount := resultCount + 1;
      j := j - arr[i-1];
      i := i - 1;
    end;
  end;
  
  // Print the subset
  write('Subset with sum ', sum, ': ');
  for i := resultCount - 1 downto 0 do
  begin
    write(result[i]);
    if i > 0 then write(' + ');
  end;
  writeln;
end;

var
  arr: array[0..4] of integer;
  n, sum: integer;
begin
  // Example usage
  arr[0] := 3;
  arr[1] := 34;
  arr[2] := 4;
  arr[3] := 12;
  arr[4] := 5;
  
  n := 5;
  sum := 9;
  
  writeln('Array: ', arr[0], ' ', arr[1], ' ', arr[2], ' ', arr[3], ' ', arr[4]);
  writeln('Target sum: ', sum);
  
  if subsetSum(arr, n, sum) then
  begin
    writeln('Subset with given sum exists');
    printSubset(arr, n, sum);
  end
  else
    writeln('No subset with given sum exists');
  
  writeln;
  
  // Test with another example
  sum := 30;
  writeln('Target sum: ', sum);
  
  if subsetSum(arr, n, sum) then
  begin
    writeln('Subset with given sum exists');
    printSubset(arr, n, sum);
  end
  else
    writeln('No subset with given sum exists');
  
  readln;
end.
```

## Output:
```
Array: 3 34 4 12 5
Target sum: 9
Subset with given sum exists
Subset with sum 9: 4 + 5

Target sum: 30
No subset with given sum exists
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: Uses a 2D boolean array `dp[i][j]` where `dp[i][j]` represents whether sum `j` can be obtained using first `i` elements.

2. **Base Cases**:
   - `dp[i][0] = true` (sum of 0 is always possible with empty subset)
   - `dp[0][j] = false` (sum > 0 with empty array is impossible)

3. **Recurrence Relation**:
   - If current element > target sum: `dp[i][j] = dp[i-1][j]`
   - Otherwise: `dp[i][j] = dp[i-1][j] or dp[i-1][j - arr[i-1]]`

4. **Time Complexity**: O(n × sum)
5. **Space Complexity**: O(n × sum)

The algorithm efficiently determines whether a subset exists that sums to the target value and can also reconstruct the actual subset when it exists.

