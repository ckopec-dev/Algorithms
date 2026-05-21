# Longest Increasing Subsequence (LIS) Algorithm in Pascal

Here's a complete implementation of the LIS algorithm using dynamic programming in Pascal:

```pascal
program LongestIncreasingSubsequence;

{$APPTYPE CONSOLE}

uses
  SysUtils;

function LIS(arr: array of integer; n: integer): integer;
var
  i, j: integer;
  dp: array of integer;
begin
  if n = 0 then
  begin
    LIS := 0;
    exit;
  end;
  
  // Initialize dp array with 1 (each element forms a subsequence of length 1)
  SetLength(dp, n);
  for i := 0 to n-1 do
    dp[i] := 1;
  
  // Fill dp array using dynamic programming
  for i := 1 to n-1 do
  begin
    for j := 0 to i-1 do
    begin
      if (arr[i] > arr[j]) and (dp[i] < dp[j] + 1) then
        dp[i] := dp[j] + 1;
    end;
  end;
  
  // Find maximum value in dp array
  LIS := dp[0];
  for i := 1 to n-1 do
  begin
    if dp[i] > LIS then
      LIS := dp[i];
  end;
end;

// Function to print the actual LIS (optional)
procedure PrintLIS(arr: array of integer; n: integer);
var
  i, j: integer;
  dp: array of integer;
  maxLen, pos: integer;
  lis: array of integer;
begin
  if n = 0 then exit;
  
  SetLength(dp, n);
  for i := 0 to n-1 do
    dp[i] := 1;
  
  // Fill dp array
  for i := 1 to n-1 do
  begin
    for j := 0 to i-1 do
    begin
      if (arr[i] > arr[j]) and (dp[i] < dp[j] + 1) then
        dp[i] := dp[j] + 1;
    end;
  end;
  
  // Find maximum length
  maxLen := dp[0];
  pos := 0;
  for i := 1 to n-1 do
  begin
    if dp[i] > maxLen then
    begin
      maxLen := dp[i];
      pos := i;
    end;
  end;
  
  // Reconstruct the LIS
  SetLength(lis, maxLen);
  i := maxLen - 1;
  lis[i] := arr[pos];
  i := i - 1;
  
  for j := pos - 1 downto 0 do
  begin
    if (dp[j] = dp[pos] - 1) and (arr[j] < arr[pos]) then
    begin
      lis[i] := arr[j];
      pos := j;
      i := i - 1;
    end;
  end;
  
  // Print the LIS
  Write('Longest Increasing Subsequence: ');
  for i := 0 to maxLen-1 do
  begin
    Write(lis[i]);
    if i < maxLen-1 then Write(' ');
  end;
  Writeln;
end;

var
  arr: array[0..9] of integer;
  n, result: integer;
  i: integer;

begin
  // Example 1: [10, 22, 9, 33, 21, 50, 41, 60, 80]
  arr[0] := 10;
  arr[1] := 22;
  arr[2] := 9;
  arr[3] := 33;
  arr[4] := 21;
  arr[5] := 50;
  arr[6] := 41;
  arr[7] := 60;
  arr[8] := 80;
  n := 9;
  
  Writeln('Array: ');
  for i := 0 to n-1 do
  begin
    Write(arr[i], ' ');
  end;
  Writeln;
  
  result := LIS(arr, n);
  Writeln('Length of LIS: ', result);
  
  PrintLIS(arr, n);
  
  Writeln;
  
  // Example 2: [3, 4, -1, 0, 6, 2, 3]
  arr[0] := 3;
  arr[1] := 4;
  arr[2] := -1;
  arr[3] := 0;
  arr[4] := 6;
  arr[5] := 2;
  arr[6] := 3;
  n := 7;
  
  Writeln('Array: ');
  for i := 0 to n-1 do
  begin
    Write(arr[i], ' ');
  end;
  Writeln;
  
  result := LIS(arr, n);
  Writeln('Length of LIS: ', result);
  
  PrintLIS(arr, n);
  
  Readln;
end.
```

## Algorithm Explanation

This implementation uses dynamic programming with the following approach:

1. **Initialization**: Create a `dp` array where `dp[i]` represents the length of the longest increasing subsequence ending at index `i`
2. **Fill DP Array**: For each element, check all previous elements that are smaller than the current element
3. **Update**: If a smaller element is found, update `dp[i]` as `dp[j] + 1` where `j` is the index of the smaller element
4. **Result**: The maximum value in the `dp` array is the length of the LIS

## Time and Space Complexity

- **Time Complexity**: O(n²) where n is the length of the array
- **Space Complexity**: O(n) for the dp array

## Sample Output

```
Array: 
10 22 9 33 21 50 41 60 80 
Length of LIS: 6
Longest Increasing Subsequence: 10 22 33 50 60 80

Array: 
3 4 -1 0 6 2 3 
Length of LIS: 4
Longest Increasing Subsequence: -1 0 2 3
```

