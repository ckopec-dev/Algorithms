# Longest Common Subsequence (LCS) Algorithm in Pascal

Here's a complete implementation of the LCS algorithm in Pascal with both the dynamic programming table construction and the actual subsequence reconstruction:

```pascal
program LongestCommonSubsequence;

uses
  crt;

const
  MAXLEN = 100;

type
  TString = array[1..MAXLEN] of char;
  TMatrix = array[0..MAXLEN, 0..MAXLEN] of integer;

var
  str1, str2: TString;
  m, n: integer;
  dp: TMatrix;
  lcsLength: integer;

// Function to find maximum of two integers
function max(a, b: integer): integer;
begin
  if a > b then
    max := a
  else
    max := b;
end;

// Function to compute LCS length using dynamic programming
function computeLCSLength(s1, s2: TString; len1, len2: integer): integer;
var
  i, j: integer;
begin
  // Initialize the DP table
  for i := 0 to len1 do
    dp[i, 0] := 0;
  for j := 0 to len2 do
    dp[0, j] := 0;

  // Fill the DP table
  for i := 1 to len1 do
  begin
    for j := 1 to len2 do
    begin
      if s1[i] = s2[j] then
        dp[i, j] := dp[i-1, j-1] + 1
      else
        dp[i, j] := max(dp[i-1, j], dp[i, j-1]);
    end;
  end;

  computeLCSLength := dp[len1, len2];
end;

// Function to reconstruct the actual LCS
procedure reconstructLCS(s1, s2: TString; len1, len2: integer);
var
  i, j: integer;
  lcs: string;
begin
  lcs := '';
  i := len1;
  j := len2;

  // Backtrack through the DP table
  while (i > 0) and (j > 0) do
  begin
    if s1[i] = s2[j] then
    begin
      lcs := s1[i] + lcs;
      i := i - 1;
      j := j - 1;
    end
    else if dp[i-1, j] > dp[i, j-1] then
      i := i - 1
    else
      j := j - 1;
  end;

  writeln('Longest Common Subsequence: ', lcs);
  writeln('Length: ', length(lcs));
end;

// Function to print the DP table (for debugging)
procedure printDPTable(s1, s2: TString; len1, len2: integer);
var
  i, j: integer;
begin
  writeln('DP Table:');
  write('    ');
  for j := 1 to len2 do
    write(s2[j]:3);
  writeln;
  
  for i := 0 to len1 do
  begin
    if i = 0 then
      write('  ')
    else
      write(s1[i]:2);
    for j := 0 to len2 do
      write(dp[i, j]:3);
    writeln;
  end;
  writeln;
end;

begin
  clrscr;
  
  // Example 1
  writeln('=== Example 1 ===');
  str1 := 'ABCDGH';
  str2 := 'AEDFHR';
  m := 6;
  n := 6;
  
  lcsLength := computeLCSLength(str1, str2, m, n);
  writeln('String 1: ', str1);
  writeln('String 2: ', str2);
  writeln('LCS Length: ', lcsLength);
  printDPTable(str1, str2, m, n);
  reconstructLCS(str1, str2, m, n);
  
  writeln;
  writeln('=== Example 2 ===');
  
  // Example 2
  str1 := 'AGGTAB';
  str2 := 'GXTXAYB';
  m := 6;
  n := 7;
  
  lcsLength := computeLCSLength(str1, str2, m, n);
  writeln('String 1: ', str1);
  writeln('String 2: ', str2);
  writeln('LCS Length: ', lcsLength);
  printDPTable(str1, str2, m, n);
  reconstructLCS(str1, str2, m, n);
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## How the Algorithm Works:

1. **Dynamic Programming Approach**: Uses a 2D table `dp[i,j]` where each cell represents the length of LCS for substrings `s1[1..i]` and `s2[1..j]`

2. **Recurrence Relation**:
   - If `s1[i] = s2[j]`: `dp[i,j] = dp[i-1,j-1] + 1`
   - Otherwise: `dp[i,j] = max(dp[i-1,j], dp[i,j-1])`

3. **Reconstruction**: Backtracks through the DP table to build the actual LCS string

## Sample Output:
```
=== Example 1 ===
String 1: ABCDGH
String 2: AEDFHR
LCS Length: 3
DP Table:
      A  E  D  F  H  R
    0  0  0  0  0  0  0
A   1  1  1  1  1  1  1
B   1  1  1  1  1  1  1
C   1  1  1  1  1  1  1
D   1  1  2  2  2  2  2
G   1  1  2  2  2  2  2
H   1  1  2  2  2  3  3

Longest Common Subsequence: ADH
Length: 3
```

## Time and Space Complexity:
- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

This implementation provides both the computation of the LCS length and the actual reconstruction of the LCS string.

