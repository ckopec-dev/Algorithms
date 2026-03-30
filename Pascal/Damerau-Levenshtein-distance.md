# Damerau-Levenshtein Distance in Pascal

Here's a complete implementation of the Damerau-Levenshtein distance algorithm in Pascal:

```pascal
program DamerauLevenshteinDistance;

{$APPTYPE CONSOLE}

uses
  SysUtils;

function DamerauLevenshteinDistance(s1, s2: string): integer;
var
  i, j, cost: integer;
  d: array of array of integer;
  len1, len2: integer;
begin
  len1 := Length(s1);
  len2 := Length(s2);
  
  // Handle empty strings
  if len1 = 0 then
  begin
    Result := len2;
    Exit;
  end;
  
  if len2 = 0 then
  begin
    Result := len1;
    Exit;
  end;
  
  // Create the distance matrix
  SetLength(d, len1 + 1, len2 + 1);
  
  // Initialize the first row and column
  for i := 0 to len1 do
    d[i, 0] := i;
  
  for j := 0 to len2 do
    d[0, j] := j;
  
  // Fill the distance matrix
  for i := 1 to len1 do
  begin
    for j := 1 to len2 do
    begin
      // Calculate cost (0 if characters are equal, 1 otherwise)
      if s1[i] = s2[j] then
        cost := 0
      else
        cost := 1;
      
      // Calculate minimum of three operations
      d[i, j] := Min(
        Min(d[i-1, j] + 1,        // deletion
            d[i, j-1] + 1),       // insertion
        d[i-1, j-1] + cost);     // substitution
      
      // Check for transposition (Damerau-Levenshtein specific)
      if (i > 1) and (j > 1) and (s1[i] = s2[j-1]) and (s1[i-1] = s2[j]) then
      begin
        d[i, j] := Min(d[i, j], d[i-2, j-2] + 1);
      end;
    end;
  end;
  
  Result := d[len1, len2];
end;

procedure TestDistance(s1, s2: string);
begin
  Writeln('String 1: "', s1, '"');
  Writeln('String 2: "', s2, '"');
  Writeln('Damerau-Levenshtein Distance: ', DamerauLevenshteinDistance(s1, s2));
  Writeln('----------------------------------------');
end;

begin
  Writeln('Damerau-Levenshtein Distance Calculator');
  Writeln('======================================');
  Writeln;
  
  // Test cases
  TestDistance('kitten', 'sitting');
  TestDistance('saturday', 'sunday');
  TestDistance('hello', 'hallo');
  TestDistance('abc', 'acb');
  TestDistance('abc', 'abcd');
  TestDistance('', 'test');
  TestDistance('same', 'same');
  
  Writeln('Press Enter to exit...');
  Readln;
end.
```

## How it works:

1. **Initialization**: Creates a 2D matrix `d` where `d[i,j]` represents the distance between the first `i` characters of string 1 and the first `j` characters of string 2.

2. **Base cases**: 
   - First row: distance from empty string to prefix of string 2 (insertions)
   - First column: distance from prefix of string 1 to empty string (deletions)

3. **Core algorithm**: For each cell `d[i,j]`, calculates the minimum cost of:
   - Deletion: `d[i-1,j] + 1`
   - Insertion: `d[i,j-1] + 1`  
   - Substitution: `d[i-1,j-1] + cost` (where cost is 0 if characters match, 1 otherwise)

4. **Damerau-Levenshtein addition**: Checks for transpositions (swapping adjacent characters) and considers them as a single operation.

## Sample Output:
```
Damerau-Levenshtein Distance Calculator
======================================

String 1: "kitten"
String 2: "sitting"
Damerau-Levenshtein Distance: 3
----------------------------------------
String 1: "saturday"
String 2: "sunday"
Damerau-Levenshtein Distance: 3
----------------------------------------
String 1: "hello"
String 2: "hallo"
Damerau-Levenshtein Distance: 1
----------------------------------------
```

The algorithm correctly handles all four basic operations (insertion, deletion, substitution, and transposition) with a time complexity of O(m×n) where m and n are the lengths of the input strings.

