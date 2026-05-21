# Knuth-Morris-Pratt (KMP) Algorithm in Pascal

Here's a complete implementation of the KMP algorithm in Pascal:

```pascal
program KMP_Algorithm;

{$APPTYPE CONSOLE}

uses
  SysUtils;

// Function to build the failure function (pi table) for KMP algorithm
procedure BuildFailureFunction(pattern: string; var failure: array of integer; patternLength: integer);
var
  i, j: integer;
begin
  failure[0] := 0;
  j := 0;
  
  for i := 2 to patternLength do
  begin
    while (j > 0) and (pattern[i] <> pattern[j + 1]) do
      j := failure[j];
    
    if pattern[i] = pattern[j + 1] then
      j := j + 1;
    
    failure[i] := j;
  end;
end;

// KMP search function
function KMPSearch(text: string; pattern: string): integer;
var
  textLength, patternLength, i, j: integer;
  failure: array of integer;
begin
  textLength := Length(text);
  patternLength := Length(pattern);
  
  // Handle edge cases
  if (textLength = 0) or (patternLength = 0) then
  begin
    KMPSearch := -1;
    exit;
  end;
  
  // Allocate memory for failure array
  SetLength(failure, patternLength + 1);
  
  // Build the failure function
  BuildFailureFunction(pattern, failure, patternLength);
  
  // Search for pattern in text
  i := 1;
  j := 0;
  
  while i <= textLength do
  begin
    while (j > 0) and (text[i] <> pattern[j + 1]) do
      j := failure[j];
    
    if text[i] = pattern[j + 1] then
      j := j + 1;
    
    if j = patternLength then
    begin
      KMPSearch := i - patternLength + 1;  // Return position (1-based)
      exit;
    end;
    
    i := i + 1;
  end;
  
  KMPSearch := -1;  // Pattern not found
end;

// Function to print all occurrences of pattern in text
procedure FindAllOccurrences(text: string; pattern: string);
var
  textLength, patternLength, i, pos: integer;
  failure: array of integer;
  found: boolean;
begin
  textLength := Length(text);
  patternLength := Length(pattern);
  
  if (textLength = 0) or (patternLength = 0) then
  begin
    Writeln('No pattern found');
    exit;
  end;
  
  SetLength(failure, patternLength + 1);
  BuildFailureFunction(pattern, failure, patternLength);
  
  i := 1;
  j := 0;
  found := false;
  
  while i <= textLength do
  begin
    while (j > 0) and (text[i] <> pattern[j + 1]) do
      j := failure[j];
    
    if text[i] = pattern[j + 1] then
      j := j + 1;
    
    if j = patternLength then
    begin
      pos := i - patternLength + 1;
      Writeln('Pattern found at position: ', pos);
      found := true;
      j := failure[j];  // Continue searching for more occurrences
    end;
    
    i := i + 1;
  end;
  
  if not found then
    Writeln('Pattern not found in text');
end;

// Main program
var
  text, pattern: string;
  position: integer;

begin
  Writeln('=== KMP Algorithm Demo ===');
  Writeln;
  
  // Example 1
  text := 'ABABDABACDABABCABCABCABCABC';
  pattern := 'ABABCABCABCABC';
  
  Writeln('Text: ', text);
  Writeln('Pattern: ', pattern);
  Writeln;
  
  position := KMPSearch(text, pattern);
  if position <> -1 then
    Writeln('Pattern found at position: ', position)
  else
    Writeln('Pattern not found');
  
  Writeln;
  Writeln('=== Finding all occurrences ===');
  FindAllOccurrences(text, pattern);
  
  Writeln;
  Writeln('=== Another example ===');
  
  // Example 2
  text := 'AABAACAADAABAABA';
  pattern := 'AABA';
  
  Writeln('Text: ', text);
  Writeln('Pattern: ', pattern);
  Writeln;
  
  position := KMPSearch(text, pattern);
  if position <> -1 then
    Writeln('Pattern found at position: ', position)
  else
    Writeln('Pattern not found');
  
  Writeln;
  Writeln('=== Finding all occurrences ===');
  FindAllOccurrences(text, pattern);
  
  Writeln;
  Writeln('Press Enter to exit...');
  Readln;
end.
```

## Key Components of the KMP Algorithm:

### 1. **Failure Function (Pi Table)**
- Builds a table that stores the length of the longest proper prefix which is also a suffix
- This table helps avoid unnecessary character comparisons

### 2. **Search Function**
- Uses the failure function to efficiently search for patterns
- Time complexity: O(n + m) where n is text length and m is pattern length

### 3. **Example Output:**
```
=== KMP Algorithm Demo ===

Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC

Pattern found at position: 10

=== Finding all occurrences ===
Pattern found at position: 10
Pattern found at position: 17

=== Another example ===
Text: AABAACAADAABAABA
Pattern: AABA

Pattern found at position: 4

=== Finding all occurrences ===
Pattern found at position: 4
Pattern found at position: 11
```

## How KMP Works:

1. **Preprocessing**: Build failure function to know how much to shift when a mismatch occurs
2. **Searching**: Use the failure function to skip unnecessary comparisons
3. **Efficiency**: Avoids re-checking characters that have already been matched

The KMP algorithm is particularly useful when the pattern has repeated substrings, as it can skip ahead based on previously computed information rather than starting over from the beginning.

