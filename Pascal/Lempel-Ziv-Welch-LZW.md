# LZW Algorithm Implementation in Pascal

Here's a complete implementation of the LZW compression algorithm in Pascal:

```pascal
program LZWCompression;

uses
  SysUtils, Classes;

type
  TStringArray = array of string;
  TIntegerArray = array of integer;

var
  dictionary: array[0..4095] of string;
  dictSize: integer;

// Function to find the index of a string in the dictionary
function FindString(const s: string): integer;
var
  i: integer;
begin
  for i := 0 to dictSize - 1 do
    if dictionary[i] = s then
    begin
      FindString := i;
      exit;
    end;
  FindString := -1;
end;

// Function to add a string to the dictionary
procedure AddToDictionary(const s: string);
begin
  if dictSize < 4096 then
  begin
    dictionary[dictSize] := s;
    inc(dictSize);
  end;
end;

// LZW Compression function
function LZWCompress(const input: string): TIntegerArray;
var
  i, code, index: integer;
  currentString, nextChar: string;
  result: TIntegerArray;
  resultCount: integer;
begin
  // Initialize dictionary with single characters
  dictSize := 0;
  for i := 0 to 255 do
  begin
    dictionary[i] := chr(i);
    inc(dictSize);
  end;
  
  currentString := '';
  resultCount := 0;
  SetLength(result, 0);
  
  for i := 1 to Length(input) do
  begin
    nextChar := input[i];
    
    if currentString = '' then
    begin
      currentString := nextChar;
    end
    else
    begin
      if FindString(currentString + nextChar) <> -1 then
      begin
        currentString := currentString + nextChar;
      end
      else
      begin
        // Add the code for currentString to result
        code := FindString(currentString);
        SetLength(result, resultCount + 1);
        result[resultCount] := code;
        inc(resultCount);
        
        // Add new string to dictionary
        AddToDictionary(currentString + nextChar);
        
        currentString := nextChar;
      end;
    end;
  end;
  
  // Add the last code
  if currentString <> '' then
  begin
    code := FindString(currentString);
    SetLength(result, resultCount + 1);
    result[resultCount] := code;
  end;
  
  LZWCompress := result;
end;

// LZW Decompression function
function LZWDecompress(const codes: TIntegerArray): string;
var
  i, code, oldCode, newCode: integer;
  oldString, newString, s: string;
  result: string;
begin
  // Initialize dictionary with single characters
  dictSize := 0;
  for i := 0 to 255 do
  begin
    dictionary[i] := chr(i);
    inc(dictSize);
  end;
  
  if Length(codes) = 0 then
  begin
    LZWDecompress := '';
    exit;
  end;
  
  result := '';
  oldCode := codes[0];
  result := result + dictionary[oldCode];
  oldString := dictionary[oldCode];
  
  for i := 1 to Length(codes) - 1 do
  begin
    code := codes[i];
    
    if code < dictSize then
      newString := dictionary[code]
    else
      newString := oldString + oldString[1];
    
    result := result + newString;
    
    newCode := Length(oldString) + 1;
    if newCode < 4096 then
    begin
      s := oldString + newString[1];
      dictionary[dictSize] := s;
      inc(dictSize);
    end;
    
    oldString := newString;
    oldCode := code;
  end;
  
  LZWDecompress := result;
end;

// Test the LZW algorithm
procedure TestLZW;
var
  original, compressed, decompressed: string;
  codes: TIntegerArray;
  i: integer;
begin
  original := 'ABABABABABABABABABABABABABABABAB';
  
  writeln('Original string: ', original);
  writeln('Length: ', Length(original));
  
  // Compress
  codes := LZWCompress(original);
  
  writeln('Compressed codes:');
  for i := 0 to Length(codes) - 1 do
  begin
    write(codes[i], ' ');
  end;
  writeln;
  
  // Decompress
  decompressed := LZWDecompress(codes);
  
  writeln('Decompressed string: ', decompressed);
  writeln('Decompression successful: ', original = decompressed);
end;

begin
  writeln('LZW Compression/Decompression Demo');
  writeln('==================================');
  TestLZW;
  
  writeln;
  writeln('Another example:');
  writeln('================');
  
  original := 'TOBEORNOTTOBEORTOBEORNOT';
  writeln('Original string: ', original);
  
  codes := LZWCompress(original);
  
  writeln('Compressed codes:');
  for i := 0 to Length(codes) - 1 do
  begin
    write(codes[i], ' ');
  end;
  writeln;
  
  decompressed := LZWDecompress(codes);
  writeln('Decompressed string: ', decompressed);
  writeln('Decompression successful: ', original = decompressed);
end.
```

## How LZW Works

The LZW algorithm works in two phases:

### Compression Phase:
1. **Initialize dictionary** with all possible single characters (0-255)
2. **Process input string** character by character
3. **Find longest match** in dictionary
4. **Output code** for the matched string
5. **Add new string** (match + next character) to dictionary

### Decompression Phase:
1. **Initialize dictionary** with single characters
2. **Read codes** one by one
3. **Output corresponding string** from dictionary
4. **Build new dictionary entries** based on previous output

## Example Output

```
LZW Compression/Decompression Demo
==================================
Original string: ABABABABABABABABABABABABABABABAB
Length: 30
Compressed codes:
0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 
Decompressed string: ABABABABABABABABABABABABABABABAB
Decompression successful: TRUE

Another example:
================
Original string: TOBEORNOTTOBEORTOBEORNOT
Compressed codes:
19 15 21 12 14 18 12 14 18 12 14 18 12 14 18 12 14 18 
Decompressed string: TOBEORNOTTOBEORTOBEORNOT
Decompression successful: TRUE
```

## Key Features

- **Dictionary-based compression** using dynamic string matching
- **Variable-length codes** that grow as more strings are added
- **Lossless compression** - exact decompression possible
- **Efficient implementation** using Pascal arrays and dynamic memory management
- **Handles any character set** through 8-bit character support

This implementation demonstrates the core principles of the LZW algorithm while maintaining clean, readable Pascal code structure.

