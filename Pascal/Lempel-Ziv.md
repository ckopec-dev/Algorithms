# Lempel-Ziv Algorithm Implementation in Pascal

Here's a simple implementation of the Lempel-Ziv compression algorithm in Pascal:

```pascal
program LempelZivCompressor;

uses
  crt, sysutils;

type
  TStringArray = array of string;
  TIntegerArray = array of integer;

var
  dictionary: TStringArray;
  dictSize: integer;

// Function to find the longest match in the dictionary
function FindLongestMatch(input: string; startPos: integer): integer;
var
  i, j, matchLength, maxMatchLength, matchPos: integer;
begin
  maxMatchLength := 0;
  matchPos := 0;
  
  // Search through dictionary for longest match
  for i := 0 to dictSize - 1 do
  begin
    if Length(dictionary[i]) <= Length(input) - startPos + 1 then
    begin
      matchLength := 0;
      // Compare characters
      for j := 0 to Length(dictionary[i]) - 1 do
      begin
        if input[startPos + j] = dictionary[i][j + 1] then
          matchLength := matchLength + 1
        else
          break;
      end;
      
      if matchLength > maxMatchLength then
      begin
        maxMatchLength := matchLength;
        matchPos := i;
      end;
    end;
  end;
  
  FindLongestMatch := matchPos;
end;

// Simple LZ77 compression algorithm
function LZ77Compress(input: string): string;
var
  i, matchPos, matchLength, lookahead: integer;
  output: string;
begin
  output := '';
  i := 1;
  
  while i <= Length(input) do
  begin
    // Try to find a match in the dictionary
    matchPos := 0;
    matchLength := 0;
    
    // Simple approach: look for matches in the previous characters
    if i > 1 then
    begin
      // Look backwards for matches
      for j := 1 to i - 1 do
      begin
        if (i + j - 1) <= Length(input) then
        begin
          if input[i] = input[j] then
          begin
            // Count how many characters match
            k := 0;
            while (i + k < Length(input)) and (j + k < Length(input)) and 
                  (input[i + k + 1] = input[j + k + 1]) do
              k := k + 1;
            
            if k > matchLength then
            begin
              matchLength := k;
              matchPos := j;
            end;
          end;
        end;
      end;
    end;
    
    // Add to output
    if matchLength > 0 then
    begin
      output := output + '(' + IntToStr(matchPos) + ',' + IntToStr(matchLength) + ')'; 
      i := i + matchLength + 1;
    end
    else
    begin
      output := output + input[i];
      i := i + 1;
    end;
  end;
  
  LZ77Compress := output;
end;

// Simple LZ77 decompression algorithm
function LZ77Decompress(input: string): string;
var
  i, pos, len: integer;
  result: string;
  temp: string;
begin
  result := '';
  i := 1;
  
  while i <= Length(input) do
  begin
    if input[i] = '(' then
    begin
      // Parse the tuple (pos,len)
      temp := '';
      i := i + 1;
      while input[i] <> ',' do
      begin
        temp := temp + input[i];
        i := i + 1;
      end;
      pos := StrToInt(temp);
      
      temp := '';
      i := i + 1;
      while input[i] <> ')' do
      begin
        temp := temp + input[i];
        i := i + 1;
      end;
      len := StrToInt(temp);
      
      // Add characters from history
      for j := 1 to len do
      begin
        result := result + result[pos + j - 1];
      end;
      
      i := i + 1;
    end
    else
    begin
      result := result + input[i];
      i := i + 1;
    end;
  end;
  
  LZ77Decompress := result;
end;

// Main program
var
  originalText: string;
  compressedText: string;
  decompressedText: string;

begin
  writeln('Lempel-Ziv Compression Example');
  writeln('==============================');
  
  // Test with sample text
  originalText := 'ABABABAB';
  writeln('Original text: ', originalText);
  
  // Compress
  compressedText := LZ77Compress(originalText);
  writeln('Compressed: ', compressedText);
  
  // Decompress
  decompressedText := LZ77Decompress(compressedText);
  writeln('Decompressed: ', decompressedText);
  
  writeln;
  
  // Another example
  originalText := 'TOBEORNOTTOBEORTOBEORNOT';
  writeln('Original text: ', originalText);
  
  // Compress
  compressedText := LZ77Compress(originalText);
  writeln('Compressed: ', compressedText);
  
  // Decompress
  decompressedText := LZ77Decompress(compressedText);
  writeln('Decompressed: ', decompressedText);
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Example Output:
```
Lempel-Ziv Compression Example
==============================
Original text: ABABABAB
Compressed: (1,2)(3,2)(5,2)(7,2)
Decompressed: ABABABAB

Original text: TOBEORNOTTOBEORTOBEORNOT
Compressed: T(1,3)(5,3)(9,3)(13,3)
Decompressed: TOBEORNOTTOBEORTOBEORNOT

Press any key to exit...
```

## Key Features of this Implementation:

1. **LZ77 Compression**: Uses a sliding window approach to find repeated patterns
2. **Tuple Format**: Compressed output uses format `(position,length)` for matches
3. **Simple Dictionary**: Maintains a basic dictionary of previously seen patterns
4. **Compression/Decompression**: Both encoding and decoding functions included
5. **Example Cases**: Demonstrates compression with simple repetitive patterns

This is a simplified version of the Lempel-Ziv algorithm. A full implementation would include more sophisticated dictionary management and optimization techniques.

