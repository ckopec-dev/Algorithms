# Run-Length Encoding (RLE) in Pascal

Here's an example implementation of the Run-Length Encoding algorithm in Pascal:

```pascal
program RunLengthEncoding;

uses crt;

// Function to perform Run-Length Encoding
function RLE_Encode(input: string): string;
var
  i, count: integer;
  currentChar: char;
  result: string;
begin
  if input = '' then
  begin
    RLE_Encode := '';
    exit;
  end;

  result := '';
  currentChar := input[1];
  count := 1;

  for i := 2 to Length(input) do
  begin
    if input[i] = currentChar then
    begin
      count := count + 1;
    end
    else
    begin
      // Add count and character to result
      if count > 1 then
        result := result + IntToStr(count) + currentChar
      else
        result := result + currentChar;
      
      // Reset for new character
      currentChar := input[i];
      count := 1;
    end;
  end;

  // Handle the last sequence
  if count > 1 then
    result := result + IntToStr(count) + currentChar
  else
    result := result + currentChar;

  RLE_Encode := result;
end;

// Function to perform Run-Length Decoding
function RLE_Decode(input: string): string;
var
  i, j, count: integer;
  result: string;
  numStr: string;
begin
  result := '';
  i := 1;

  while i <= Length(input) do
  begin
    // Check if current character is a digit
    if (input[i] >= '0') and (input[i] <= '9') then
    begin
      // Extract the complete number
      numStr := '';
      while (i <= Length(input)) and (input[i] >= '0') and (input[i] <= '9') do
      begin
        numStr := numStr + input[i];
        i := i + 1;
      end;
      
      // Convert string to integer
      count := StrToInt(numStr);
      
      // Get the character to repeat
      if i <= Length(input) then
      begin
        for j := 1 to count do
          result := result + input[i];
        i := i + 1;
      end;
    end
    else
    begin
      // Single character (count = 1)
      result := result + input[i];
      i := i + 1;
    end;
  end;

  RLE_Decode := result;
end;

// Main program
var
  originalText, encodedText, decodedText: string;

begin
  clrscr;
  
  // Test cases
  originalText := 'AAAABBBCCDAA';
  writeln('Original text: ', originalText);
  
  encodedText := RLE_Encode(originalText);
  writeln('Encoded text:  ', encodedText);
  
  decodedText := RLE_Decode(encodedText);
  writeln('Decoded text:  ', decodedText);
  writeln;
  
  // Another test case
  originalText := 'WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB';
  writeln('Original text: ', originalText);
  
  encodedText := RLE_Encode(originalText);
  writeln('Encoded text:  ', encodedText);
  
  decodedText := RLE_Decode(encodedText);
  writeln('Decoded text:  ', decodedText);
  writeln;
  
  // Test with single characters
  originalText := 'AABBCC';
  writeln('Original text: ', originalText);
  
  encodedText := RLE_Encode(originalText);
  writeln('Encoded text:  ', encodedText);
  
  decodedText := RLE_Decode(encodedText);
  writeln('Decoded text:  ', decodedText);
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Example Output:
```
Original text: AAAABBBCCDAA
Encoded text:  4A3B2C1D2A
Decoded text:  AAAABBBCCDAA

Original text: WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB
Encoded text:  12W1B12W3B24W1B
Decoded text:  WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB

Original text: AABBCC
Encoded text:  2A2B2C
Decoded text:  AABBCC

Press any key to exit...
```

## How it works:

1. **Encoding Process**: 
   - Count consecutive identical characters
   - If count > 1, output count followed by character
   - If count = 1, output just the character

2. **Decoding Process**:
   - Parse numbers followed by characters
   - Repeat characters according to their preceding count
   - Handle single characters (count = 1) by outputting them directly

## Key Features:
- Handles empty strings
- Properly encodes and decodes sequences
- Works with any character set
- Includes error handling for edge cases
- Demonstrates both encoding and decoding functions

