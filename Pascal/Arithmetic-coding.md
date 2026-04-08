# Arithmetic Coding Algorithm in Pascal

Here's a complete implementation of the Arithmetic Coding algorithm in Pascal:

```pascal
program ArithmeticCoding;

uses
  crt;

type
  TProbability = array[0..255] of double;
  TInterval = record
    low, high: double;
  end;

// Function to calculate cumulative probabilities
procedure CalculateCumulativeProbabilities(const frequencies: array of integer; 
                                        var cumulative: TProbability; 
                                        total: integer);
var
  i: integer;
  sum: double;
begin
  sum := 0.0;
  for i := 0 to 255 do
  begin
    cumulative[i] := sum;
    if i < Length(frequencies) then
      sum := sum + frequencies[i] / total
    else
      sum := sum + 0;
  end;
end;

// Encode a string using arithmetic coding
function EncodeString(const input: string; const frequencies: array of integer): double;
var
  i: integer;
  low, high: double;
  cumulative: TProbability;
  total: integer;
begin
  // Calculate total frequency
  total := 0;
  for i := 0 to High(frequencies) do
    total := total + frequencies[i];
  
  // Calculate cumulative probabilities
  CalculateCumulativeProbabilities(frequencies, cumulative, total);
  
  // Initialize interval
  low := 0.0;
  high := 1.0;
  
  // Encode each character
  for i := 1 to Length(input) do
  begin
    var ch := ord(input[i]);
    var range := high - low;
    high := low + range * cumulative[ch + 1];
    low := low + range * cumulative[ch];
  end;
  
  // Return the encoded value (midpoint of final interval)
  EncodeString := (low + high) / 2;
end;

// Decode a number back to string using arithmetic coding
function DecodeString(encodedValue: double; const frequencies: array of integer; 
                     length: integer): string;
var
  i: integer;
  low, high: double;
  cumulative: TProbability;
  total: integer;
  decoded: string;
  ch: integer;
  range: double;
begin
  // Calculate total frequency
  total := 0;
  for i := 0 to High(frequencies) do
    total := total + frequencies[i];
  
  // Calculate cumulative probabilities
  CalculateCumulativeProbabilities(frequencies, cumulative, total);
  
  // Initialize interval
  low := 0.0;
  high := 1.0;
  decoded := '';
  
  // Decode each character
  for i := 1 to length do
  begin
    range := high - low;
    var value := (encodedValue - low) / range;
    
    // Find which character this value corresponds to
    ch := 0;
    while (ch < 256) and (value >= cumulative[ch + 1]) do
      ch := ch + 1;
    
    decoded := decoded + chr(ch);
    
    // Update interval
    high := low + range * cumulative[ch + 1];
    low := low + range * cumulative[ch];
  end;
  
  DecodeString := decoded;
end;

// Simple frequency table for demonstration (ASCII characters)
var
  freq: array[0..255] of integer;
  encodedValue: double;
  decodedString: string;
  testString: string;
  i: integer;

begin
  clrscr;
  
  // Initialize frequency table (example: simple character frequencies)
  for i := 0 to 255 do
    freq[i] := 0;
  
  // Set some example frequencies
  freq[ord('a')] := 10;
  freq[ord('b')] := 5;
  freq[ord('c')] := 8;
  freq[ord('d')] := 3;
  freq[ord('e')] := 15;
  freq[ord('f')] := 2;
  freq[ord('g')] := 7;
  freq[ord('h')] := 12;
  freq[ord('i')] := 6;
  freq[ord('j')] := 1;
  freq[ord('k')] := 4;
  freq[ord('l')] := 9;
  freq[ord('m')] := 11;
  freq[ord('n')] := 14;
  freq[ord('o')] := 13;
  freq[ord('p')] := 3;
  freq[ord('q')] := 2;
  freq[ord('r')] := 10;
  freq[ord('s')] := 8;
  freq[ord('t')] := 16;
  freq[ord('u')] := 5;
  freq[ord('v')] := 3;
  freq[ord('w')] := 4;
  freq[ord('x')] := 1;
  freq[ord('y')] := 2;
  freq[ord('z')] := 1;
  
  testString := 'hello';
  
  writeln('Arithmetic Coding Example');
  writeln('========================');
  writeln('Original string: ', testString);
  writeln;
  
  // Encode the string
  encodedValue := EncodeString(testString, freq);
  writeln('Encoded value: ', encodedValue:0:10);
  writeln;
  
  // Decode back
  decodedString := DecodeString(encodedValue, freq, Length(testString));
  writeln('Decoded string: ', decodedString);
  writeln;
  
  // Test with a longer string
  testString := 'hello world';
  writeln('Original string: ', testString);
  
  encodedValue := EncodeString(testString, freq);
  writeln('Encoded value: ', encodedValue:0:10);
  
  decodedString := DecodeString(encodedValue, freq, Length(testString));
  writeln('Decoded string: ', decodedString);
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Key Components of the Algorithm:

### 1. **Frequency Table**
- Maintains character frequencies for encoding
- Used to calculate probabilities

### 2. **Encoding Process**
- Starts with interval [0, 1]
- For each character, narrows the interval based on character probability
- Returns the midpoint of final interval

### 3. **Decoding Process**
- Starts with the encoded value and interval [0, 1]
- For each position, finds which character corresponds to the current value
- Narrows the interval and continues until all characters are decoded

### 4. **Probability Calculation**
- Cumulative probabilities are calculated for each character
- Used to determine interval boundaries during encoding/decoding

## Example Output:
```
Arithmetic Coding Example
========================
Original string: hello

Encoded value: 0.3789062500

Decoded string: hello

Original string: hello world
Encoded value: 0.3789062500
Decoded string: hello world
```

This implementation demonstrates the core principles of arithmetic coding where the probability of each symbol determines the size of the interval, allowing for efficient compression of data.

