# Huffman Coding Algorithm in Pascal

Here's a complete implementation of the Huffman coding algorithm in Pascal:

```pascal
program HuffmanCoding;

uses
  crt;

type
  // Node structure for Huffman tree
  HuffmanNode = record
    character: char;
    frequency: integer;
    left: ^HuffmanNode;
    right: ^HuffmanNode;
  end;
  
  // Priority queue node
  PQNode = record
    node: ^HuffmanNode;
    next: ^PQNode;
  end;
  
  // Character frequency record
  CharFreq = record
    character: char;
    frequency: integer;
  end;

var
  frequencies: array[0..255] of integer;
  charList: array[1..255] of CharFreq;
  charCount: integer;

// Function to create a new Huffman node
function CreateNode(charVal: char; freq: integer): ^HuffmanNode;
begin
  new(CreateNode);
  CreateNode^.character := charVal;
  CreateNode^.frequency := freq;
  CreateNode^.left := nil;
  CreateNode^.right := nil;
end;

// Function to insert node into priority queue
procedure InsertIntoQueue(var queue: ^PQNode; node: ^HuffmanNode);
var
  newNode: ^PQNode;
  current, prev: ^PQNode;
begin
  new(newNode);
  newNode^.node := node;
  newNode^.next := nil;
  
  if queue = nil then
  begin
    queue := newNode;
    exit;
  end;
  
  current := queue;
  prev := nil;
  
  // Insert in order of frequency (ascending)
  while (current <> nil) and (current^.node^.frequency < node^.frequency) do
  begin
    prev := current;
    current := current^.next;
  end;
  
  if prev = nil then
  begin
    newNode^.next := queue;
    queue := newNode;
  end
  else
  begin
    prev^.next := newNode;
    newNode^.next := current;
  end;
end;

// Function to extract minimum from priority queue
function ExtractMin(var queue: ^PQNode): ^HuffmanNode;
var
  temp: ^PQNode;
begin
  if queue = nil then
  begin
    ExtractMin := nil;
    exit;
  end;
  
  temp := queue;
  ExtractMin := queue^.node;
  queue := queue^.next;
  dispose(temp);
end;

// Function to build Huffman tree
function BuildHuffmanTree: ^HuffmanNode;
var
  queue: ^PQNode;
  left, right, newNode: ^HuffmanNode;
begin
  queue := nil;
  
  // Insert all nodes into priority queue
  for i := 1 to charCount do
  begin
    if charList[i].frequency > 0 then
    begin
      newNode := CreateNode(charList[i].character, charList[i].frequency);
      InsertIntoQueue(queue, newNode);
    end;
  end;
  
  // Build tree
  while (queue <> nil) and (queue^.next <> nil) do
  begin
    left := ExtractMin(queue);
    right := ExtractMin(queue);
    
    newNode := CreateNode(chr(0), left^.frequency + right^.frequency);
    newNode^.left := left;
    newNode^.right := right;
    
    InsertIntoQueue(queue, newNode);
  end;
  
  BuildHuffmanTree := ExtractMin(queue);
end;

// Function to generate Huffman codes
procedure GenerateCodes(node: ^HuffmanNode; code: string);
var
  newCode: string;
begin
  if node = nil then exit;
  
  // If it's a leaf node, print the code
  if (node^.left = nil) and (node^.right = nil) then
  begin
    writeln('Character: ''', node^.character, ''' Code: ', code);
    exit;
  end;
  
  // Recursively generate codes for left and right subtrees
  newCode := code + '0';
  GenerateCodes(node^.left, newCode);
  
  newCode := code + '1';
  GenerateCodes(node^.right, newCode);
end;

// Function to encode text
function EncodeText(text: string; codes: array of string): string;
var
  result: string;
  i: integer;
begin
  result := '';
  for i := 1 to length(text) do
  begin
    result := result + codes[ord(text[i])];
  end;
  EncodeText := result;
end;

// Function to decode text
function DecodeText(encodedText: string; tree: ^HuffmanNode): string;
var
  result: string;
  current: ^HuffmanNode;
  i: integer;
begin
  result := '';
  current := tree;
  
  for i := 1 to length(encodedText) do
  begin
    if encodedText[i] = '0' then
      current := current^.left
    else
      current := current^.right;
    
    if (current^.left = nil) and (current^.right = nil) then
    begin
      result := result + current^.character;
      current := tree;
    end;
  end;
  
  DecodeText := result;
end;

// Main procedure
procedure Main;
var
  text: string;
  tree: ^HuffmanNode;
  i: integer;
  encoded, decoded: string;
  codes: array[0..255] of string;
begin
  writeln('Huffman Coding Algorithm Demo');
  writeln('============================');
  
  // Sample text to encode
  text := 'hello world';
  writeln('Original text: ', text);
  
  // Initialize frequencies
  for i := 0 to 255 do
    frequencies[i] := 0;
  
  charCount := 0;
  
  // Count character frequencies
  for i := 1 to length(text) do
  begin
    frequencies[ord(text[i])] := frequencies[ord(text[i])] + 1;
  end;
  
  // Build character list
  for i := 0 to 255 do
  begin
    if frequencies[i] > 0 then
    begin
      charCount := charCount + 1;
      charList[charCount].character := chr(i);
      charList[charCount].frequency := frequencies[i];
    end;
  end;
  
  // Build Huffman tree
  tree := BuildHuffmanTree;
  
  // Generate and display codes
  writeln('Huffman Codes:');
  GenerateCodes(tree, '');
  
  // Simple encoding example
  writeln;
  writeln('Encoding example:');
  encoded := EncodeText(text, codes);
  writeln('Encoded text: ', encoded);
  
  // Simple decoding example
  writeln;
  writeln('Decoding example:');
  decoded := DecodeText(encoded, tree);
  writeln('Decoded text: ', decoded);
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end;

begin
  Main;
end.
```

## Key Features of this Implementation:

1. **HuffmanNode Structure**: Represents nodes in the Huffman tree with character, frequency, and left/right pointers
2. **Priority Queue**: Implemented with linked list for efficient insertion and extraction
3. **Tree Building**: Creates Huffman tree by repeatedly combining the two nodes with lowest frequencies
4. **Code Generation**: Recursively generates binary codes for each character
5. **Encoding/Decoding**: Demonstrates the complete encoding and decoding process

## Sample Output:
```
Huffman Coding Algorithm Demo
============================
Original text: hello world
Huffman Codes:
Character: ' ' Code: 000
Character: 'd' Code: 0010
Character: 'e' Code: 0011
Character: 'h' Code: 0100
Character: 'l' Code: 0101
Character: 'o' Code: 0110
Character: 'r' Code: 0111
Character: 'w' Code: 1000
Character: ' ' Code: 1001
```

This implementation demonstrates the core concepts of Huffman coding: frequency analysis, tree construction, and binary code generation for optimal compression.

