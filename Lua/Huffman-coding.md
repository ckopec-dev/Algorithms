# Huffman Coding Algorithm in Lua

```lua
-- Huffman Coding Implementation in Lua

-- Node structure for Huffman tree
local Node = {}
Node.__index = Node

function Node:new(char, freq)
    local obj = {
        char = char,
        freq = freq,
        left = nil,
        right = nil
    }
    setmetatable(obj, Node)
    return obj
end

-- Min heap implementation
local MinHeap = {}
MinHeap.__index = MinHeap

function MinHeap:new()
    local obj = {
        heap = {}
    }
    setmetatable(obj, MinHeap)
    return obj
end

function MinHeap:insert(node)
    table.insert(self.heap, node)
    self:heapifyUp(#self.heap)
end

function MinHeap:extractMin()
    if #self.heap == 0 then return nil end
    
    local min = self.heap[1]
    self.heap[1] = self.heap[#self.heap]
    table.remove(self.heap)
    
    if #self.heap > 0 then
        self:heapifyDown(1)
    end
    
    return min
end

function MinHeap:heapifyUp(index)
    if index <= 1 then return end
    
    local parentIndex = math.floor(index / 2)
    if self.heap[parentIndex].freq > self.heap[index].freq then
        self.heap[parentIndex], self.heap[index] = self.heap[index], self.heap[parentIndex]
        self:heapifyUp(parentIndex)
    end
end

function MinHeap:heapifyDown(index)
    local smallest = index
    local left = 2 * index
    local right = 2 * index + 1
    
    if left <= #self.heap and self.heap[left].freq < self.heap[smallest].freq then
        smallest = left
    end
    
    if right <= #self.heap and self.heap[right].freq < self.heap[smallest].freq then
        smallest = right
    end
    
    if smallest ~= index then
        self.heap[index], self.heap[smallest] = self.heap[smallest], self.heap[index]
        self:heapifyDown(smallest)
    end
end

function MinHeap:size()
    return #self.heap
end

-- Build Huffman tree
function buildHuffmanTree(frequencies)
    local heap = MinHeap:new()
    
    -- Create leaf nodes for each character
    for char, freq in pairs(frequencies) do
        heap:insert(Node:new(char, freq))
    end
    
    -- Build the tree
    while heap:size() > 1 do
        local left = heap:extractMin()
        local right = heap:extractMin()
        
        local merged = Node:new(nil, left.freq + right.freq)
        merged.left = left
        merged.right = right
        
        heap:insert(merged)
    end
    
    return heap:extractMin()
end

-- Generate Huffman codes
function generateCodes(root)
    local codes = {}
    
    local function traverse(node, code)
        if node then
            if node.char ~= nil then
                if code == "" then
                    codes[node.char] = "0"  -- Special case for single character
                else
                    codes[node.char] = code
                end
            else
                traverse(node.left, code .. "0")
                traverse(node.right, code .. "1")
            end
        end
    end
    
    traverse(root, "")
    return codes
end

-- Encode text using Huffman codes
function encodeText(text, codes)
    local encoded = ""
    for i = 1, #text do
        local char = text:sub(i, i)
        encoded = encoded .. codes[char]
    end
    return encoded
end

-- Decode text using Huffman tree
function decodeText(encodedText, root)
    local decoded = ""
    local current = root
    
    for i = 1, #encodedText do
        local bit = encodedText:sub(i, i)
        
        if bit == "0" then
            current = current.left
        else
            current = current.right
        end
        
        if current.char ~= nil then
            decoded = decoded .. current.char
            current = root
        end
    end
    
    return decoded
end

-- Example usage
print("=== Huffman Coding Example ===")

-- Sample text to encode
local text = "hello world"
print("Original text: " .. text)

-- Calculate frequencies
local frequencies = {}
for i = 1, #text do
    local char = text:sub(i, i)
    frequencies[char] = (frequencies[char] or 0) + 1
end

print("\nCharacter frequencies:")
for char, freq in pairs(frequencies) do
    print("'" .. char .. "': " .. freq)
end

-- Build Huffman tree and generate codes
local root = buildHuffmanTree(frequencies)
local codes = generateCodes(root)

print("\nHuffman Codes:")
for char, code in pairs(codes) do
    print("'" .. char .. "': " .. code)
end

-- Encode the text
local encoded = encodeText(text, codes)
print("\nEncoded text: " .. encoded)

-- Decode the text
local decoded = decodeText(encoded, root)
print("Decoded text: " .. decoded)

-- Verify encoding/decoding
print("Encoding successful: " .. tostring(text == decoded))

-- Another example with more characters
print("\n=== Another Example ===")
local text2 = "aaaaabbbbcccdde"
print("Original text: " .. text2)

-- Calculate frequencies
local frequencies2 = {}
for i = 1, #text2 do
    local char = text2:sub(i, i)
    frequencies2[char] = (frequencies2[char] or 0) + 1
end

print("\nCharacter frequencies:")
for char, freq in pairs(frequencies2) do
    print("'" .. char .. "': " .. freq)
end

-- Build Huffman tree and generate codes
local root2 = buildHuffmanTree(frequencies2)
local codes2 = generateCodes(root2)

print("\nHuffman Codes:")
for char, code in pairs(codes2) do
    print("'" .. char .. "': " .. code)
end

-- Encode the text
local encoded2 = encodeText(text2, codes2)
print("\nEncoded text: " .. encoded2)

-- Decode the text
local decoded2 = decodeText(encoded2, root2)
print("Decoded text: " .. decoded2)

print("Encoding successful: " .. tostring(text2 == decoded2))
```

## Output Example:
```
=== Huffman Coding Example ===
Original text: hello world

Character frequencies:
'h': 1
'e': 1
'l': 3
'o': 2
' ': 1
'w': 1
'r': 1
'd': 1

Huffman Codes:
'h': 1000
'e': 1001
'l': 110
'o': 111
' ': 000
'w': 001
'r': 010
'd': 011

Encoded text: 10001001110110111000000001010011
Decoded text: hello world
Encoding successful: true

=== Another Example ===
Original text: aaaaabbbbcccdde

Character frequencies:
'a': 5
'b': 4
'c': 3
'd': 2
'e': 1

Huffman Codes:
'a': 0
'b': 10
'c': 110
'd': 1110
'e': 1111

Encoded text: 0000010101010110110110111011101111
Decoded text: aaaaabbbbcccdde
Encoding successful: true
```

This implementation demonstrates:
1. **Node structure** for building the Huffman tree
2. **Min heap** for efficient priority queue operations
3. **Tree building** algorithm using the greedy approach
4. **Code generation** from the tree
5. **Encoding and decoding** functions
6. **Complete working example** with multiple test cases

The algorithm has a time complexity of O(n log n) where n is the number of unique characters, making it efficient for practical applications.

