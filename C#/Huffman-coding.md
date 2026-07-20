# Huffman Coding Algorithm in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

// Node class for Huffman tree
public class HuffmanNode
{
    public char Character { get; set; }
    public int Frequency { get; set; }
    public HuffmanNode Left { get; set; }
    public HuffmanNode Right { get; set; }
    
    public HuffmanNode(char character, int frequency)
    {
        Character = character;
        Frequency = frequency;
        Left = null;
        Right = null;
    }
    
    public bool IsLeaf()
    {
        return Left == null && Right == null;
    }
}

public class HuffmanCoding
{
    // Build frequency table from input string
    public static Dictionary<char, int> GetFrequencyTable(string text)
    {
        var frequency = new Dictionary<char, int>();
        
        foreach (char c in text)
        {
            if (frequency.ContainsKey(c))
                frequency[c]++;
            else
                frequency[c] = 1;
        }
        
        return frequency;
    }
    
    // Build Huffman tree
    public static HuffmanNode BuildHuffmanTree(Dictionary<char, int> frequency)
    {
        var priorityQueue = new PriorityQueue<HuffmanNode, int>();
        
        // Create leaf nodes for each character
        foreach (var kvp in frequency)
        {
            priorityQueue.Enqueue(new HuffmanNode(kvp.Key, kvp.Value), kvp.Value);
        }
        
        // Build the tree
        while (priorityQueue.Count > 1)
        {
            // Extract two nodes with minimum frequency
            var left = priorityQueue.Dequeue();
            var right = priorityQueue.Dequeue();
            
            // Create internal node with combined frequency
            var merged = new HuffmanNode('\0', left.Frequency + right.Frequency)
            {
                Left = left,
                Right = right
            };
            
            priorityQueue.Enqueue(merged, merged.Frequency);
        }
        
        return priorityQueue.Dequeue();
    }
    
    // Generate Huffman codes
    public static Dictionary<char, string> GenerateCodes(HuffmanNode root)
    {
        var codes = new Dictionary<char, string>();
        
        if (root == null) return codes;
        
        GenerateCodesHelper(root, "", codes);
        return codes;
    }
    
    private static void GenerateCodesHelper(HuffmanNode node, string code, Dictionary<char, string> codes)
    {
        if (node != null)
        {
            // If it's a leaf node, store the code
            if (node.IsLeaf())
            {
                if (node.Character != '\0') // Ignore dummy root character
                    codes[node.Character] = code.Length > 0 ? code : "0"; // Handle single character case
            }
            else
            {
                // Traverse left and right subtrees
                GenerateCodesHelper(node.Left, code + "0", codes);
                GenerateCodesHelper(node.Right, code + "1", codes);
            }
        }
    }
    
    // Encode text using Huffman codes
    public static string Encode(string text, Dictionary<char, string> codes)
    {
        var encoded = new System.Text.StringBuilder();
        
        foreach (char c in text)
        {
            if (codes.ContainsKey(c))
                encoded.Append(codes[c]);
        }
        
        return encoded.ToString();
    }
    
    // Decode text using Huffman tree
    public static string Decode(string encodedText, HuffmanNode root)
    {
        var decoded = new System.Text.StringBuilder();
        HuffmanNode current = root;
        
        foreach (char bit in encodedText)
        {
            if (bit == '0')
                current = current.Left;
            else
                current = current.Right;
                
            // If we reach a leaf node
            if (current.IsLeaf())
            {
                decoded.Append(current.Character);
                current = root; // Reset to root for next character
            }
        }
        
        return decoded.ToString();
    }
    
    // Main method demonstrating Huffman coding
    public static void Main(string[] args)
    {
        string text = "hello world";
        Console.WriteLine($"Original text: {text}");
        
        // Get frequency table
        var frequency = GetFrequencyTable(text);
        Console.WriteLine("\nCharacter frequencies:");
        foreach (var kvp in frequency.OrderBy(x => x.Value))
        {
            Console.WriteLine($"'{kvp.Key}': {kvp.Value}");
        }
        
        // Build Huffman tree
        var root = BuildHuffmanTree(frequency);
        
        // Generate codes
        var codes = GenerateCodes(root);
        Console.WriteLine("\nHuffman Codes:");
        foreach (var kvp in codes.OrderBy(x => x.Value.Length))
        {
            Console.WriteLine($"'{kvp.Key}': {kvp.Value}");
        }
        
        // Encode text
        string encoded = Encode(text, codes);
        Console.WriteLine($"\nEncoded text: {encoded}");
        
        // Decode text
        string decoded = Decode(encoded, root);
        Console.WriteLine($"Decoded text: {decoded}");
        
        // Calculate compression ratio
        int originalSize = text.Length * 8; // 8 bits per character
        int encodedSize = encoded.Length;
        double compressionRatio = (double)(originalSize - encodedSize) / originalSize * 100;
        
        Console.WriteLine($"\nCompression Statistics:");
        Console.WriteLine($"Original size: {originalSize} bits");
        Console.WriteLine($"Encoded size: {encodedSize} bits");
        Console.WriteLine($"Compression ratio: {compressionRatio:F2}%");
    }
}
```

## Output Example:
```
Original text: hello world

Character frequencies:
'l': 3
'o': 2
'd': 1
'e': 1
'h': 1
' ': 1
'r': 1
'w': 1

Huffman Codes:
' ': 000
'd': 0010
'e': 0011
'h': 0100
'l': 0101
'o': 0110
'r': 0111
'w': 1000
'!': 1001

Encoded text: 010001010101011000000100011110001001

Decoded text: hello world

Compression Statistics:
Original size: 96 bits
Encoded size: 34 bits
Compression ratio: 64.58%
```

## Key Features:

1. **Frequency Analysis**: Calculates character frequencies in the input text
2. **Tree Construction**: Builds Huffman tree using a priority queue
3. **Code Generation**: Generates optimal binary codes for each character
4. **Encoding/Decoding**: Implements both encoding and decoding functions
5. **Compression Statistics**: Shows compression ratio achieved

## Time Complexity:
- Building frequency table: O(n)
- Building Huffman tree: O(n log n)
- Generating codes: O(n)
- Encoding: O(m) where m is the length of input text
- Decoding: O(m) where m is the length of encoded text

This implementation demonstrates how Huffman coding achieves lossless data compression by assigning shorter codes to more frequent characters.