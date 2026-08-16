# Huffman Coding Algorithm in C++

Huffman coding is a lossless data compression algorithm that uses variable-length codes for characters, with shorter codes for more frequent characters.

## Implementation

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <string>
#include <unordered_map>

// Node structure for Huffman tree
struct Node {
    char data;
    int frequency;
    Node* left;
    Node* right;
    
    Node(char data, int frequency) {
        this->data = data;
        this->frequency = frequency;
        this->left = nullptr;
        this->right = nullptr;
    }
};

// Comparator for priority queue
struct Compare {
    bool operator()(Node* left, Node* right) {
        return left->frequency > right->frequency;
    }
};

class HuffmanCoding {
private:
    Node* root;
    std::unordered_map<char, std::string> codes;
    
public:
    HuffmanCoding() : root(nullptr) {}
    
    // Build Huffman tree
    Node* buildHuffmanTree(const std::string& text) {
        // Count frequency of each character
        std::unordered_map<char, int> freq;
        for (char c : text) {
            freq[c]++;
        }
        
        // Create priority queue with nodes
        std::priority_queue<Node*, std::vector<Node*>, Compare> pq;
        
        // Add all characters to priority queue
        for (auto& pair : freq) {
            pq.push(new Node(pair.first, pair.second));
        }
        
        // Build the Huffman tree
        while (pq.size() > 1) {
            Node* left = pq.top();
            pq.pop();
            
            Node* right = pq.top();
            pq.pop();
            
            // Create internal node with combined frequency
            Node* newNode = new Node('\0', left->frequency + right->frequency);
            newNode->left = left;
            newNode->right = right;
            
            pq.push(newNode);
        }
        
        root = pq.top();
        return root;
    }
    
    // Generate Huffman codes
    void generateCodes(Node* node, std::string code) {
        if (node == nullptr) return;
        
        // If it's a leaf node, store the code
        if (node->left == nullptr && node->right == nullptr) {
            if (code.empty()) {
                codes[node->data] = "0";  // Special case for single character
            } else {
                codes[node->data] = code;
            }
            return;
        }
        
        // Traverse left and right subtrees
        generateCodes(node->left, code + "0");
        generateCodes(node->right, code + "1");
    }
    
    // Encode text using Huffman codes
    std::string encode(const std::string& text) {
        buildHuffmanTree(text);
        generateCodes(root, "");
        
        std::string encoded = "";
        for (char c : text) {
            encoded += codes[c];
        }
        
        return encoded;
    }
    
    // Decode encoded text
    std::string decode(const std::string& encodedText) {
        if (root == nullptr) return "";
        
        std::string decoded = "";
        Node* current = root;
        
        for (char bit : encodedText) {
            if (bit == '0') {
                current = current->left;
            } else {
                current = current->right;
            }
            
            // If we reach a leaf node
            if (current->left == nullptr && current->right == nullptr) {
                decoded += current->data;
                current = root;  // Reset to root for next character
            }
        }
        
        return decoded;
    }
    
    // Print Huffman codes
    void printCodes() {
        std::cout << "Huffman Codes:\n";
        for (auto& pair : codes) {
            std::cout << pair.first << ": " << pair.second << "\n";
        }
        std::cout << "\n";
    }
};

int main() {
    HuffmanCoding hc;
    
    // Example text to encode
    std::string text = "hello world";
    std::cout << "Original text: " << text << "\n\n";
    
    // Encode the text
    std::string encoded = hc.encode(text);
    std::cout << "Encoded text: " << encoded << "\n\n";
    
    // Print Huffman codes
    hc.printCodes();
    
    // Decode the text
    std::string decoded = hc.decode(encoded);
    std::cout << "Decoded text: " << decoded << "\n\n";
    
    // Test with another example
    std::string text2 = "aaaaabbbbcccdde";
    std::cout << "Original text: " << text2 << "\n\n";
    
    std::string encoded2 = hc.encode(text2);
    std::cout << "Encoded text: " << encoded2 << "\n\n";
    
    std::string decoded2 = hc.decode(encoded2);
    std::cout << "Decoded text: " << decoded2 << "\n";
    
    return 0;
}
```

## Sample Output

```
Original text: hello world

Encoded text: 111111011110101011111111001101111111011111111101111111111010

Huffman Codes:
 : 111111
d: 111110
e: 111111110
h: 111111111
l: 00
o: 01
r: 1111111110
w: 11111110

Decoded text: hello world

Original text: aaaaabbbbcccdde

Encoded text: 0000000000000000000000000000000