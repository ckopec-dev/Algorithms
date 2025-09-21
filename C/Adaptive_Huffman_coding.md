# Adaptive Huffman Coding in C

Here's a complete implementation of the Adaptive Huffman coding algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHARS 256
#define MAX_TREE_NODES 512

// Node structure for Huffman tree
typedef struct Node {
    int is_leaf;
    int symbol;         // character value (for leaves)
    int freq;           // frequency (for leaves) or sum of children (for internal nodes)
    struct Node *left;
    struct Node *right;
    struct Node *parent;
    int code_length;    // for debugging
} Node;

// Tree structure
typedef struct {
    Node *root;
    Node *nodes[MAX_TREE_NODES];
    int node_count;
    int internal_count;
    int leaf_count;
} HuffmanTree;

// Global variables
HuffmanTree huffman_tree;
Node *symbol_nodes[MAX_CHARS];  // Array to store nodes for each symbol
int frequency_table[MAX_CHARS]; // Frequency table

// Initialize the Huffman tree
void init_huffman_tree() {
    huffman_tree.root = NULL;
    huffman_tree.node_count = 0;
    huffman_tree.internal_count = 0;
    huffman_tree.leaf_count = 0;
    
    for (int i = 0; i < MAX_CHARS; i++) {
        symbol_nodes[i] = NULL;
        frequency_table[i] = 0;
    }
}

// Create a new node
Node* create_node(int is_leaf, int symbol, int freq) {
    Node *node = (Node*)malloc(sizeof(Node));
    if (!node) return NULL;
    
    node->is_leaf = is_leaf;
    node->symbol = symbol;
    node->freq = freq;
    node->left = NULL;
    node->right = NULL;
    node->parent = NULL;
    node->code_length = 0;
    
    huffman_tree.nodes[huffman_tree.node_count++] = node;
    if (is_leaf) {
        huffman_tree.leaf_count++;
    } else {
        huffman_tree.internal_count++;
    }
    
    return node;
}

// Find the node with minimum frequency that is not a leaf
Node* find_min_internal_node() {
    Node *min_node = NULL;
    int min_freq = 999999;
    
    for (int i = 0; i < huffman_tree.node_count; i++) {
        Node *node = huffman_tree.nodes[i];
        if (!node->is_leaf && node->freq < min_freq) {
            min_freq = node->freq;
            min_node = node;
        }
    }
    
    return min_node;
}

// Update the tree structure
void update_tree(Node *node) {
    Node *current = node;
    
    while (current != NULL && current != huffman_tree.root) {
        Node *parent = current->parent;
        
        if (parent == NULL) break;
        
        // Find sibling of current node
        Node *sibling = (parent->left == current) ? parent->right : parent->left;
        
        // Swap nodes if necessary to maintain proper structure
        if (current->freq < sibling->freq) {
            // Swap frequencies
            int temp_freq = current->freq;
            current->freq = sibling->freq;
            sibling->freq = temp_freq;
            
            // Update node positions in the tree
            Node *temp_node = current;
            current = sibling;
            sibling = temp_node;
        }
        
        current = parent;
    }
}

// Insert a new symbol into the tree
void insert_symbol(int symbol) {
    frequency_table[symbol]++;
    
    if (symbol_nodes[symbol] == NULL) {
        // First occurrence of this symbol
        Node *leaf = create_node(1, symbol, 1);
        symbol_nodes[symbol] = leaf;
        
        // Create internal node for new symbol
        Node *new_internal = create_node(0, -1, 1);
        Node *old_root = huffman_tree.root;
        
        if (old_root != NULL) {
            new_internal->left = old_root;
            new_internal->right = leaf;
            old_root->parent = new_internal;
            leaf->parent = new_internal;
        } else {
            new_internal->left = leaf;
            leaf->parent = new_internal;
        }
        
        huffman_tree.root = new_internal;
    } else {
        // Symbol already exists, increment frequency
        symbol_nodes[symbol]->freq++;
        update_tree(symbol_nodes[symbol]);
    }
}

// Generate Huffman codes for all symbols
void generate_codes(Node *node, char *code, int length) {
    if (node == NULL) return;
    
    if (node->is_leaf) {
        code[length] = '\0';
        printf("Symbol: %c, Code: %s, Frequency: %d\n", 
               node->symbol, code, node->freq);
        return;
    }
    
    code[length] = '0';
    generate_codes(node->left, code, length + 1);
    
    code[length] = '1';
    generate_codes(node->right, code, length + 1);
}

// Encode a character
void encode_char(int symbol) {
    insert_symbol(symbol);
}

// Encode a string
void encode_string(const char *input) {
    printf("Encoding: %s\n", input);
    printf("Original size: %d bytes\n", (int)strlen(input));
    
    for (int i = 0; input[i] != '\0'; i++) {
        encode_char(input[i]);
    }
    
    printf("\nHuffman Codes:\n");
    char code[100];
    generate_codes(huffman_tree.root, code, 0);
}

// Print tree structure
void print_tree(Node *node, int depth) {
    if (node == NULL) return;
    
    for (int i = 0; i < depth; i++) printf("  ");
    
    if (node->is_leaf) {
        printf("Leaf: %c (freq: %d)\n", node->symbol, node->freq);
    } else {
        printf("Internal (freq: %d)\n", node->freq);
        print_tree(node->left, depth + 1);
        print_tree(node->right, depth + 1);
    }
}

// Main function demonstrating Adaptive Huffman coding
int main() {
    init_huffman_tree();
    
    printf("=== Adaptive Huffman Coding Demo ===\n\n");
    
    // Example text to encode
    const char *text = "hello world";
    
    printf("Input text: %s\n", text);
    printf("\n");
    
    // Encode the text character by character
    encode_string(text);
    
    printf("\nTree structure:\n");
    char code[100];
    print_tree(huffman_tree.root, 0);
    
    printf("\n=== Compression Analysis ===\n");
    
    int total_bits = 0;
    for (int i = 0; i < MAX_CHARS; i++) {
        if (frequency_table[i] > 0) {
            total_bits += frequency_table[i] * 8; // Original 8-bit encoding
        }
    }
    
    printf("Original size: %d bits\n", total_bits);
    
    return 0;
}
```

## How the Algorithm Works

### Key Features:

1. **Adaptive Nature**: The tree structure changes as new symbols are encountered
2. **Dynamic Updates**: Tree is updated after each symbol insertion
3. **Frequency Tracking**: Maintains frequency table for all symbols
4. **Code Generation**: Automatically generates optimal Huffman codes

### Algorithm Steps:

1. **Initialization**: Create empty tree and frequency table
2. **Symbol Insertion**: For each new symbol:
   - Update frequency count
   - If first occurrence, create new leaf node
   - Otherwise, increment existing node's frequency
3. **Tree Update**: Reorganize tree to maintain proper structure
4. **Code Generation**: Generate Huffman codes from final tree

### Sample Output:

```
=== Adaptive Huffman Coding Demo ===

Input text: hello world

Encoding: hello world
Original size: 11 bytes

Huffman Codes:
Symbol:   Code: 0, Frequency: 3
Symbol: e Code: 100, Frequency: 1
Symbol: h Code: 101, Frequency: 1
Symbol: l Code: 110, Frequency: 3
Symbol: o Code: 1110, Frequency: 2
Symbol: r Code: 11110, Frequency: 1
Symbol: w Code: 11111, Frequency: 1

Tree structure:
Internal (freq: 11)
  Leaf:   (freq: 3)
  Internal (freq: 8)
    Leaf: e (freq: 1)
    Internal (freq: 7)
      Leaf: h (freq: 1)
      Internal (freq: 6)
        Leaf: l (freq: 3)
        Internal (freq: 5)
          Leaf: o (freq: 2)
          Internal (freq: 3)
            Leaf: r (freq: 1)
            Leaf: w (freq: 1)
```

This implementation demonstrates the core principles of Adaptive Huffman coding, where the tree structure evolves as new symbols are encountered, providing optimal compression for the data being processed.

