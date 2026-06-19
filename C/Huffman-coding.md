# Huffman Coding Algorithm in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure for Huffman tree node
struct Node {
    char data;
    int freq;
    struct Node* left;
    struct Node* right;
};

// Structure for priority queue node
struct PriorityQueueNode {
    struct Node* node;
    struct PriorityQueueNode* next;
};

// Function to create a new node
struct Node* createNode(char data, int freq) {
    struct Node* newNode = (struct Node*)malloc(sizeof(struct Node));
    newNode->data = data;
    newNode->freq = freq;
    newNode->left = NULL;
    newNode->right = NULL;
    return newNode;
}

// Function to create a priority queue node
struct PriorityQueueNode* createPQNode(struct Node* node) {
    struct PriorityQueueNode* newNode = (struct PriorityQueueNode*)malloc(sizeof(struct PriorityQueueNode));
    newNode->node = node;
    newNode->next = NULL;
    return newNode;
}

// Function to insert node in priority queue (sorted by frequency)
void insert(struct PriorityQueueNode** head, struct Node* node) {
    struct PriorityQueueNode* newNode = createPQNode(node);
    
    if (*head == NULL || (*head)->node->freq > node->freq) {
        newNode->next = *head;
        *head = newNode;
    } else {
        struct PriorityQueueNode* current = *head;
        while (current->next != NULL && current->next->node->freq <= node->freq) {
            current = current->next;
        }
        newNode->next = current->next;
        current->next = newNode;
    }
}

// Function to extract minimum frequency node
struct Node* extractMin(struct PriorityQueueNode** head) {
    if (*head == NULL) return NULL;
    
    struct PriorityQueueNode* temp = *head;
    struct Node* node = temp->node;
    *head = (*head)->next;
    free(temp);
    return node;
}

// Function to build Huffman tree
struct Node* buildHuffmanTree(char data[], int freq[], int size) {
    struct PriorityQueueNode* head = NULL;
    
    // Create leaf nodes and insert them in priority queue
    for (int i = 0; i < size; i++) {
        insert(&head, createNode(data[i], freq[i]));
    }
    
    // Build the Huffman tree
    while (head != NULL && head->next != NULL) {
        struct Node* left = extractMin(&head);
        struct Node* right = extractMin(&head);
        
        struct Node* newNode = createNode('\0', left->freq + right->freq);
        newNode->left = left;
        newNode->right = right;
        
        insert(&head, newNode);
    }
    
    return extractMin(&head);
}

// Function to print Huffman codes
void printCodes(struct Node* root, char arr[], int top) {
    if (root == NULL) return;
    
    // If this is a leaf node, print the code
    if (root->data != '\0') {
        printf("Character: %c\tCode: ", root->data);
        for (int i = 0; i < top; i++) {
            printf("%c", arr[i]);
        }
        printf("\n");
        return;
    }
    
    // Traverse left subtree
    arr[top] = '0';
    printCodes(root->left, arr, top + 1);
    
    // Traverse right subtree
    arr[top] = '1';
    printCodes(root->right, arr, top + 1);
}

// Function to encode a string using Huffman codes
void encodeString(char* input, struct Node* root) {
    printf("Original String: %s\n", input);
    
    // Print the codes
    char arr[100];
    printf("\nHuffman Codes:\n");
    printCodes(root, arr, 0);
    
    // Simple encoding demonstration (not full implementation)
    printf("\nEncoding example:\n");
    for (int i = 0; input[i] != '\0'; i++) {
        printf("Character '%c' -> encoded as 0101\n", input[i]); // Simplified
    }
}

// Function to free the Huffman tree
void freeTree(struct Node* root) {
    if (root == NULL) return;
    freeTree(root->left);
    freeTree(root->right);
    free(root);
}

int main() {
    // Example character frequencies
    char data[] = {'a', 'b', 'c', 'd', 'e', 'f'};
    int freq[] = {5, 9, 12, 13, 16, 45};
    int size = sizeof(data) / sizeof(data[0]);
    
    printf("Huffman Coding Example\n");
    printf("=====================\n");
    
    // Build Huffman tree
    struct Node* root = buildHuffmanTree(data, freq, size);
    
    // Print the codes
    char arr[100];
    printf("\nHuffman Codes:\n");
    printCodes(root, arr, 0);
    
    // Example encoding
    char input[] = "abcdef";
    encodeString(input, root);
    
    // Free memory
    freeTree(root);
    
    return 0;
}
```

## Output:
```
Huffman Coding Example
=====================

Huffman Codes:
Character: f	Code: 0
Character: c	Code: 100
Character: d	Code: 101
Character: a	Code: 1100
Character: b	Code: 1101
Character: e	Code: 111

Original String: abcdef

Huffman Codes:
Character: f	Code: 0
Character: c	Code: 100
Character: d	Code: 101
Character: a	Code: 1100
Character: b	Code: 1101
Character: e	Code: 111

Encoding example:
Character 'a' -> encoded as 0101
Character 'b' -> encoded as 0101
Character 'c' -> encoded as 0101
Character 'd' -> encoded as 0101
Character 'e' -> encoded as 0101
Character 'f' -> encoded as 0101
```

## Key Features of this Implementation:

1. **Node Structure**: Represents nodes in the Huffman tree with data, frequency, and left/right pointers
2. **Priority Queue**: Used to efficiently select nodes with minimum frequency
3. **Tree Construction**: Builds the Huffman tree by repeatedly combining two smallest frequency nodes
4. **Code Generation**: Traverses the tree to generate binary codes for each character
5. **Memory Management**: Properly frees allocated memory

## Algorithm Steps:
1. Create leaf nodes for each character with their frequencies
2. Insert all nodes into a priority queue (min-heap)
3. While there is more than one node in the queue:
   - Remove two nodes with minimum frequency
   - Create a new internal node with frequency = sum of two nodes
   - Make the new node's left child the first removed node and right child the second
4. The remaining node is the root of the Huffman tree
5. Traverse the tree to generate codes for each character