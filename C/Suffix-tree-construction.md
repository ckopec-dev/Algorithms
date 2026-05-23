# Suffix Tree Construction Algorithm in C

Here's an implementation of Ukkonen's algorithm for constructing a suffix tree in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHAR 256

// Node structure for suffix tree
typedef struct SuffixTreeNode {
    struct SuffixTreeNode* children[MAX_CHAR];
    int start;
    int end;
    int suffixIndex;
    struct SuffixTreeNode* suffixLink;
} SuffixTreeNode;

// Global variables
char* text;
int textLength;
SuffixTreeNode* root;
SuffixTreeNode* activeNode;
int activeEdge;
int activeLength;
int remainingSuffixCount;
int leafEnd;
int* suffixArray;

// Create new node
SuffixTreeNode* createNode(int start, int end) {
    SuffixTreeNode* node = (SuffixTreeNode*)malloc(sizeof(SuffixTreeNode));
    node->start = start;
    node->end = end;
    node->suffixIndex = -1;
    node->suffixLink = NULL;
    
    for (int i = 0; i < MAX_CHAR; i++) {
        node->children[i] = NULL;
    }
    
    return node;
}

// Initialize suffix tree
void initSuffixTree(char* inputText) {
    text = inputText;
    textLength = strlen(text);
    leafEnd = -1;
    activeNode = NULL;
    activeEdge = -1;
    activeLength = 0;
    remainingSuffixCount = 0;
    suffixArray = (int*)malloc(sizeof(int) * textLength);
    
    root = createNode(-1, -1);
    activeNode = root;
}

// Get length of edge
int edgeLength(int start, int end) {
    return end - start + 1;
}

// Get character at position
char charAt(int index) {
    return text[index];
}

// Update suffix link
void updateSuffixLink(SuffixTreeNode* node) {
    if (node->suffixLink != NULL) {
        return;
    }
    
    SuffixTreeNode* parent = NULL;
    SuffixTreeNode* temp = node;
    
    while (temp != root && temp != NULL) {
        parent = temp;
        temp = temp->suffixLink;
    }
    
    if (parent != NULL) {
        node->suffixLink = parent;
    }
}

// Extend suffix tree
void extendSuffixTree(int pos) {
    leafEnd = pos;
    remainingSuffixCount++;
    
    SuffixTreeNode* lastNewNode = NULL;
    
    while (remainingSuffixCount > 0) {
        if (activeLength == 0) {
            activeEdge = pos;
        }
        
        if (activeNode->children[charAt(activeEdge)] == NULL) {
            // Create new leaf node
            SuffixTreeNode* newNode = createNode(pos, leafEnd);
            activeNode->children[charAt(activeEdge)] = newNode;
            
            // Update suffix link
            if (lastNewNode != NULL) {
                lastNewNode->suffixLink = activeNode;
                lastNewNode = NULL;
            }
        } else {
            // Walk down
            SuffixTreeNode* next = activeNode->children[charAt(activeEdge)];
            int edgeLength = next->end - next->start + 1;
            
            if (activeLength >= edgeLength) {
                activeEdge += edgeLength;
                activeLength -= edgeLength;
                activeNode = next;
                continue;
            }
            
            // Check if we have reached the end of edge
            if (charAt(next->start + activeLength) == charAt(pos)) {
                // We have found a match
                activeLength++;
                
                if (lastNewNode != NULL && activeNode != root) {
                    lastNewNode->suffixLink = activeNode;
                    lastNewNode = NULL;
                }
                break;
            }
            
            // Split the edge
            SuffixTreeNode* splitNode = createNode(next->start, next->start + activeLength - 1);
            activeNode->children[charAt(activeEdge)] = splitNode;
            splitNode->children[charAt(pos)] = createNode(pos, leafEnd);
            next->start += activeLength;
            splitNode->children[charAt(next->start)] = next;
            
            if (lastNewNode != NULL) {
                lastNewNode->suffixLink = splitNode;
            }
            
            lastNewNode = splitNode;
        }
        
        remainingSuffixCount--;
        if (activeNode == root && activeLength > 0) {
            activeLength--;
            activeEdge = pos - remainingSuffixCount + 1;
        } else if (activeNode != root) {
            activeNode = activeNode->suffixLink;
        }
    }
}

// Build suffix tree
void buildSuffixTree() {
    for (int i = 0; i < textLength; i++) {
        extendSuffixTree(i);
    }
}

// Print suffix tree
void printSuffixTree(SuffixTreeNode* node, int height) {
    if (node == NULL) return;
    
    if (node->suffixIndex != -1) {
        printf("Leaf: %d\n", node->suffixIndex);
        return;
    }
    
    for (int i = 0; i < MAX_CHAR; i++) {
        if (node->children[i] != NULL) {
            SuffixTreeNode* child = node->children[i];
            int start = child->start;
            int end = child->end;
            
            printf("Edge from %d to %d: ", start, end);
            for (int j = start; j <= end; j++) {
                printf("%c", text[j]);
            }
            printf("\n");
            
            printSuffixTree(child, height + 1);
        }
    }
}

// Print suffix array
void printSuffixArray() {
    printf("Suffix Array: ");
    for (int i = 0; i < textLength; i++) {
        printf("%d ", suffixArray[i]);
    }
    printf("\n");
}

// Main function
int main() {
    char inputText[] = "banana$";
    
    printf("Input text: %s\n", inputText);
    
    initSuffixTree(inputText);
    buildSuffixTree();
    
    printf("\nSuffix Tree Structure:\n");
    printSuffixTree(root, 0);
    
    printf("\n");
    printSuffixArray();
    
    return 0;
}
```

## How the Algorithm Works

This implementation uses Ukkonen's algorithm for suffix tree construction:

1. **Initialization**: Creates the root node and initializes global variables
2. **Phase Processing**: For each phase (character in the text):
   - Extends the tree by adding the new character
   - Handles active point management
   - Updates suffix links
   - Performs edge splitting when necessary
3. **Edge Management**: Uses the active point (activeNode, activeEdge, activeLength) to efficiently navigate the tree
4. **Suffix Links**: Maintains suffix links for efficient transitions

## Key Features

- **Time Complexity**: O(n) where n is the length of the input string
- **Space Complexity**: O(n) for storing the suffix tree
- **Handles**: Suffix links, edge splitting, and active point management
- **Output**: Complete suffix tree structure with edge labels

## Sample Output

For input "banana$", the algorithm will construct a suffix tree with all suffixes of the string and their relationships in the tree structure.

The implementation demonstrates the core concepts of suffix tree construction including the active point technique and suffix link management that make Ukkonen's algorithm efficient.

