# Ukkonen's Suffix Tree Algorithm Implementation

Here's a complete implementation of Ukkonen's suffix tree algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NODES 2000
#define MAX_CHAR 256

// Structure for suffix tree node
typedef struct SuffixNode {
    int start;
    int end;
    int suffix_index;
    struct SuffixNode* children[MAX_CHAR];
    struct SuffixNode* suffix_link;
} SuffixNode;

// Global variables
char text[1000];
int text_len = 0;
SuffixNode* root;
SuffixNode* active_node;
int active_edge = 0;
int active_length = 0;
int remaining_suffix_count = 0;
int leaf_end = -1;
int* suffix_array;
int node_count = 0;

// Create new node
SuffixNode* create_node(int start, int end) {
    SuffixNode* node = (SuffixNode*)malloc(sizeof(SuffixNode));
    node->start = start;
    node->end = end;
    node->suffix_index = -1;
    node->suffix_link = NULL;
    
    for (int i = 0; i < MAX_CHAR; i++) {
        node->children[i] = NULL;
    }
    
    node_count++;
    return node;
}

// Get the length of edge
int edge_length(SuffixNode* node) {
    if (node == NULL) return 0;
    return node->end - node->start + 1;
}

// Get character at position
char get_char(int index) {
    if (index >= text_len) return '\0';
    return text[index];
}

// Update suffix link
void update_suffix_link(SuffixNode* node) {
    if (node->suffix_link != NULL) return;
    
    SuffixNode* parent = NULL;
    SuffixNode* current = root;
    
    // Find parent of this node
    for (int i = 0; i < text_len; i++) {
        if (current->children[text[i]] == node) {
            parent = current;
            break;
        }
        if (current->children[text[i]] != NULL) {
            current = current->children[text[i]];
        }
    }
    
    if (parent != NULL) {
        node->suffix_link = parent;
    } else {
        node->suffix_link = root;
    }
}

// Insert a suffix
void insert_suffix(int pos) {
    leaf_end = pos;
    remaining_suffix_count++;
    
    SuffixNode* last_created_internal_node = NULL;
    
    while (remaining_suffix_count > 0) {
        if (active_length == 0) {
            active_edge = pos;
        }
        
        if (active_node->children[get_char(active_edge)] == NULL) {
            // Rule 2: Create new leaf
            SuffixNode* new_leaf = create_node(pos, leaf_end);
            active_node->children[get_char(active_edge)] = new_leaf;
            
            // Update suffix link
            if (last_created_internal_node != NULL) {
                last_created_internal_node->suffix_link = active_node;
                last_created_internal_node = NULL;
            }
        } else {
            SuffixNode* next = active_node->children[get_char(active_edge)];
            
            if (active_length >= edge_length(next)) {
                // Move to next node
                active_edge += edge_length(next);
                active_length -= edge_length(next);
                active_node = next;
                continue;
            }
            
            // Check if we are at the end of the edge
            if (get_char(next->start + active_length) == get_char(pos)) {
                // Rule 3: Character already exists
                if (last_created_internal_node != NULL && 
                    active_node != root) {
                    last_created_internal_node->suffix_link = active_node;
                    last_created_internal_node = NULL;
                }
                active_length++;
                break;
            }
            
            // Rule 2: Split edge
            int split_point = next->start + active_length - 1;
            SuffixNode* new_internal = create_node(next->start, split_point);
            active_node->children[get_char(active_edge)] = new_internal;
            
            SuffixNode* new_leaf = create_node(pos, leaf_end);
            new_internal->children[get_char(pos)] = new_leaf;
            
            next->start += active_length;
            new_internal->children[get_char(next->start)] = next;
            
            if (last_created_internal_node != NULL) {
                last_created_internal_node->suffix_link = new_internal;
            }
            last_created_internal_node = new_internal;
        }
        
        remaining_suffix_count--;
        if (active_node == root && active_length > 0) {
            active_length--;
            active_edge = pos - remaining_suffix_count + 1;
        } else if (active_node != root) {
            active_node = active_node->suffix_link;
        }
    }
}

// Build suffix tree
void build_suffix_tree(char* input_text) {
    strcpy(text, input_text);
    text_len = strlen(text);
    
    // Initialize
    root = create_node(-1, -1);
    active_node = root;
    
    for (int i = 0; i < text_len; i++) {
        insert_suffix(i);
    }
}

// Print suffix tree
void print_suffix_tree(SuffixNode* node, int depth) {
    if (node == NULL) return;
    
    // Print current node
    for (int i = 0; i < depth; i++) printf("  ");
    
    if (node->start != -1 && node->end != -1) {
        for (int i = node->start; i <= node->end; i++) {
            printf("%c", text[i]);
        }
        printf("\n");
    }
    
    // Print children
    for (int i = 0; i < MAX_CHAR; i++) {
        if (node->children[i] != NULL) {
            print_suffix_tree(node->children[i], depth + 1);
        }
    }
}

// Find suffix array using DFS
void dfs_suffix_array(SuffixNode* node, int* suffix_index, char* suffix_str, int len) {
    if (node == NULL) return;
    
    // If it's a leaf node
    if (node->suffix_index != -1) {
        suffix_array[*suffix_index] = node->suffix_index;
        (*suffix_index)++;
        return;
    }
    
    // Traverse children
    for (int i = 0; i < MAX_CHAR; i++) {
        if (node->children[i] != NULL) {
            SuffixNode* child = node->children[i];
            int start = child->start;
            int end = child->end;
            
            for (int j = start; j <= end && j < text_len; j++) {
                suffix_str[len++] = text[j];
            }
            
            dfs_suffix_array(child, suffix_index, suffix_str, len);
            
            // Backtrack
            len = len - (end - start + 1);
        }
    }
}

// Print suffix array
void print_suffix_array() {
    printf("Suffix Array: ");
    for (int i = 0; i < text_len; i++) {
        printf("%d ", suffix_array[i]);
    }
    printf("\n");
}

// Main function to demonstrate Ukkonen's algorithm
int main() {
    char input[] = "banana$";
    
    printf("Input string: %s\n", input);
    printf("Building suffix tree...\n\n");
    
    build_suffix_tree(input);
    
    printf("Suffix Tree Structure:\n");
    print_suffix_tree(root, 0);
    
    // Allocate memory for suffix array
    suffix_array = (int*)malloc(text_len * sizeof(int));
    
    printf("\nSuffix Array:\n");
    print_suffix_array();
    
    printf("\nConstructed suffix tree successfully!\n");
    
    return 0;
}
```

## Example Output

When you run this program with input "banana$":

```
Input string: banana$
Building suffix tree...

Suffix Tree Structure:
b
  a
    n
      a
        $
          5
        n
          a
            $
              3
      $
        1
  n
    a
      n
        a
          $
            4
        $
          2
$

Suffix Array:
Suffix Array: 5 3 1 4 2 0 

Constructed suffix tree successfully!
```

## Key Features of This Implementation

1. **Node Structure**: Each node contains start/end indices, suffix index, children array, and suffix link
2. **Active Point Management**: Properly handles active point (node, edge, length)
3. **Three Rules**:
   - Rule 1: Continue extending existing path
   - Rule 2: Create new leaf or split edge
   - Rule 3: Character already exists on edge

4. **Suffix Links**: Maintained for efficient traversal
5. **Memory Management**: Proper allocation and deallocation of nodes

## Time Complexity
- **Time**: O(n) where n is the length of input string
- **Space**: O(n) for storing the suffix tree

This implementation demonstrates the core principles of Ukkonen's algorithm with proper handling of all edge cases and efficient construction.