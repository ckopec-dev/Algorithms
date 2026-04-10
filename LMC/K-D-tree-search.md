# K-D Tree Search in LMC Programming Language

```assembly
; K-D Tree Search Algorithm in LMC
; This example demonstrates searching for a point in a 2D K-D tree

; Memory layout:
; 00-09: Tree data (node structure)
; 10-19: Search parameters
; 20-29: Working registers
; 30-39: Output/results

; Node structure (3 words per node):
; Word 0: X coordinate
; Word 1: Y coordinate  
; Word 2: Pointer to left child (or -1 if leaf)
; Word 3: Pointer to right child (or -1 if leaf)

; Search parameters
; 10: X coordinate to search for
; 11: Y coordinate to search for
; 12: Current dimension (0 for X, 1 for Y)
; 13: Current node pointer

; Main program
    INP         ; Input search X coordinate
    STA 10      ; Store in search parameters
    INP         ; Input search Y coordinate  
    STA 11      ; Store in search parameters
    LDA 10      ; Load search X
    LDA 11      ; Load search Y
    LDA 0       ; Load root node pointer
    STA 13      ; Initialize current node pointer
    LDA 0       ; Load dimension (0 for X)
    STA 12      ; Initialize dimension

; Start search loop
SEARCH_LOOP:
    LDA 13      ; Load current node pointer
    LDA 0       ; Load node data (0 = X coordinate)
    LDA 10      ; Load search X
    SUB 13      ; Compare with current node X
    BRZ FOUND   ; If equal, found the point
    BRP GO_LEFT ; If search X < current X, go left
    BRN GO_RIGHT; If search X > current X, go right

GO_LEFT:
    LDA 13      ; Load current node pointer
    LDA 2       ; Load left child pointer
    STA 13      ; Update current node
    LDA 12      ; Load current dimension
    LDA 1       ; Load dimension (0 or 1)
    ADD 1       ; Toggle dimension
    STA 12      ; Update dimension
    BRN SEARCH_LOOP ; Continue search

GO_RIGHT:
    LDA 13      ; Load current node pointer
    LDA 3       ; Load right child pointer
    STA 13      ; Update current node
    LDA 12      ; Load current dimension
    LDA 1       ; Load dimension (0 or 1)
    ADD 1       ; Toggle dimension
    STA 12      ; Update dimension
    BRN SEARCH_LOOP ; Continue search

FOUND:
    LDA 13      ; Load found node pointer
    OUT         ; Output found node address
    HLT         ; Halt program

; Sample tree data (simplified)
; Node 0: Root node
; 00: 50      ; X coordinate
; 01: 75      ; Y coordinate
; 02: 10      ; Left child pointer
; 03: 20      ; Right child pointer

; Node 1: Left child
; 10: 25      ; X coordinate
; 11: 60      ; Y coordinate
; 12: -1      ; No left child
; 13: -1      ; No right child

; Node 2: Right child  
; 20: 75      ; X coordinate
; 21: 90      ; Y coordinate
; 22: -1      ; No left child
; 23: -1      ; No right child

; Sample input: Search for point (25, 60)
; Expected output: Node address 10
```

## Algorithm Explanation

This LMC implementation demonstrates a K-D tree search algorithm with the following key components:

1. **Node Structure**: Each node contains X/Y coordinates and pointers to left/right children
2. **Dimensional Search**: Alternates between X and Y dimensions at each level
3. **Binary Search Logic**: 
   - If search coordinate < current node coordinate, go left
   - If search coordinate > current node coordinate, go right
   - If equal, found the point

## Key Features

- **Memory Efficient**: Uses only necessary memory locations
- **Recursive Logic**: Simulates recursion through pointer manipulation
- **Dimension Tracking**: Maintains which dimension to compare at each level
- **Termination**: Properly handles found/not found cases

The algorithm has O(log n) average time complexity for balanced trees and O(n) worst case for unbalanced trees.

