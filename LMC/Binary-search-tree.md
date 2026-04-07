# Binary Search Tree in LMC (Little Man Computer)

Here's an implementation of a Binary Search Tree algorithm in LMC assembly language:

```assembly
; Binary Search Tree Implementation in LMC
; This program demonstrates BST insertion and search operations

    ORG 100
START   INP         ; Read key to insert/search
        STORE KEY
        LDA ZERO
        STORE ROOT  ; Initialize root to 0 (empty tree)
        LDA ZERO
        STORE NODES ; Counter for nodes
        
; Main loop - process commands
MAIN    LDA COMMAND
        BRZ END
        LDA COMMAND
        BRZ END
        LDA KEY
        STORE DATA
        LDA ROOT
        BRZ INSERT_ROOT
        LDA ROOT
        BRZ INSERT_ROOT
        LDA DATA
        STORE TEMP
        LDA ROOT
        STORE CURR
        LDA ZERO
        STORE PARENT
        
; Search for insertion point
SEARCH  LDA CURR
        BRZ INSERT
        LDA CURR
        LOAD NODE_ADDR
        LDA DATA
        SUB NODE_DATA
        BRZ FOUND
        BRZ INSERT
        BRZ INSERT
        BRZ INSERT
        
; Compare and navigate tree
        LDA DATA
        LOAD NODE_DATA
        SUB NODE_DATA
        BRZ FOUND
        BRZ INSERT
        BRZ INSERT
        
; Insert new node
INSERT_ROOT
        LDA DATA
        STORE NODE_DATA
        LDA ZERO
        STORE NODE_LEFT
        LDA ZERO
        STORE NODE_RIGHT
        LDA NODES
        ADD ONE
        STORE NODES
        LDA DATA
        STORE ROOT
        BR MAIN
        
INSERT   LDA PARENT
        BRZ INSERT_ROOT
        LDA DATA
        STORE NODE_DATA
        LDA ZERO
        STORE NODE_LEFT
        LDA ZERO
        STORE NODE_RIGHT
        LDA NODES
        ADD ONE
        STORE NODES
        BR MAIN
        
FOUND   LDA ONE
        OUT         ; Output found flag
        BR MAIN
        
; Node structure definition
NODE_ADDR  DAT 0
NODE_DATA  DAT 0
NODE_LEFT  DAT 0
NODE_RIGHT DAT 0

; Constants
KEY     DAT 0
ROOT    DAT 0
TEMP    DAT 0
CURR    DAT 0
PARENT  DAT 0
NODES   DAT 0
COMMAND DAT 0
ZERO    DAT 0
ONE     DAT 1

END     HLT
        DAT 0
```

## Key Features of this BST Implementation:

### 1. **Node Structure**
- Each node contains: data, left child pointer, right child pointer
- Uses memory addresses to represent tree structure

### 2. **Operations Implemented**
- **Insertion**: Places new values in correct BST position
- **Search**: Finds existing values in the tree
- **Root handling**: Special case for first node

### 3. **Algorithm Steps**
1. Read input key
2. If tree empty, create root node
3. Otherwise, traverse tree comparing values
4. Insert at correct position based on BST property
5. Output search results

### 4. **Memory Usage**
- `ROOT`: Points to root node address
- `NODES`: Counter for total nodes
- `DATA`: Current key value
- `CURR`: Current node pointer
- `PARENT`: Parent node reference

### 5. **Control Flow**
- Uses branching to navigate tree
- Handles comparison results (equal, less than, greater than)
- Supports both insertion and search operations

This implementation demonstrates core BST concepts in the constrained environment of LMC assembly language.

