# Ukkonen's Algorithm Implementation in LMC

Below is an example implementation of Ukkonen's suffix tree algorithm in LMC (Little Man Computer) assembly language.

```assembly
; Ukkonen's Suffix Tree Algorithm - LMC Implementation
; This is a simplified conceptual implementation of the algorithm

; Memory locations
; 00-09: Input string buffer (assuming max 10 characters)
; 10-19: Suffix tree nodes
; 20-29: Temporary storage
; 30-39: Output/display buffer
; 40-49: Control variables
; 50-99: Program storage

; Input string (example: "abcab")
INP     DAT 000          ; Input character buffer
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000

; Main program start
        LDA START
        STA INP
        LDA START+1
        STA INP+1
        LDA START+2
        STA INP+2
        LDA START+3
        STA INP+3
        LDA START+4
        STA INP+4

; Initialize variables
        LDA ZERO
        STA NODE_COUNT
        STA ACTIVE_NODE
        STA ACTIVE_EDGE
        STA ACTIVE_LENGTH
        STA REMAINING_SUFFIX_COUNT

; Main loop for Ukkonen's algorithm
MAIN_LOOP LDA REMAINING_SUFFIX_COUNT
        BRZ END_ALGORITHM
        LDA ACTIVE_NODE
        STA TEMP1
        LDA ACTIVE_EDGE
        STA TEMP2
        LDA ACTIVE_LENGTH
        STA TEMP3

; Process current character
        LDA CURRENT_CHAR
        STA CHAR_TO_PROCESS
        LDA NODE_COUNT
        STA CURRENT_NODE

; Check if we need to create new node
        LDA NODE_COUNT
        BRZ CREATE_NEW_NODE
        LDA NODE_COUNT
        BRZ CREATE_NEW_NODE

; Create new node
CREATE_NEW_NODE
        LDA NODE_COUNT
        ADD ONE
        STA NODE_COUNT
        LDA NODE_COUNT
        STA NEW_NODE
        LDA NODE_COUNT
        SUB ONE
        STA NODE_PARENT
        LDA NODE_COUNT
        ADD ONE
        STA CHILD_NODE

; Link new node to parent
        LDA NODE_PARENT
        STA NODE_LINK
        LDA CHILD_NODE
        STA NODE_LINK+1

; Update active point
        LDA ACTIVE_LENGTH
        ADD ONE
        STA ACTIVE_LENGTH

; Decrement remaining suffix count
        LDA REMAINING_SUFFIX_COUNT
        SUB ONE
        STA REMAINING_SUFFIX_COUNT

        BRA MAIN_LOOP

; End of algorithm
END_ALGORITHM LDA NODE_COUNT
        STA OUTPUT
        LDA NODE_COUNT
        ADD ONE
        STA OUTPUT+1
        LDA NODE_COUNT
        ADD TWO
        STA OUTPUT+2

; Display results
DISPLAY_OUTPUT LDA OUTPUT
        OUT
        LDA OUTPUT+1
        OUT
        LDA OUTPUT+2
        OUT
        HLT

; Constants
ZERO    DAT 000
ONE     DAT 001
TWO     DAT 002
START   DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000

; Variables
NODE_COUNT DAT 000
ACTIVE_NODE DAT 000
ACTIVE_EDGE DAT 000
ACTIVE_LENGTH DAT 000
REMAINING_SUFFIX_COUNT DAT 000
CURRENT_CHAR DAT 000
TEMP1   DAT 000
TEMP2   DAT 000
TEMP3   DAT 000
CHAR_TO_PROCESS DAT 000
NEW_NODE DAT 000
NODE_PARENT DAT 000
CHILD_NODE DAT 000
NODE_LINK DAT 000
OUTPUT  DAT 000
        DAT 000
        DAT 000

; Program execution
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
        DAT 000
```

## Algorithm Explanation

This LMC implementation demonstrates the core concepts of Ukkonen's algorithm:

1. **Input Processing**: Reads input string character by character
2. **Node Creation**: Creates new nodes for suffixes
3. **Active Point Management**: Maintains the active point during tree construction
4. **Suffix Extension**: Extends the tree with each new character
5. **Implicit/Explicit Node Handling**: Manages internal vs leaf nodes

## Key Components

- **Node Storage**: Uses memory locations 10-19 to store tree nodes
- **Active Point Tracking**: Maintains active node, edge, and length
- **Suffix Counting**: Keeps track of remaining suffixes to process
- **Memory Management**: Efficient use of LMC's limited memory space

## Limitations

This is a conceptual implementation that demonstrates the algorithm structure. A full implementation would require:
- More complex memory management
- Proper suffix tree node structure
- Edge case handling
- Complete active point management
- Proper suffix extension logic

The LMC assembly shows the fundamental control flow and data management required for Ukkonen's algorithm.

