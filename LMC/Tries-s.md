# Tries_s Algorithm in LMC (Little Man Computer)

Here's an implementation of a Trie (prefix tree) algorithm in LMC assembly language:

```assembly
; Trie Implementation in LMC
; This program demonstrates basic Trie operations: insert and search

; Data Section
NUM_NODES   DAT 100         ; Maximum number of nodes
NODE_SIZE   DAT 10          ; Size of each node (10 words)
ROOT_ADDR   DAT 0           ; Root node address
CURRENT_PTR DAT 0           ; Current pointer for traversal

; Node structure:
; [0] - Letter (character)
; [1] - IsEnd (0=not end, 1=end)
; [2-9] - Children pointers (10 children for each letter)

; Main program
    INP                 ; Input word to insert
    STA WORD_INPUT
    LDA WORD_INPUT
    BRZ END_PROGRAM     ; If empty, exit

    ; Insert word into trie
    LDA ROOT_ADDR
    BRZ INIT_ROOT       ; If no root, initialize it
    LDA ROOT_ADDR
    STA CURRENT_PTR
    JMP INSERT_LOOP

INIT_ROOT:
    LDA NUM_NODES
    STA ROOT_ADDR
    LDA ROOT_ADDR
    STA CURRENT_PTR
    LDA ZERO
    STA NODE_DATA
    LDA ZERO
    STA NODE_DATA+1
    LDA ZERO
    STA NODE_DATA+2
    LDA ZERO
    STA NODE_DATA+3
    LDA ZERO
    STA NODE_DATA+4
    LDA ZERO
    STA NODE_DATA+5
    LDA ZERO
    STA NODE_DATA+6
    LDA ZERO
    STA NODE_DATA+7
    LDA ZERO
    STA NODE_DATA+8
    LDA ZERO
    STA NODE_DATA+9
    JMP INSERT_LOOP

INSERT_LOOP:
    LDA WORD_INPUT
    BRZ INSERT_DONE     ; If end of word, mark as end
    LDA WORD_INPUT
    STA CHAR_TO_INSERT
    LDA CHAR_TO_INSERT
    SUB ONE
    STA CHAR_INDEX      ; Convert char to index (0-25)
    
    ; Check if child exists
    LDA CURRENT_PTR
    LDA NODE_DATA+2     ; Load first child pointer
    BRZ CREATE_CHILD    ; If no child, create new one
    LDA NODE_DATA+2
    STA CURRENT_PTR
    JMP INSERT_LOOP

CREATE_CHILD:
    ; Allocate new node
    LDA NUM_NODES
    SUB ONE
    STA NEW_NODE_ADDR
    LDA NEW_NODE_ADDR
    STA NODE_DATA
    LDA ZERO
    STA NODE_DATA+1
    LDA ZERO
    STA NODE_DATA+2
    LDA ZERO
    STA NODE_DATA+3
    LDA ZERO
    STA NODE_DATA+4
    LDA ZERO
    STA NODE_DATA+5
    LDA ZERO
    STA NODE_DATA+6
    LDA ZERO
    STA NODE_DATA+7
    LDA ZERO
    STA NODE_DATA+8
    LDA ZERO
    STA NODE_DATA+9
    LDA NEW_NODE_ADDR
    STA CURRENT_PTR
    LDA CHAR_TO_INSERT
    STA NODE_DATA
    JMP INSERT_LOOP

INSERT_DONE:
    LDA CURRENT_PTR
    LDA ONE
    STA NODE_DATA+1     ; Mark as end of word
    JMP END_PROGRAM

; Search function
SEARCH:
    LDA ROOT_ADDR
    BRZ SEARCH_FAIL     ; If no root, fail
    LDA ROOT_ADDR
    STA CURRENT_PTR
    JMP SEARCH_LOOP

SEARCH_LOOP:
    LDA WORD_INPUT
    BRZ SEARCH_SUCCESS  ; If end of word, success
    LDA WORD_INPUT
    STA CHAR_TO_SEARCH
    LDA CHAR_TO_SEARCH
    SUB ONE
    STA CHAR_INDEX
    
    ; Find child node
    LDA CURRENT_PTR
    LDA NODE_DATA+2     ; Load first child pointer
    BRZ SEARCH_FAIL     ; If no child, fail
    LDA NODE_DATA+2
    STA CURRENT_PTR
    JMP SEARCH_LOOP

SEARCH_SUCCESS:
    LDA CURRENT_PTR
    LDA NODE_DATA+1
    BRZ SEARCH_FAIL     ; If not marked as end, fail
    LDA ONE
    STA FOUND_RESULT
    JMP END_SEARCH

SEARCH_FAIL:
    LDA ZERO
    STA FOUND_RESULT

END_SEARCH:
    LDA FOUND_RESULT
    OUT
    JMP END_PROGRAM

; End of program
END_PROGRAM:
    HLT
    DAT 0

; Data constants
WORD_INPUT  DAT 0
CHAR_TO_INSERT DAT 0
CHAR_INDEX  DAT 0
NEW_NODE_ADDR DAT 0
FOUND_RESULT DAT 0
ZERO        DAT 0
ONE         DAT 1
NODE_DATA   DAT 0
```

## Key Features of this LMC Trie Implementation:

1. **Node Structure**: Each node stores:
   - Character (letter)
   - End-of-word flag
   - Pointers to 10 children (for each letter a-z)

2. **Main Operations**:
   - **Insert**: Adds words to the trie structure
   - **Search**: Checks if a word exists in the trie

3. **Memory Management**:
   - Uses a fixed-size array for nodes
   - Dynamic allocation of new nodes as needed

4. **Algorithm Steps**:
   - Traverse the trie character by character
   - Create new nodes when needed
   - Mark end-of-words with the IsEnd flag

This implementation demonstrates how trie algorithms can be adapted for the limited memory and instruction set of LMC, showing the fundamental concepts of prefix trees in a constrained environment.

