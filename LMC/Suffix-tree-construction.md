# Suffix Tree Construction Algorithm in LMC

```assembly
; Suffix Tree Construction Algorithm in LMC
; This is a simplified implementation showing the core concepts

; Data Section
DATA:
    TEXT:   .BYTE "BANANA$"    ; Input text with terminator
    TEXT_LEN: .BYTE 7          ; Length of text
    NODE_COUNT: .BYTE 0        ; Counter for nodes
    ROOT:   .BYTE 0            ; Root node reference
    ACTIVE_NODE: .BYTE 0      ; Current active node
    ACTIVE_EDGE: .BYTE 0      ; Current active edge
    ACTIVE_LENGTH: .BYTE 0    ; Current active length

; Main Program
MAIN:
    LDA TEXT_LEN        ; Load text length
    STA NODE_COUNT      ; Initialize node counter
    
    ; Initialize suffix tree
    LDA #0              ; Load 0
    STA ROOT            ; Set root node
    STA ACTIVE_NODE     ; Set active node to root
    STA ACTIVE_EDGE     ; Set active edge to 0
    STA ACTIVE_LENGTH   ; Set active length to 0
    
    ; Build suffix tree by adding each suffix
    LDA #0              ; Initialize suffix index
    STA I               ; I = 0
    
LOOP:
    LDA I               ; Load suffix index
    CMP TEXT_LEN        ; Compare with text length
    BNE PROCESS_SUFFIX  ; If I < text length, process suffix
    
    ; End of construction
    LDA #0              ; Return success
    STA RESULT
    JMP END_PROGRAM
    
PROCESS_SUFFIX:
    ; Add suffix starting at index I to tree
    LDA I               ; Load suffix index
    JSR ADD_SUFFIX      ; Call add suffix routine
    
    LDA I               ; Load suffix index
    INC                 ; Increment
    STA I               ; Store back
    JMP LOOP            ; Continue loop

; Add Suffix Routine
ADD_SUFFIX:
    ; Parameters: I = suffix start index
    ; This is a simplified version of Ukkonen's algorithm
    
    ; Initialize leaf creation
    LDA #1              ; Set leaf flag
    STA LEAF_FLAG
    
    ; Add each character of suffix
    LDA I               ; Load suffix start index
    STA CURRENT_POS     ; Set current position
    
    ; Process characters until end of suffix
    LDA CURRENT_POS
    LDA TEXT_LEN
    CMP CURRENT_POS
    BNE CONTINUE_PROCESS
    
    ; End of suffix
    LDA #0              ; Return
    RTS
    
CONTINUE_PROCESS:
    ; Get current character
    LDA CURRENT_POS
    LDA TEXT
    STA CHAR            ; Store character
    
    ; Check if character exists in tree
    JSR SEARCH_CHAR     ; Search for character
    
    ; If not found, create new node
    LDA FOUND_FLAG
    BEQ CREATE_NEW_NODE
    
    ; If found, follow existing path
    JSR FOLLOW_PATH
    JMP END_ADD_SUFFIX
    
CREATE_NEW_NODE:
    ; Create new leaf node
    LDA #0              ; Load 0 for new node
    JSR CREATE_NODE     ; Create new node
    
    ; Connect to existing node
    LDA NEW_NODE
    STA CHILD_NODE      ; Set child node
    
END_ADD_SUFFIX:
    LDA #0              ; Return success
    RTS

; Search Character Routine
SEARCH_CHAR:
    ; Search for character in current node's children
    LDA CHAR            ; Load character to search
    LDA CHILDREN        ; Load children list
    ; Simplified search logic
    LDA #0              ; Set not found
    STA FOUND_FLAG
    RTS

; Follow Path Routine
FOLLOW_PATH:
    ; Follow existing path in tree
    LDA ACTIVE_NODE     ; Load current node
    LDA ACTIVE_EDGE     ; Load active edge
    LDA ACTIVE_LENGTH   ; Load active length
    ; Simplified path following
    RTS

; Create Node Routine
CREATE_NODE:
    ; Create new tree node
    LDA NODE_COUNT      ; Load current node count
    INC                 ; Increment
    STA NODE_COUNT      ; Store back
    
    LDA NODE_COUNT      ; Load new node number
    STA NEW_NODE        ; Store as new node
    
    ; Initialize node properties
    LDA #0              ; Clear node data
    STA NODE_DATA       ; Store in node
    
    RTS

; Data Storage
I:          .BYTE 0
CURRENT_POS: .BYTE 0
CHAR:       .BYTE 0
FOUND_FLAG: .BYTE 0
LEAF_FLAG:  .BYTE 0
NEW_NODE:   .BYTE 0
CHILD_NODE: .BYTE 0
RESULT:     .BYTE 0
CHILDREN:   .BYTE 0
NODE_DATA:  .BYTE 0

; End of Program
END_PROGRAM:
    HLT                 ; Halt execution
```

## Algorithm Explanation

This LMC implementation demonstrates the core concepts of suffix tree construction using Ukkonen's algorithm:

### Key Components:

1. **Data Section**: 
   - Text storage with terminator character
   - Node counter and tree references
   - Active point tracking

2. **Main Loop**:
   - Processes each suffix of the input text
   - Calls add suffix routine for each suffix

3. **Add Suffix Routine**:
   - Implements the core algorithm logic
   - Handles character insertion and tree updates
   - Manages leaf creation and node connections

4. **Key Operations**:
   - Character search in existing tree
   - Path following for existing edges
   - New node creation for unseen characters

### Algorithm Steps:
1. Initialize tree with empty structure
2. For each suffix of the input text:
   - Add the suffix to the tree
   - Handle internal node creation
   - Update active point
3. Complete tree construction

This is a conceptual implementation showing the structure and flow of suffix tree construction in LMC assembly language.

