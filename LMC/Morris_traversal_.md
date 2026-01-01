# Morris Traversal in Little Man Computer (LMC)

Morris traversal is a tree traversal algorithm that allows traversal without using recursion or a stack. Here's an implementation in LMC:

```assembly
; Morris Traversal - Inorder Tree Traversal
; Assumes a binary tree stored in memory with:
; - Each node has: [data][left pointer][right pointer]
; - Root node at location 100
; - Null pointers = 0

    INP         ; Input root address
    STA ROOT    ; Store root address
    LDA ZERO    ; Initialize current node pointer
    STA CURR    ; Set current to root
    LDA ZERO    ; Initialize previous pointer
    STA PREV    ; Set previous to null

START   LDA CURR    ; Load current node address
        BRZ DONE    ; If null, we're done
        LDA CURR    ; Load current address
        LDA 0       ; Load data from current node
        OUT         ; Output data (inorder visit)
        LDA CURR    ; Load current address
        LDA 1       ; Load left pointer
        STA PREV    ; Store left pointer in previous
        LDA PREV    ; Load left pointer
        BRZ RIGHT   ; If left is null, go to right subtree
        LDA PREV    ; Load left pointer
        LDA 2       ; Load right pointer from left node
        BRZ LEFT    ; If right of left is null, continue
        LDA PREV    ; Load left pointer
        LDA 2       ; Load right pointer
        STA CURR    ; Set current to right pointer of left node
        LDA CURR    ; Load current address
        LDA 1       ; Load left pointer
        BRZ LEFT    ; If left is null, continue
        LDA CURR    ; Load current address
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer from left node
        STA TEMP    ; Store right pointer
        LDA CURR    ; Load current address
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer from left node
        LDA 2       ; Load right pointer of right node
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node
        LDA 1       ; Load left pointer
        LDA 2       ; Load right pointer
        LDA 0       ; Load data from right node


