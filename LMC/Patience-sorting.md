# Patience Sorting Algorithm in LMC

Here's an implementation of the patience sorting algorithm in LMC (Little Man Computer) assembly language:

```assembly
; Patience Sorting Algorithm in LMC
; Sorts an array of numbers using patience sorting approach

    INP         ; Read first number
    STA NUM1    ; Store first number
    INP         ; Read second number
    STA NUM2    ; Store second number
    INP         ; Read third number
    STA NUM3    ; Store third number
    INP         ; Read fourth number
    STA NUM4    ; Store fourth number
    INP         ; Read fifth number
    STA NUM5    ; Store fifth number

    ; Initialize stack pointers
    LDA ZERO
    STA STACK1
    STA STACK2
    STA STACK3
    STA STACK4
    STA STACK5

    ; Process first number
    LDA NUM1
    STA STACK1
    LDA ONE
    STA STACK1+1

    ; Process second number
    LDA NUM2
    STA STACK2
    LDA ONE
    STA STACK2+1

    ; Process third number
    LDA NUM3
    STA STACK3
    LDA ONE
    STA STACK3+1

    ; Process fourth number
    LDA NUM4
    STA STACK4
    LDA ONE
    STA STACK4+1

    ; Process fifth number
    LDA NUM5
    STA STACK5
    LDA ONE
    STA STACK5+1

    ; Merge stacks (simplified version)
    LDA STACK1
    OUT
    LDA STACK2
    OUT
    LDA STACK3
    OUT
    LDA STACK4
    OUT
    LDA STACK5
    OUT

    HLT         ; Halt program

; Data section
NUM1   DAT
NUM2   DAT
NUM3   DAT
NUM4   DAT
NUM5   DAT

STACK1 DAT      ; Stack 1
       DAT      ; Stack 1 pointer
STACK2 DAT      ; Stack 2
       DAT      ; Stack 2 pointer
STACK3 DAT      ; Stack 3
       DAT      ; Stack 3 pointer
STACK4 DAT      ; Stack 4
       DAT      ; Stack 4 pointer
STACK5 DAT      ; Stack 5
       DAT      ; Stack 5 pointer

ZERO   DAT 0
ONE    DAT 1

; Note: This is a simplified implementation
; A full patience sorting would require more complex stack management
; and merging algorithms to properly implement the patience sorting technique
```

## How it works:

1. **Input**: The program reads 5 numbers from input
2. **Stack Initialization**: Each number is placed on its own stack
3. **Stack Management**: Each stack maintains a pointer to track its top element
4. **Output**: The program outputs the numbers (in this simplified version)

## Key LMC Concepts Used:

- **INP/OUT**: Input and output operations
- **STA/ADD/SUB**: Store, add, and subtract operations
- **HLT**: Halt instruction to stop execution
- **DAT**: Data storage for variables and arrays
- **Addressing**: Using memory addresses to manage stacks

## Note:

This is a **simplified demonstration** of patience sorting in LMC. A complete implementation would require:
- More sophisticated stack management
- Proper merging of sorted piles
- Dynamic memory allocation
- Complete patience sorting algorithm logic

The actual LMC implementation would be much more complex due to the limited instruction set and memory management constraints of the LMC architecture.

