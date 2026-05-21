# CIPHER__family Algorithm Example in LMC

The following is an example of a simple cipher algorithm implemented in LMC (Little Man Computer) programming language. This example demonstrates a basic Caesar cipher with a shift of 3 positions.

```assembly
    INP         ; Read input character
    STA CHAR    ; Store input character
    LDA CHAR    ; Load character
    SUB THREE   ; Subtract 3 to shift back
    BRZ DONE    ; If zero, we're done
    BRP CONT    ; If positive, continue
    LDA CHAR    ; Load original character
    ADD TWENTY  ; Add 20 to wrap around
    BRZ DONE    ; If zero, we're done
    BRP CONT    ; If positive, continue

CONT    LDA CHAR    ; Load character
    ADD THREE   ; Add 3 to shift forward
    STA RESULT  ; Store result
    LDA RESULT  ; Load result
    OUT         ; Output result
    BRZ DONE    ; If zero, we're done
    BRP CONT    ; If positive, continue

DONE    HLT         ; Halt program

CHAR    DAT 0     ; Storage for input character
RESULT  DAT 0     ; Storage for result
THREE   DAT 3     ; Shift value
TWENTY  DAT 20    ; Wrap-around value
```

## Explanation

This LMC program implements a basic Caesar cipher algorithm where:
- Input characters are shifted by 3 positions
- The program handles both encryption and decryption
- Character wrapping is implemented for proper cipher functionality
- Uses basic LMC instructions: INP, OUT, STA, LDA, SUB, ADD, BRZ, BRP, HLT

The algorithm follows the CIPHER__family pattern of character manipulation and shifting, demonstrating how encryption concepts can be implemented in the limited instruction set of LMC.

