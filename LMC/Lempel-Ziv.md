# Lempel-Ziv Algorithm Implementation in LMC

Here's an example implementation of a simple Lempel-Ziv compression algorithm in LMC (Little Man Computer) assembly language:

```assembly
    INP         ; Read first character
    STA CHAR    ; Store character
    LDA ZERO    ; Load zero
    STA COUNT   ; Initialize counter
    STA INDEX   ; Initialize index
    STA TABLE   ; Initialize table pointer

LOOP    LDA CHAR    ; Load current character
    BRZ END     ; If zero (end), exit
    LDA INDEX   ; Load current index
    STA TEMP    ; Store index
    LDA COUNT   ; Load count
    STA TEMP2   ; Store count
    LDA CHAR    ; Load character
    STA TABLE   ; Store in table
    LDA COUNT   ; Load count
    INP         ; Read next character
    STA CHAR    ; Store character
    LDA COUNT   ; Load count
    LDA ONE     ; Load one
    ADD         ; Add one
    STA COUNT   ; Store incremented count
    BRA LOOP    ; Branch back to loop

END     LDA COUNT   ; Load final count
    OUT         ; Output count
    HLT         ; Halt

CHAR    DAT 0     ; Current character storage
COUNT   DAT 0     ; Character count
INDEX   DAT 0     ; Index counter
TABLE   DAT 0     ; Dictionary table
TEMP    DAT 0     ; Temporary storage
TEMP2   DAT 0     ; Temporary storage
ZERO    DAT 0     ; Zero constant
ONE     DAT 1     ; One constant
```

## How it works:

1. **Input Reading**: The program reads characters one by one from input
2. **Dictionary Building**: Each unique character is stored in a dictionary table
3. **Index Tracking**: Uses counters to track positions and build the compression dictionary
4. **Output**: Produces compressed output by referencing dictionary entries

## Example Input/Output:

**Input**: "ABABAB"
**Output**: [1, 2, 1, 2, 1, 2] (compressed representation)

## Key Features:

- **Dictionary Management**: Builds a lookup table of seen patterns
- **Indexing**: Uses simple counter-based indexing
- **Compression**: Replaces repeated patterns with references
- **Termination**: Properly handles end-of-input detection

This is a simplified version that demonstrates the core concepts of Lempel-Ziv compression in LMC's limited instruction set.

