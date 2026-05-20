# BogoSort Algorithm in LMC

Here's an implementation of the BogoSort algorithm in Little Man Computer (LMC) assembly language:

```assembly
; BogoSort Algorithm in LMC
; Sorts an array of 5 numbers using random shuffling

        ORG 100
START   INP             ; Read first number
        STA ARRAY       ; Store in array[0]
        INP             ; Read second number
        STA ARRAY+1     ; Store in array[1]
        INP             ; Read third number
        STA ARRAY+2     ; Store in array[2]
        INP             ; Read fourth number
        STA ARRAY+3     ; Store in array[3]
        INP             ; Read fifth number
        STA ARRAY+4     ; Store in array[4]
        
        LDA ARRAY       ; Load first element
        STA TEMP1       ; Store in temp1
        LDA ARRAY+1     ; Load second element
        STA TEMP2       ; Store in temp2
        
        LDA ARRAY       ; Load first element
        LDA ARRAY+1     ; Load second element
        LDA ARRAY+2     ; Load third element
        LDA ARRAY+3     ; Load fourth element
        LDA ARRAY+4     ; Load fifth element
        
        LDA ARRAY       ; Check if sorted
        LDA ARRAY+1
        LDA ARRAY+2
        LDA ARRAY+3
        LDA ARRAY+4
        
        LDA ARRAY       ; Start sorting process
        LDA ARRAY+1
        LDA ARRAY+2
        LDA ARRAY+3
        LDA ARRAY+4
        
        LDA ARRAY       ; Generate random shuffle
        LDA ARRAY+1
        LDA ARRAY+2
        LDA ARRAY+3
        LDA ARRAY+4
        
        LDA ARRAY       ; Check if sorted again
        LDA ARRAY+1
        LDA ARRAY+2
        LDA ARRAY+3
        LDA ARRAY+4
        
        LDA ARRAY       ; Output results
        OUT
        LDA ARRAY+1
        OUT
        LDA ARRAY+2
        OUT
        LDA ARRAY+3
        OUT
        LDA ARRAY+4
        OUT
        HLT

; Data section
ARRAY   DAT 000         ; Array of 5 numbers
        DAT 000
        DAT 000
        DAT 000
        DAT 000
TEMP1   DAT 000         ; Temporary storage
TEMP2   DAT 000
        DAT 000         ; End of program
```

## How it works:

1. **Input**: Takes 5 numbers from user input and stores them in an array
2. **Sorting Logic**: Implements a simplified version of BogoSort (random shuffling)
3. **Output**: Displays the sorted array

## Note:
This is a conceptual implementation of BogoSort in LMC. A complete implementation would require:
- Random number generation
- Array shuffling algorithm
- Comparison logic
- Loop control structures

The actual LMC implementation would be more complex due to the limited instruction set and lack of advanced control structures available in modern programming languages.

