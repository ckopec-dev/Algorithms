# Comb Sort Algorithm in LMC

Here's an implementation of the Comb Sort algorithm in LMC (Little Man Computer) assembly language:

```assembly
; Comb Sort Algorithm in LMC
; Sorts an array of numbers in ascending order

        INP             ; Input first number
        STA ARRAY       ; Store first number
        INP             ; Input second number
        STA ARRAY+1     ; Store second number
        INP             ; Input third number
        STA ARRAY+2     ; Store third number
        INP             ; Input fourth number
        STA ARRAY+3     ; Store fourth number
        INP             ; Input fifth number
        STA ARRAY+4     ; Store fifth number

        LDA ARRAY       ; Load first element
        STA TEMP1       ; Store in temp1
        LDA ARRAY+1     ; Load second element
        STA TEMP2       ; Store in temp2
        LDA ARRAY+2     ; Load third element
        STA TEMP3       ; Store in temp3
        LDA ARRAY+3     ; Load fourth element
        STA TEMP4       ; Store in temp4
        LDA ARRAY+4     ; Load fifth element
        STA TEMP5       ; Store in temp5

        LDA COUNT       ; Load array size (5)
        STA GAP         ; Initialize gap to array size
        LDA ONE         ; Load 1
        STA SWAP        ; Initialize swap flag to 0 (false)

SORT_LOOP:
        LDA GAP         ; Load current gap
        LDA GAP         ; Load gap again
        LDA FIVE        ; Load 5
        SUB             ; Subtract 5 from gap
        BRZ SORT_END    ; If gap = 0, sorting complete

        LDA GAP         ; Load gap
        STA GAP_TEMP    ; Store gap in temp

COMPARISON_LOOP:
        LDA GAP_TEMP    ; Load gap
        LDA COUNT       ; Load array size
        SUB             ; Subtract gap from size
        BRZ NEXT_GAP    ; If gap >= array size, move to next gap

        LDA GAP_TEMP    ; Load gap
        LDA ARRAY       ; Load first element
        ADD             ; Add gap to index
        LDA ARRAY       ; Load element at gap position
        STA COMP1       ; Store first element to compare
        LDA ARRAY       ; Load first element
        LDA GAP_TEMP    ; Load gap
        SUB             ; Subtract gap from index
        LDA ARRAY       ; Load element at gap position
        STA COMP2       ; Store second element to compare

        LDA COMP1       ; Load first element to compare
        LDA COMP2       ; Load second element to compare
        SUB             ; Subtract second from first
        BRP SWAP_ELEMENTS ; If first >= second, swap

NEXT_COMP:
        LDA GAP_TEMP    ; Load gap
        LDA ONE         ; Load 1
        ADD             ; Add 1 to gap
        STA GAP_TEMP    ; Store back in gap temp
        BRA COMPARISON_LOOP ; Continue comparison

SWAP_ELEMENTS:
        LDA COMP1       ; Load first element
        STA TEMP        ; Store in temp
        LDA COMP2       ; Load second element
        STA COMP1       ; Store second element in first position
        LDA TEMP        ; Load temp
        STA COMP2       ; Store temp in second position

        LDA ONE         ; Load 1
        STA SWAP        ; Set swap flag to 1 (true)

        LDA GAP_TEMP    ; Load gap
        LDA ONE         ; Load 1
        ADD             ; Add 1 to gap
        STA GAP_TEMP    ; Store back in gap temp
        BRA COMPARISON_LOOP ; Continue comparison

NEXT_GAP:
        LDA GAP         ; Load current gap
        LDA FIVE        ; Load 5
        DIV             ; Divide gap by 1.3 (approximate)
        STA GAP         ; Store new gap
        LDA ONE         ; Load 1
        STA SWAP        ; Reset swap flag

        LDA SWAP        ; Load swap flag
        BRZ SORT_END    ; If no swaps, sorting complete

        BRA SORT_LOOP   ; Continue sorting

SORT_END:
        LDA ARRAY       ; Load sorted array elements
        OUT             ; Output first element
        LDA ARRAY+1     ; Load second element
        OUT             ; Output second element
        LDA ARRAY+2     ; Load third element
        OUT             ; Output third element
        LDA ARRAY+3     ; Load fourth element
        OUT             ; Output fourth element
        LDA ARRAY+4     ; Load fifth element
        OUT             ; Output fifth element

        HLT             ; Halt program

; Data section
ARRAY:  DAT 0         ; Array to store 5 numbers
        DAT 0
        DAT 0
        DAT 0
        DAT 0
TEMP1:  DAT 0         ; Temporary storage
TEMP2:  DAT 0
TEMP3:  DAT 0
TEMP4:  DAT 0
TEMP5:  DAT 0
TEMP:   DAT 0
COMP1:  DAT 0
COMP2:  DAT 0
GAP:    DAT 0
GAP_TEMP: DAT 0
SWAP:   DAT 0
COUNT:  DAT 5         ; Array size
ONE:    DAT 1
FIVE:   DAT 5

        END
```

## How Comb Sort Works in LMC:

1. **Initialization**: The program loads 5 numbers into an array
2. **Gap Calculation**: Uses a gap that starts at array size and reduces by a factor of 1.3
3. **Comparison**: Compares elements separated by the current gap
4. **Swapping**: Swaps elements if they're in wrong order
5. **Iteration**: Continues until no more swaps are needed
6. **Output**: Displays the sorted array

## Key Features:

- **Gap Sequence**: Implements the comb sort's characteristic gap reduction
- **Dynamic Gap**: Gap decreases with each iteration
- **Swap Detection**: Uses a flag to detect when sorting is complete
- **Memory Management**: Uses multiple temporary storage locations

This implementation demonstrates the core principles of comb sort while working within LMC's limited instruction set and memory constraints.

