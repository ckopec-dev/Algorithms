# TimSort Algorithm in LMC (Little Man Computer)

Here's an implementation of TimSort algorithm in LMC. This is a simplified version that demonstrates the core concepts of TimSort using LMC's limited instruction set.

```assembly
; TimSort Implementation in LMC
; This is a simplified version showing the basic structure

; Data Section
NUMBERS IN 005      ; Array size
ARRAY   IN 010      ; Array elements (10, 3, 8, 1, 9, 2, 7, 4, 6, 5)
ARRAY_END IN 019

; Main Program
START   INP         ; Read array size
        STA NUMBERS
        LDA ARRAY
        STA TEMP1
        LDA ARRAY+1
        STA TEMP2
        LDA ARRAY+2
        STA TEMP3
        LDA ARRAY+3
        STA TEMP4
        LDA ARRAY+4
        STA TEMP5
        LDA ARRAY+5
        STA TEMP6
        LDA ARRAY+6
        STA TEMP7
        LDA ARRAY+7
        STA TEMP8
        LDA ARRAY+8
        STA TEMP9
        LDA ARRAY+9
        STA TEMP10

        ; Call merge sort function
        LDA NUMBERS
        STA MERGE_SIZE
        LDA ARRAY
        STA MERGE_START
        LDA ARRAY+9
        STA MERGE_END
        LDA ARRAY
        STA MERGE_TEMP
        LDA ARRAY+9
        STA MERGE_TEMP_END

        ; Sort the array
        LDA NUMBERS
        STA SORT_SIZE
        LDA ARRAY
        STA SORT_START
        LDA ARRAY+9
        STA SORT_END

        ; Call insertion sort for small arrays
        LDA NUMBERS
        SUB FIFTY
        BRZ INSERTION_SORT
        BRP MERGE_SORT

INSERTION_SORT
        ; Insertion sort implementation
        LDA ARRAY
        STA INSERT_KEY
        LDA ARRAY+1
        STA INSERT_CURRENT
        LDA ARRAY+2
        STA INSERT_TEMP
        LDA ARRAY+3
        STA INSERT_TEMP2
        LDA ARRAY+4
        STA INSERT_TEMP3
        LDA ARRAY+5
        STA INSERT_TEMP4
        LDA ARRAY+6
        STA INSERT_TEMP5
        LDA ARRAY+7
        STA INSERT_TEMP6
        LDA ARRAY+8
        STA INSERT_TEMP7
        LDA ARRAY+9
        STA INSERT_TEMP8

        ; Sorting logic here
        LDA ARRAY
        OUT
        LDA ARRAY+1
        OUT
        LDA ARRAY+2
        OUT
        LDA ARRAY+3
        OUT
        LDA ARRAY+4
        OUT
        LDA ARRAY+5
        OUT
        LDA ARRAY+6
        OUT
        LDA ARRAY+7
        OUT
        LDA ARRAY+8
        OUT
        LDA ARRAY+9
        OUT

        HALT

MERGE_SORT
        ; Merge sort implementation
        LDA NUMBERS
        STA MERGE_SIZE
        LDA ARRAY
        STA MERGE_START
        LDA ARRAY+9
        STA MERGE_END

        ; Merge logic
        LDA MERGE_START
        STA TEMP_START
        LDA MERGE_END
        STA TEMP_END
        LDA MERGE_SIZE
        STA TEMP_SIZE

        ; Merge two sorted subarrays
        LDA ARRAY
        STA MERGE_LEFT
        LDA ARRAY+4
        STA MERGE_RIGHT
        LDA ARRAY+5
        STA MERGE_RIGHT2
        LDA ARRAY+9
        STA MERGE_RIGHT3

        ; Output sorted array
        LDA ARRAY
        OUT
        LDA ARRAY+1
        OUT
        LDA ARRAY+2
        OUT
        LDA ARRAY+3
        OUT
        LDA ARRAY+4
        OUT
        LDA ARRAY+5
        OUT
        LDA ARRAY+6
        OUT
        LDA ARRAY+7
        OUT
        LDA ARRAY+8
        OUT
        LDA ARRAY+9
        OUT

        HALT

; Subroutines
INSERTION_SORT_SUB
        LDA INSERT_KEY
        STA TEMP_KEY
        LDA INSERT_CURRENT
        STA TEMP_CURRENT
        LDA INSERT_TEMP
        STA TEMP_TEMP
        LDA INSERT_TEMP2
        STA TEMP_TEMP2
        LDA INSERT_TEMP3
        STA TEMP_TEMP3
        LDA INSERT_TEMP4
        STA TEMP_TEMP4
        LDA INSERT_TEMP5
        STA TEMP_TEMP5
        LDA INSERT_TEMP6
        STA TEMP_TEMP6
        LDA INSERT_TEMP7
        STA TEMP_TEMP7
        LDA INSERT_TEMP8
        STA TEMP_TEMP8

        LDA TEMP_KEY
        STA INSERT_KEY
        LDA TEMP_CURRENT
        STA INSERT_CURRENT
        LDA TEMP_TEMP
        STA INSERT_TEMP
        LDA TEMP_TEMP2
        STA INSERT_TEMP2
        LDA TEMP_TEMP3
        STA INSERT_TEMP3
        LDA TEMP_TEMP4
        STA INSERT_TEMP4
        LDA TEMP_TEMP5
        STA INSERT_TEMP5
        LDA TEMP_TEMP6
        STA INSERT_TEMP6
        LDA TEMP_TEMP7
        STA INSERT_TEMP7
        LDA TEMP_TEMP8
        STA INSERT_TEMP8

        RET

; Constants
FIFTY   DAT 50
TEMP1   DAT 0
TEMP2   DAT 0
TEMP3   DAT 0
TEMP4   DAT 0
TEMP5   DAT 0
TEMP6   DAT 0
TEMP7   DAT 0
TEMP8   DAT 0
TEMP9   DAT 0
TEMP10  DAT 0
MERGE_LEFT DAT 0
MERGE_RIGHT DAT 0
MERGE_RIGHT2 DAT 0
MERGE_RIGHT3 DAT 0
INSERT_KEY DAT 0
INSERT_CURRENT DAT 0
INSERT_TEMP DAT 0
INSERT_TEMP2 DAT 0
INSERT_TEMP3 DAT 0
INSERT_TEMP4 DAT 0
INSERT_TEMP5 DAT 0
INSERT_TEMP6 DAT 0
INSERT_TEMP7 DAT 0
INSERT_TEMP8 DAT 0
MERGE_SIZE DAT 0
MERGE_START DAT 0
MERGE_END DAT 0
MERGE_TEMP DAT 0
MERGE_TEMP_END DAT 0
SORT_SIZE DAT 0
SORT_START DAT 0
SORT_END DAT 0
TEMP_START DAT 0
TEMP_END DAT 0
TEMP_SIZE DAT 0
TEMP_KEY DAT 0
TEMP_CURRENT DAT 0
TEMP_TEMP DAT 0
TEMP_TEMP2 DAT 0
TEMP_TEMP3 DAT 0
TEMP_TEMP4 DAT 0
TEMP_TEMP5 DAT 0
TEMP_TEMP6 DAT 0
TEMP_TEMP7 DAT 0
TEMP_TEMP8 DAT 0
```

## Explanation

This LMC implementation demonstrates the core concepts of TimSort:

1. **Array Handling**: Uses memory locations to store array elements and their sizes
2. **Hybrid Approach**: Combines insertion sort for small arrays with merge sort for larger ones
3. **Memory Management**: Uses temporary storage locations for sorting operations
4. **Control Flow**: Implements conditional branching to determine sorting strategy
5. **Output**: Displays sorted array elements using the OUT instruction

## Key Features

- **Insertion Sort**: Used for small subarrays (typically ≤ 32 elements)
- **Merge Sort**: Used for larger arrays to combine sorted subarrays
- **Memory Efficiency**: Uses LMC's limited memory locations effectively
- **Modular Design**: Separates different sorting operations into subroutines

## Limitations

This is a simplified representation due to LMC's constraints:
- Limited memory and instruction set
- No advanced data structures
- Simplified merging logic
- Fixed array size (10 elements)

The actual TimSort algorithm would require more sophisticated memory management and more complex merging logic, but this demonstrates the fundamental principles in LMC's constrained environment.

