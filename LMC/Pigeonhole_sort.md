# Pigeonhole Sort in Little Man Computer (LMC)

Here's an implementation of pigeonhole sort using LMC. This algorithm sorts an array by placing elements in "pigeonholes" and then collecting them back in order.

```
; Pigeonhole Sort Implementation for LMC
; Sorts an array of numbers from 0 to 9

        ORG 100
START   LDZ COUNT         ; Load count of elements
        STO TEMP1         ; Store count in temp1
        LDZ MAX           ; Load maximum value (assumed 9)
        STO MAXVAL        ; Store maximum value
        LDZ MIN           ; Load minimum value (assumed 0)
        STO MINVAL        ; Store minimum value
        
        ; Initialize pigeonholes array to zero
INIT    LDZ INITCOUNT   ; Load initialization counter
        STO TEMP2         ; Store in temp2
        LDZ PIGEONHOLE    ; Load base address of pigeonhole array
        STO ADDRESS       ; Store address
LOOP1   LDZ ZERO          ; Load zero
        STO ADDRESS       ; Store zero at current address
        ADD ONE           ; Increment address
        STO ADDRESS       ; Store new address
        SUB INITCOUNT     ; Subtract counter from initial count
        BRZ ENDINIT       ; If zero, end initialization
        SUB ONE           ; Decrement counter
        STO INITCOUNT     ; Store updated counter
        BRA LOOP1         ; Branch back to loop

ENDINIT LDZ COUNT         ; Load count of elements
        STO TEMP3         ; Store in temp3
        LDZ ARRAY         ; Load base address of input array
        STO ADDRESS       ; Store in address
        LDZ PIGEONHOLE    ; Load base address of pigeonhole array
        STO PIGEONADDR    ; Store in pigeonhole address

; Main sorting loop - process each element
SORT    LDZ TEMP3         ; Load remaining elements count
        BRZ SORTEND       ; If zero, end sorting
        LDZ ADDRESS       ; Load current array address
        LDA ADDRESS       ; Load element value
        STO VALUE         ; Store value
        ADD ZERO          ; Add offset to get correct pigeonhole index
        STO INDEX         ; Store index
        
        ; Increment pigeonhole count
        LDZ PIGEONADDR    ; Load pigeonhole base address
        ADD INDEX         ; Add index to get specific pigeonhole
        LDA ADDRESS       ; Load current pigeonhole value
        ADD ONE           ; Increment by one
        STO ADDRESS       ; Store incremented value
        
        ; Move to next element in array
        ADD ONE           ; Increment array address
        STO ADDRESS       ; Store new address
        SUB ONE           ; Decrement count
        STO TEMP3         ; Store updated count
        BRA SORT          ; Branch back to sort loop

SORTEND LDZ PIGEONHOLE    ; Load base address of pigeonholes
        STO PIGEONADDR    ; Store in pigeonhole address
        LDZ MINVAL        ; Load minimum value
        STO INDEX         ; Store index
        LDZ ARRAY         ; Load base address of output array
        STO OUTPUTADDR    ; Store in output address

; Collect elements from pigeonholes back to array
COLLECT LDZ INDEX         ; Load current index
        ADD ONE           ; Increment index
        STO INDEX         ; Store updated index
        LDZ PIGEONADDR    ; Load current pigeonhole address
        LDA ADDRESS       ; Load pigeonhole value
        BRZ NEXT          ; If zero, skip this pigeonhole
        
        ; Store the element in output array
        LDZ OUTPUTADDR    ; Load output address
        STO TEMP4         ; Store in temp4
        STO ADDRESS       ; Store value at output address
        ADD ONE           ; Increment output address
        STO OUTPUTADDR    ; Store new output address
        
NEXT    LDZ MAXVAL        ; Load maximum value
        SUB INDEX         ; Subtract current index
        BRZ DONE          ; If zero, we're done
        BRA COLLECT       ; Branch back to collect

DONE    HLT               ; Halt program

; Data Section
COUNT   DAT 5             ; Number of elements to sort (example: 5)
MAX     DAT 9             ; Maximum possible value (0-9)
MIN     DAT 0             ; Minimum possible value (0)
INITCOUNT DAT 10          ; Initialization counter (assuming max 10 elements)
TEMP1   DAT 0             ; Temporary storage
TEMP2   DAT 0             ; Temporary storage
TEMP3   DAT 0             ; Temporary storage
TEMP4   DAT 0             ; Temporary storage
ADDRESS DAT 0             ; General address register
PIGEONADDR DAT 0         ; Pigeonhole array address
OUTPUTADDR DAT 0         ; Output array address
INDEX   DAT 0             ; Index counter
VALUE   DAT 0             ; Current value being processed
MAXVAL  DAT 0             ; Maximum value
MINVAL  DAT 0             ; Minimum value

; Arrays
ARRAY   DAT 3             ; Input array elements (example: 3,1,4,1,5)
        DAT 1
        DAT 4
        DAT 1
        DAT 5
PIGEONHOLE DAT 0        ; Pigeonhole array (size = max - min + 1)
        DAT 0
        DAT 0
        DAT 0
        DAT 0
        DAT 0
        DAT 0
        DAT 0
        DAT 0
        DAT 0
        DAT 0

; Constants
ZERO    DAT 0             ; Zero constant
ONE     DAT 1             ; One constant
```

## How it works:

1. **Initialization**: Sets up the pigeonhole array with zeros
2. **Counting Phase**: Each element from the input array is placed in its corresponding pigeonhole (index = element value)
3. **Collection Phase**: Elements are collected from pigeonholes back to the output array in sorted order

## Example Input/Output:
- **Input Array**: [3, 1, 4, 1, 5]
- **Output Array**: [1, 1, 3, 4, 5]

This implementation assumes a known range of values (0-9) for efficient pigeonhole allocation. The algorithm works by counting occurrences of each value and then reconstructing the sorted array.

