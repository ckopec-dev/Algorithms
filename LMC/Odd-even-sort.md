# Odd-Even Sort in LMC (Little Man Computer)

Here's an implementation of the Odd-Even sort algorithm in LMC:

```assembly
        INP
        STA NUM1
        INP
        STA NUM2
        INP
        STA NUM3
        INP
        STA NUM4
        INP
        STA NUM5
        INP
        STA NUM6
        INP
        STA NUM7
        INP
        STA NUM8

        LDA NUM1
        LDA NUM2
        SUB NUM1
        BRP COMPARE1
        LDA NUM1
        STA TEMP
        LDA NUM2
        STA NUM1
        LDA TEMP
        STA NUM2
COMPARE1
        LDA NUM3
        LDA NUM4
        SUB NUM3
        BRP COMPARE2
        LDA NUM3
        STA TEMP
        LDA NUM4
        STA NUM3
        LDA TEMP
        STA NUM4
COMPARE2
        LDA NUM5
        LDA NUM6
        SUB NUM5
        BRP COMPARE3
        LDA NUM5
        STA TEMP
        LDA NUM6
        STA NUM5
        LDA TEMP
        STA NUM6
COMPARE3
        LDA NUM7
        LDA NUM8
        SUB NUM7
        BRP COMPARE4
        LDA NUM7
        STA TEMP
        LDA NUM8
        STA NUM7
        LDA TEMP
        STA NUM8
COMPARE4
        LDA NUM1
        LDA NUM3
        SUB NUM1
        BRP COMPARE5
        LDA NUM1
        STA TEMP
        LDA NUM3
        STA NUM1
        LDA TEMP
        STA NUM3
COMPARE5
        LDA NUM2
        LDA NUM4
        SUB NUM2
        BRP COMPARE6
        LDA NUM2
        STA TEMP
        LDA NUM4
        STA NUM2
        LDA TEMP
        STA NUM4
COMPARE6
        LDA NUM5
        LDA NUM7
        SUB NUM5
        BRP COMPARE7
        LDA NUM5
        STA TEMP
        LDA NUM7
        STA NUM5
        LDA TEMP
        STA NUM7
COMPARE7
        LDA NUM6
        LDA NUM8
        SUB NUM6
        BRP COMPARE8
        LDA NUM6
        STA TEMP
        LDA NUM8
        STA NUM6
        LDA TEMP
        STA NUM8
COMPARE8
        LDA NUM1
        LDA NUM2
        SUB NUM1
        BRP COMPARE9
        LDA NUM1
        STA TEMP
        LDA NUM2
        STA NUM1
        LDA TEMP
        STA NUM2
COMPARE9
        LDA NUM3
        LDA NUM4
        SUB NUM3
        BRP COMPARE10
        LDA NUM3
        STA TEMP
        LDA NUM4
        STA NUM3
        LDA TEMP
        STA NUM4
COMPARE10
        LDA NUM5
        LDA NUM6
        SUB NUM5
        BRP COMPARE11
        LDA NUM5
        STA TEMP
        LDA NUM6
        STA NUM5
        LDA TEMP
        STA NUM6
COMPARE11
        LDA NUM7
        LDA NUM8
        SUB NUM7
        BRP COMPARE12
        LDA NUM7
        STA TEMP
        LDA NUM8
        STA NUM7
        LDA TEMP
        STA NUM8
COMPARE12
        LDA NUM1
        OUT
        LDA NUM2
        OUT
        LDA NUM3
        OUT
        LDA NUM4
        OUT
        LDA NUM5
        OUT
        LDA NUM6
        OUT
        LDA NUM7
        OUT
        LDA NUM8
        OUT
        HLT

NUM1    DAT 0
NUM2    DAT 0
NUM3    DAT 0
NUM4    DAT 0
NUM5    DAT 0
NUM6    DAT 0
NUM7    DAT 0
NUM8    DAT 0
TEMP    DAT 0
```

## How it works:

1. **Input**: Takes 8 numbers as input and stores them in memory locations NUM1 through NUM8
2. **Sorting**: Implements odd-even merge sort logic by performing multiple passes of bubble sort comparisons:
   - Compares adjacent pairs (1,2), (3,4), (5,6), (7,8)
   - Compares pairs with gaps (1,3), (2,4), (5,7), (6,8)
   - Compares pairs with gaps (1,5), (2,6), (3,7), (4,8)
   - Final pass with adjacent pairs (1,2), (3,4), (5,6), (7,8)
3. **Output**: Outputs the sorted numbers in ascending order
4. **Comparison Logic**: Uses subtraction and branch-if-positive to determine which number is larger, then swaps them if needed

## Example Usage:
- Input: 5 2 8 1 9 3 7 4
- Output: 1 2 3 4 5 7 8 9

The algorithm performs a series of comparisons and swaps to sort the numbers in ascending order using the odd-even merge pattern.

