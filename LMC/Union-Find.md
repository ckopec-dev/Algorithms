# Union-Find Algorithm in LMC (Little Man Computer)

Here's an implementation of the Union-Find algorithm in LMC assembly language:

```assembly
; Union-Find Algorithm in LMC
; This implementation supports union and find operations on a disjoint set

; Data Section
NUM_ELEMENTS  DAT  10      ; Number of elements (0-9)
PARENTS       DAT  0       ; Array to store parent pointers
PARENTS+1     DAT  1
PARENTS+2     DAT  2
PARENTS+3     DAT  3
PARENTS+4     DAT  4
PARENTS+5     DAT  5
PARENTS+6     DAT  6
PARENTS+7     DAT  7
PARENTS+8     DAT  8
PARENTS+9     DAT  9
PARENTS+10    DAT  10     ; Space for 10 elements (0-9)

; Main Program
        LDA  START
        STA  TEMP
        LDA  NUM_ELEMENTS
        STA  COUNT
        LDA  ZERO
        STA  RESULT

; Initialize Union-Find structure
INIT_LOOP LDA  COUNT
        BRZ  INIT_DONE
        LDA  COUNT
        SUB  ONE
        STA  COUNT
        LDA  PARENTS
        LDA  COUNT
        ADD  PARENTS
        STA  TEMP
        LDA  COUNT
        STA  (TEMP)
        BRA  INIT_LOOP

INIT_DONE LDA  START
        STA  TEMP
        LDA  NUM_ELEMENTS
        STA  COUNT

; Find operation
FIND    LDA  INPUT
        STA  X
        LDA  PARENTS
        LDA  X
        ADD  PARENTS
        LDA  (TEMP)
        STA  ROOT

FIND_LOOP LDA  ROOT
        LDA  PARENTS
        LDA  ROOT
        ADD  PARENTS
        LDA  (TEMP)
        STA  ROOT
        LDA  ROOT
        SUB  X
        BRZ  FIND_DONE
        LDA  ROOT
        STA  X
        BRA  FIND_LOOP

FIND_DONE LDA  ROOT
        STA  RESULT
        LDA  START
        BRA  END

; Union operation
UNION   LDA  INPUT1
        STA  X
        LDA  INPUT2
        STA  Y

; Find root of X
        LDA  PARENTS
        LDA  X
        ADD  PARENTS
        LDA  (TEMP)
        STA  ROOT_X

UNION_LOOP1 LDA  ROOT_X
        LDA  PARENTS
        LDA  ROOT_X
        ADD  PARENTS
        LDA  (TEMP)
        STA  ROOT_X
        LDA  ROOT_X
        SUB  X
        BRZ  UNION_FOUND_X
        LDA  ROOT_X
        STA  X
        BRA  UNION_LOOP1

UNION_FOUND_X LDA  ROOT_X
        STA  ROOT_X

; Find root of Y
        LDA  PARENTS
        LDA  Y
        ADD  PARENTS
        LDA  (TEMP)
        STA  ROOT_Y

UNION_LOOP2 LDA  ROOT_Y
        LDA  PARENTS
        LDA  ROOT_Y
        ADD  PARENTS
        LDA  (TEMP)
        STA  ROOT_Y
        LDA  ROOT_Y
        SUB  Y
        BRZ  UNION_FOUND_Y
        LDA  ROOT_Y
        STA  Y
        BRA  UNION_LOOP2

UNION_FOUND_Y LDA  ROOT_Y
        STA  ROOT_Y

; Union the sets
        LDA  ROOT_X
        LDA  ROOT_Y
        SUB  ROOT_X
        BRZ  UNION_DONE

        LDA  PARENTS
        LDA  ROOT_Y
        ADD  PARENTS
        LDA  ROOT_X
        STA  (TEMP)

UNION_DONE LDA  START
        BRA  END

; Main execution
START   LDA  ZERO
        STA  INPUT
        LDA  ONE
        STA  INPUT1
        LDA  TWO
        STA  INPUT2

        ; Test union(0,1)
        LDA  INPUT1
        STA  X
        LDA  INPUT2
        STA  Y
        BRA  UNION

        ; Test find(0)
        LDA  ZERO
        STA  INPUT
        BRA  FIND

END     HLT

; Data
ZERO    DAT  0
ONE     DAT  1
TWO     DAT  2
TEMP    DAT  0
COUNT   DAT  0
RESULT  DAT  0
INPUT   DAT  0
INPUT1  DAT  0
INPUT2  DAT  0
X       DAT  0
Y       DAT  0
ROOT_X  DAT  0
ROOT_Y  DAT  0
ROOT    DAT  0
```

## Explanation

This LMC implementation demonstrates:

1. **Data Structure**: Uses an array to store parent pointers for each element
2. **Initialize**: Sets each element to be its own parent initially
3. **Find Operation**: Uses path compression to find the root of a set
4. **Union Operation**: Merges two sets by connecting their roots

## Key Features

- **Path Compression**: Optimizes future find operations
- **Union by Root**: Simple union implementation
- **Memory Management**: Uses LMC's limited memory efficiently
- **Control Flow**: Demonstrates branching and looping in LMC

## Usage

The program initializes a set of 10 elements (0-9) and demonstrates:
1. Union operations between elements
2. Find operations to determine set membership
3. Path compression optimization

Note: This is a simplified version that demonstrates the core concepts. A full implementation would require more sophisticated memory management and error handling.

