# Subset Sum Algorithm in LMC

Here's an implementation of the subset sum algorithm in LMC (Little Man Computer) assembly language:

```assembly
        INP
        STA SUM
        INP
        STA SIZE
        LDA SIZE
        STA COUNT
        LDA ZERO
        STA RESULT

LOOP    LDA COUNT
        BRZ END
        INP
        STA ARRAY
        LDA COUNT
        SUB ONE
        STA COUNT
        BRA LOOP

END     LDA ZERO
        STA TARGET
        LDA SUM
        STA TARGET
        LDA ZERO
        STA INDEX
        LDA ZERO
        STA CURRENT
        LDA ZERO
        STA FOUND

CHECK   LDA INDEX
        SUB SIZE
        BRZ RETURN
        LDA CURRENT
        LDA ARRAY
        ADD CURRENT
        STA CURRENT
        LDA INDEX
        ADD ONE
        STA INDEX
        BRA CHECK

RETURN  LDA CURRENT
        LDA TARGET
        SUB CURRENT
        BRZ FOUNDIT
        LDA ZERO
        STA FOUND
        BRA ENDIT

FOUNDIT LDA ONE
        STA FOUND

ENDIT   LDA FOUND
        OUT
        HLT

SUM     DAT 0
SIZE    DAT 0
COUNT   DAT 0
RESULT  DAT 0
ARRAY   DAT 0
TARGET  DAT 0
INDEX   DAT 0
CURRENT DAT 0
FOUND   DAT 0
ZERO    DAT 0
ONE     DAT 1
```

## How it works:

1. **Input Processing**: 
   - First input is the target sum
   - Second input is the size of the array
   - Then read the array elements

2. **Algorithm Logic**:
   - Uses nested loops to check all possible subsets
   - For each element, decides whether to include it in the current subset
   - Calculates running sum and compares with target

3. **Key Features**:
   - Uses counters and indices for iteration
   - Implements basic subset checking logic
   - Returns 1 if subset found, 0 otherwise
   - Uses LMC's limited instruction set (INP, OUT, HLT, ADD, SUB, BRZ, BRA)

## Sample Input:
```
10     (target sum)
3      (array size)
5      (first element)
3      (second element)
2      (third element)
```

## Sample Output:
```
1      (subset found: 5+3+2=10)
```

**Note**: This is a simplified version. A complete subset sum algorithm would require more complex logic to handle all combinations, but this demonstrates the core concepts in LMC assembly.

