# Egyptian Multiplication in LMC

Here's an implementation of the Egyptian multiplication algorithm in LMC (Little Man Computer) assembly language:

```assembly
        INP
        STA NUM1
        INP
        STA NUM2
        LDA ZERO
        STA RESULT
        LDA NUM1
        STA TEMP1
        LDA NUM2
        STA TEMP2

LOOP    LDA TEMP2
        BRZ END
        LDA TEMP2
        SUB ONE
        STA TEMP2
        LDA RESULT
        ADD TEMP1
        STA RESULT
        LDA TEMP1
        ADD TEMP1
        STA TEMP1
        BRA LOOP

END     LDA RESULT
        OUT
        HLT

NUM1    DAT 0
NUM2    DAT 0
TEMP1   DAT 0
TEMP2   DAT 0
RESULT  DAT 0
ZERO    DAT 0
ONE     DAT 1
```

## How it works:

1. **Input**: Takes two numbers as input (NUM1 and NUM2)
2. **Initialization**: Sets up variables and initializes RESULT to 0
3. **Main Loop**: 
   - Checks if TEMP2 (second number) is zero
   - If not zero, adds TEMP1 (first number) to RESULT
   - Doubles TEMP1 (shift left)
   - Decrements TEMP2 (shift right)
   - Repeats until TEMP2 becomes zero
4. **Output**: Displays the final result

## Example trace (5 × 3):
- Initial: NUM1=5, NUM2=3, RESULT=0, TEMP1=5, TEMP2=3
- Iteration 1: RESULT=0+5=5, TEMP1=10, TEMP2=2
- Iteration 2: RESULT=5+10=15, TEMP1=20, TEMP2=1  
- Iteration 3: RESULT=15+20=35, TEMP1=40, TEMP2=0
- Output: 35

This implements the ancient Egyptian method of multiplication using repeated addition and bit shifting.

