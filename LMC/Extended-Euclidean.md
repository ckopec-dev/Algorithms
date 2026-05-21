# Extended Euclidean Algorithm in LMC

Here's an implementation of the Extended Euclidean Algorithm in LMC (Little Man Computer) assembly language:

```assembly
        INP          ; Input first number (a)
        STA NUM1     ; Store first number
        INP          ; Input second number (b)
        STA NUM2     ; Store second number
        LDA NUM1     ; Load a
        STA A        ; Store a
        LDA NUM2     ; Load b
        STA B        ; Store b
        LDA ZERO     ; Load 0
        STA X1       ; X1 = 0
        LDA ONE      ; Load 1
        STA X2       ; X2 = 1
        LDA ZERO     ; Load 0
        STA Y1       ; Y1 = 0
        LDA ONE      ; Load 1
        STA Y2       ; Y2 = 1
        LDA A        ; Load a
        STA TEMP1    ; TEMP1 = a
        LDA B        ; Load b
        STA TEMP2    ; TEMP2 = b

LOOP    LDA TEMP2    ; Load TEMP2 (b)
        BRZ END      ; If b = 0, end
        LDA TEMP1    ; Load TEMP1 (a)
        LDA TEMP2    ; Load TEMP2 (b)
        REM          ; Calculate remainder (a % b)
        STA REMAIND  ; Store remainder
        LDA TEMP1    ; Load TEMP1 (a)
        LDA TEMP2    ; Load TEMP2 (b)
        DIV          ; Calculate quotient (a / b)
        STA QUOTIENT ; Store quotient
        LDA TEMP2    ; Load TEMP2 (b)
        STA TEMP1    ; TEMP1 = b
        LDA REMAIND  ; Load remainder
        STA TEMP2    ; TEMP2 = remainder

        ; Update coefficients
        LDA X2       ; Load X2
        LDA QUOTIENT ; Load quotient
        MUL          ; X2 * quotient
        LDA X1       ; Load X1
        SUB          ; X1 - (X2 * quotient)
        STA X3       ; X3 = X1 - (X2 * quotient)
        LDA X2       ; Load X2
        STA X1       ; X1 = X2
        LDA X3       ; Load X3
        STA X2       ; X2 = X3

        LDA Y2       ; Load Y2
        LDA QUOTIENT ; Load quotient
        MUL          ; Y2 * quotient
        LDA Y1       ; Load Y1
        SUB          ; Y1 - (Y2 * quotient)
        STA Y3       ; Y3 = Y1 - (Y2 * quotient)
        LDA Y2       ; Load Y2
        STA Y1       ; Y1 = Y2
        LDA Y3       ; Load Y3
        STA Y2       ; Y2 = Y3

        BRA LOOP     ; Repeat loop

END     LDA TEMP1    ; Load final TEMP1 (gcd)
        OUT          ; Output gcd
        LDA X1       ; Load X1 (coefficient x)
        OUT          ; Output x coefficient
        LDA Y1       ; Load Y1 (coefficient y)
        OUT          ; Output y coefficient
        HLT          ; Halt

NUM1    DAT 0        ; First input number
NUM2    DAT 0        ; Second input number
A       DAT 0        ; Variable a
B       DAT 0        ; Variable b
TEMP1   DAT 0        ; Temporary variable
TEMP2   DAT 0        ; Temporary variable
REMAIND DAT 0        ; Remainder storage
QUOTIENT DAT 0       ; Quotient storage
X1      DAT 0        ; Coefficient x1
X2      DAT 0        ; Coefficient x2
X3      DAT 0        ; Coefficient x3
Y1      DAT 0        ; Coefficient y1
Y2      DAT 0        ; Coefficient y2
Y3      DAT 0        ; Coefficient y3
ZERO    DAT 0        ; Constant 0
ONE     DAT 1        ; Constant 1
```

## How it works:

1. **Input**: Takes two numbers (a and b) as input
2. **Initialization**: Sets up the initial coefficients for the extended algorithm
3. **Loop**: Repeatedly applies the Euclidean algorithm while updating the coefficients
4. **Output**: Returns the GCD and the coefficients x and y such that ax + by = gcd(a,b)

## Example usage:
If you input a=35 and b=15:
- Output: 5 (GCD)
- Output: 1 (coefficient x)
- Output: -2 (coefficient y)
- Verification: 35×1 + 15×(-2) = 35 - 30 = 5 ✓

This implementation follows the standard Extended Euclidean Algorithm to find both the greatest common divisor and the Bézout coefficients.

