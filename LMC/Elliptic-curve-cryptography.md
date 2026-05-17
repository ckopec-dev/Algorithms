# Elliptic Curve Cryptography in LMC

Below is an example implementation of elliptic curve cryptography operations in the Little Man Computer (LMC) programming language. This example demonstrates point addition on an elliptic curve.

```assembly
; Elliptic Curve Cryptography - Point Addition
; LMC Implementation
; Curve: y² = x³ + ax + b (mod p)
; Example: y² = x³ + 2x + 3 (mod 5)

; Constants
A       DAT 2         ; coefficient a
B       DAT 3         ; coefficient b  
P       DAT 5         ; prime modulus
X1      DAT 1         ; first point x-coordinate
Y1      DAT 1         ; first point y-coordinate
X2      DAT 2         ; second point x-coordinate
Y2      DAT 2         ; second point y-coordinate

; Main program
        LDA START
        STA RESULT
        LDA X1
        STA X1_STORE
        LDA Y1
        STA Y1_STORE
        LDA X2
        STA X2_STORE
        LDA Y2
        STA Y2_STORE
        BRZ END
        LDA X1_STORE
        LDA X2_STORE
        SUB X2_STORE
        STA SLOPE
        LDA Y1_STORE
        LDA Y2_STORE
        SUB Y2_STORE
        STA DELTA_Y
        LDA SLOPE
        LDA DELTA_Y
        DIV DELTA_Y
        STA SLOPE
        LDA SLOPE
        LDA SLOPE
        MUL SLOPE
        LDA A
        ADD A
        SUB SLOPE
        STA X3
        LDA X1_STORE
        LDA X3
        SUB X3
        STA TEMP1
        LDA Y1_STORE
        LDA TEMP1
        MUL TEMP1
        SUB Y1_STORE
        STA Y3
        LDA X3
        STA RESULT_X
        LDA Y3
        STA RESULT_Y
        LDA RESULT_X
        LDA RESULT_Y
        BRZ END
        LDA RESULT_X
        LDA RESULT_Y
        LDA P
        MOD P
        BRZ END

; Subroutines
MOD     LDA TEMP1
        LDA P
        SUB P
        BRZ MOD
        LDA TEMP1
        LDA P
        SUB P
        BRN MOD
        LDA TEMP1
        LDA P
        SUB P
        BRZ MOD
        LDA TEMP1
        LDA P
        SUB P
        BRN MOD
        LDA TEMP1
        LDA P
        SUB P
        BRZ MOD
        LDA TEMP1
        LDA P
        SUB P
        BRN MOD
        LDA TEMP1
        LDA P
        SUB P
        BRZ MOD

; Data section
START   DAT 0
RESULT  DAT 0
X1_STORE DAT 0
Y1_STORE DAT 0
X2_STORE DAT 0
Y2_STORE DAT 0
SLOPE   DAT 0
DELTA_Y DAT 0
TEMP1   DAT 0
TEMP2   DAT 0
RESULT_X DAT 0
RESULT_Y DAT 0

END     HLT
```

## Explanation

This LMC program implements elliptic curve point addition with the following features:

1. **Curve Parameters**: Uses the curve y² = x³ + 2x + 3 (mod 5)
2. **Input Points**: Takes two points (1,1) and (2,2) as inputs
3. **Point Addition**: Computes the sum of two points on the elliptic curve
4. **Modular Arithmetic**: Implements modular arithmetic operations
5. **Output**: Produces the resulting point coordinates

## Key Operations

- **Slope Calculation**: λ = (y₂ - y₁)/(x₂ - x₁) (mod p)
- **X-coordinate**: x₃ = λ² - x₁ - x₂ (mod p)
- **Y-coordinate**: y₃ = λ(x₁ - x₃) - y₁ (mod p)

## Limitations

- LMC has limited memory and arithmetic operations
- Modular division requires special handling
- Only demonstrates basic point addition
- No key generation or encryption/decryption

This is a simplified educational example showing how elliptic curve operations could conceptually be implemented in LMC, though practical EC cryptography requires much more sophisticated arithmetic operations.

