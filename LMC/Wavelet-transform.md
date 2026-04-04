# Wavelet Transform Algorithm in LMC (Little Man Computer)

Here's an example implementation of a simple 1D Haar wavelet transform using LMC assembly language:

```assembly
; Haar Wavelet Transform - LMC Implementation
; Transforms a 4-point signal into wavelet coefficients

    INP         ; Input first sample (x[0])
    STA NUM1
    INP         ; Input second sample (x[1])
    STA NUM2
    INP         ; Input third sample (x[2])
    STA NUM3
    INP         ; Input fourth sample (x[3])
    STA NUM4

; Step 1: Calculate averages (low-pass coefficients)
; Average of first two samples
    LDA NUM1
    ADD NUM2
    DIV TWO
    STA AVG1

; Average of last two samples
    LDA NUM3
    ADD NUM4
    DIV TWO
    STA AVG2

; Step 2: Calculate differences (high-pass coefficients)
; Difference of first two samples
    LDA NUM1
    SUB NUM2
    STA DIFF1

; Difference of last two samples
    LDA NUM3
    SUB NUM4
    STA DIFF2

; Output results
    LDA AVG1
    OUT
    LDA AVG2
    OUT
    LDA DIFF1
    OUT
    LDA DIFF2
    OUT

    HLT         ; Halt program

; Data section
NUM1   DAT
NUM2   DAT
NUM3   DAT
NUM4   DAT
AVG1   DAT
AVG2   DAT
DIFF1  DAT
DIFF2  DAT
TWO    DAT 2

; Example usage:
; Input: 10, 6, 8, 4
; Expected output: 8, 6, 4, -4
```

## How it works:

1. **Input**: Takes 4 samples of a signal
2. **Low-pass filtering**: Computes averages of pairs
   - `avg1 = (x[0] + x[1]) / 2`
   - `avg2 = (x[2] + x[3]) / 2`
3. **High-pass filtering**: Computes differences of pairs
   - `diff1 = x[0] - x[1]`
   - `diff2 = x[2] - x[3]`
4. **Output**: Returns the wavelet coefficients

## Key LMC Instructions Used:
- `INP` - Input data
- `STA` - Store accumulator
- `LDA` - Load accumulator
- `ADD` - Addition
- `SUB` - Subtraction
- `DIV` - Division
- `OUT` - Output result
- `HLT` - Halt program

This demonstrates the basic principle of wavelet decomposition in LMC, though it's a simplified 1D Haar transform for educational purposes.

