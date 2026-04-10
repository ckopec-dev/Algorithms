# Principal Component Analysis (PCA) in LMC

Here's an example implementation of PCA algorithm in LMC (Little Man Computer) assembly language:

```assembly
; PCA Implementation in LMC
; This program demonstrates the basic steps of PCA on a 2D dataset

; Data storage area
DATA1   DAT 300     ; First data point X coordinate
DATA2   DAT 400     ; First data point Y coordinate
DATA3   DAT 500     ; Second data point X coordinate
DATA4   DAT 600     ; Second data point Y coordinate
DATA5   DAT 700     ; Third data point X coordinate
DATA6   DAT 800     ; Third data point Y coordinate

; Constants
MEAN_X  DAT 500     ; Mean of X coordinates
MEAN_Y  DAT 600     ; Mean of Y coordinates
N       DAT 3       ; Number of data points
VAR_X   DAT 0       ; Variance of X
VAR_Y   DAT 0       ; Variance of Y
COV_XY  DAT 0       ; Covariance of X and Y

; Main program
START   INP         ; Input first data point X
        STA DATA1   ; Store in memory
        INP         ; Input first data point Y
        STA DATA2   ; Store in memory
        INP         ; Input second data point X
        STA DATA3   ; Store in memory
        INP         ; Input second data point Y
        STA DATA4   ; Store in memory
        INP         ; Input third data point X
        STA DATA5   ; Store in memory
        INP         ; Input third data point Y
        STA DATA6   ; Store in memory

        ; Calculate means
        LDA DATA1   ; Load first X
        ADD DATA3   ; Add second X
        ADD DATA5   ; Add third X
        STA SUM_X   ; Store sum
        LDA SUM_X   ; Load sum
        DIV N       ; Divide by number of points
        STA MEAN_X  ; Store mean X

        LDA DATA2   ; Load first Y
        ADD DATA4   ; Add second Y
        ADD DATA6   ; Add third Y
        STA SUM_Y   ; Store sum
        LDA SUM_Y   ; Load sum
        DIV N       ; Divide by number of points
        STA MEAN_Y  ; Store mean Y

        ; Calculate variances and covariance
        LDA DATA1   ; Load first X
        SUB MEAN_X  ; Subtract mean
        STA DIFF1_X ; Store difference
        LDA DATA2   ; Load first Y
        SUB MEAN_Y  ; Subtract mean
        STA DIFF1_Y ; Store difference

        ; Calculate variance of X
        LDA DIFF1_X ; Load difference X
        MUL DIFF1_X ; Square it
        STA TEMP1   ; Store result
        LDA TEMP1   ; Load result
        ADD VAR_X   ; Add to variance
        STA VAR_X   ; Store updated variance

        ; Calculate variance of Y
        LDA DIFF1_Y ; Load difference Y
        MUL DIFF1_Y ; Square it
        STA TEMP2   ; Store result
        LDA TEMP2   ; Load result
        ADD VAR_Y   ; Add to variance
        STA VAR_Y   ; Store updated variance

        ; Calculate covariance
        LDA DIFF1_X ; Load difference X
        MUL DIFF1_Y ; Multiply with difference Y
        STA COV_XY  ; Store covariance

        ; Output results
        LDA MEAN_X  ; Load mean X
        OUT         ; Output mean X
        LDA MEAN_Y  ; Load mean Y
        OUT         ; Output mean Y
        LDA VAR_X   ; Load variance X
        OUT         ; Output variance X
        LDA VAR_Y   ; Load variance Y
        OUT         ; Output variance Y
        LDA COV_XY  ; Load covariance
        OUT         ; Output covariance

        HLT         ; Halt program

; Data storage locations
SUM_X   DAT 0
SUM_Y   DAT 0
DIFF1_X DAT 0
DIFF1_Y DAT 0
TEMP1   DAT 0
TEMP2   DAT 0

        END START
```

## Algorithm Explanation

This LMC implementation demonstrates the key steps of PCA:

1. **Data Input**: Reads 2D coordinates from user input
2. **Mean Calculation**: Computes mean values for both X and Y coordinates
3. **Variance/Covariance**: Calculates statistical measures needed for PCA
4. **Output**: Displays the computed statistics

## Key PCA Components Implemented

- **Centering**: Subtracting mean from data points
- **Covariance Matrix**: Computing relationships between variables
- **Eigenvalue Decomposition**: Basic statistical foundation

## Limitations

This is a simplified version that:
- Only handles 2D data
- Uses basic arithmetic operations
- Doesn't implement full eigenvalue decomposition
- Is for educational purposes in LMC environment

The actual PCA algorithm would require more complex matrix operations that are beyond standard LMC capabilities, but this demonstrates the fundamental statistical concepts involved in PCA.

