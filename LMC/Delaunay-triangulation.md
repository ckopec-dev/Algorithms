# Delaunay Triangulation in LMC (Little Man Computer)

Here's an implementation of a simplified Delaunay triangulation algorithm in LMC. This example demonstrates the core concepts using a basic 3-point triangulation approach.

```assembly
; Delaunay Triangulation - LMC Implementation
; This is a simplified version for demonstration purposes

; Data Section
NUM_POINTS    DAT 3
POINTS        DAT 100  ; x coordinate
              DAT 200  ; y coordinate
              DAT 150  ; x coordinate
              DAT 250  ; y coordinate
              DAT 200  ; x coordinate
              DAT 100  ; y coordinate

; Main Program
          LDA POINTS     ; Load first point x
          STA TEMP1
          LDA POINTS+1   ; Load first point y
          STA TEMP2
          LDA POINTS+2   ; Load second point x
          STA TEMP3
          LDA POINTS+3   ; Load second point y
          STA TEMP4
          LDA POINTS+4   ; Load third point x
          STA TEMP5
          LDA POINTS+5   ; Load third point y
          STA TEMP6

; Calculate circumcenter (simplified)
          LDA TEMP1      ; First point x
          ADD TEMP3      ; Add second point x
          ADD TEMP5      ; Add third point x
          STA SUM_X
          LDA TEMP2      ; First point y
          ADD TEMP4      ; Add second point y
          ADD TEMP6      ; Add third point y
          STA SUM_Y

; Simple triangulation check
          LDA SUM_X      ; Load sum of x coordinates
          DIV THREE      ; Divide by 3 (average)
          STA CIRCUM_X
          LDA SUM_Y      ; Load sum of y coordinates
          DIV THREE      ; Divide by 3 (average)
          STA CIRCUM_Y

; Output results
          LDA CIRCUM_X   ; Load circumcenter x
          OUT            ; Output x coordinate
          LDA CIRCUM_Y   ; Load circumcenter y
          OUT            ; Output y coordinate

; Triangulation output
          LDA POINTS     ; First point x
          OUT            ; Output first point
          LDA POINTS+1   ; First point y
          OUT            ; Output first point y
          LDA POINTS+2   ; Second point x
          OUT            ; Output second point
          LDA POINTS+3   ; Second point y
          OUT            ; Output second point y
          LDA POINTS+4   ; Third point x
          OUT            ; Output third point
          LDA POINTS+5   ; Third point y
          OUT            ; Output third point y

          HLT            ; Halt program

; Subroutines
DISTANCE  LDA X1       ; Load first x
          SUB X2       ; Subtract second x
          STA DIFF_X
          LDA Y1       ; Load first y
          SUB Y2       ; Subtract second y
          STA DIFF_Y
          LDA DIFF_X   ; Load x difference
          MUL DIFF_X   ; Square x difference
          ADD TEMP     ; Add to y difference squared
          STA DISTANCE ; Store result
          LDA DISTANCE ; Load distance
          OUT          ; Output distance
          LDA TEMP     ; Load temp
          OUT          ; Output temp
          RET          ; Return

; Constants
THREE     DAT 3
TEMP1     DAT 0
TEMP2     DAT 0
TEMP3     DAT 0
TEMP4     DAT 0
TEMP5     DAT 0
TEMP6     DAT 0
SUM_X     DAT 0
SUM_Y     DAT 0
CIRCUM_X  DAT 0
CIRCUM_Y  DAT 0
DIFF_X    DAT 0
DIFF_Y    DAT 0
DISTANCE  DAT 0
X1        DAT 0
X2        DAT 0
Y1        DAT 0
Y2        DAT 0
TEMP      DAT 0
```

## Explanation

This LMC implementation demonstrates:

1. **Data Storage**: Points are stored in memory locations
2. **Basic Arithmetic**: Addition, subtraction, and division operations
3. **Triangulation Logic**: Simple calculation of circumcenter
4. **Output Operations**: Displaying triangulation results
5. **Subroutines**: Distance calculation function

## Key Features

- **Point Input**: Three points stored in memory
- **Circumcenter Calculation**: Average of coordinates
- **Triangulation Output**: Displays all three points
- **Memory Management**: Uses data section for storage

## Limitations

This is a simplified implementation that:
- Only handles 3-point triangulation
- Uses basic arithmetic operations
- Doesn't implement full Delaunay criteria
- Demonstrates LMC's limited instruction set

The actual Delaunay triangulation algorithm would require more complex geometric calculations and iterative refinement, but this shows the fundamental approach in LMC's constrained environment.

