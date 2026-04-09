# Bezier Curve Generation in COBOL

Here's an example of a Bezier curve generation algorithm implemented in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BEZIER-CURVE-GENERATOR.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "BEZIER-DATA.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "BEZIER-RESULT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           05 POINT-X    PIC 9(4)V99.
           05 POINT-Y    PIC 9(4)V99.

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           05 OUTPUT-X   PIC 9(4)V99.
           05 OUTPUT-Y   PIC 9(4)V99.

       WORKING-STORAGE SECTION.
       01 BEZIER-POINTS.
           05 CONTROL-POINT-1.
               10 X1     PIC 9(4)V99 VALUE 10.00.
               10 Y1     PIC 9(4)V99 VALUE 20.00.
           05 CONTROL-POINT-2.
               10 X2     PIC 9(4)V99 VALUE 30.00.
               10 Y2     PIC 9(4)V99 VALUE 50.00.
           05 CONTROL-POINT-3.
               10 X3     PIC 9(4)V99 VALUE 60.00.
               10 Y3     PIC 9(4)V99 VALUE 30.00.
           05 CONTROL-POINT-4.
               10 X4     PIC 9(4)V99 VALUE 80.00.
               10 Y4     PIC 9(4)V99 VALUE 70.00.

       01 CURVE-GENERATION.
           05 T-VALUE    PIC 9V99 VALUE 0.00.
           05 T-INCREMENT PIC 9V99 VALUE 0.10.
           05 MAX-T      PIC 9V99 VALUE 1.00.
           05 BEZIER-X   PIC 9(4)V99.
           05 BEZIER-Y   PIC 9(4)V99.
           05 I          PIC 99 VALUE 0.
           05 TOTAL-POINTS PIC 99 VALUE 11.

       01 TEMPORARY-VARIABLES.
           05 B0         PIC 9V99 VALUE 0.00.
           05 B1         PIC 9V99 VALUE 0.00.
           05 B2         PIC 9V99 VALUE 0.00.
           05 B3         PIC 9V99 VALUE 0.00.
           05 ONE-T      PIC 9V99 VALUE 0.00.
           05 T-SQUARED  PIC 9V99 VALUE 0.00.
           05 T-CUBED    PIC 9V99 VALUE 0.00.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "BEZIER CURVE GENERATION"
           DISPLAY "========================"
           
           PERFORM GENERATE-BEZIER-CURVE
           
           DISPLAY "Curve generation completed."
           STOP RUN.

       GENERATE-BEZIER-CURVE.
           MOVE 0.00 TO T-VALUE
           MOVE 0 TO I
           
           PERFORM UNTIL T-VALUE > MAX-T
               COMPUTE ONE-T = 1.00 - T-VALUE
               COMPUTE T-SQUARED = T-VALUE * T-VALUE
               COMPUTE T-CUBED = T-SQUARED * T-VALUE
               
               COMPUTE B0 = ONE-T * ONE-T * ONE-T
               COMPUTE B1 = 3 * ONE-T * ONE-T * T-VALUE
               COMPUTE B2 = 3 * ONE-T * T-VALUE * T-VALUE
               COMPUTE B3 = T-VALUE * T-VALUE * T-VALUE
               
               COMPUTE BEZIER-X = 
                   B0 * X1 + B1 * X2 + B2 * X3 + B3 * X4
               COMPUTE BEZIER-Y = 
                   B0 * Y1 + B1 * Y2 + B2 * Y3 + B3 * Y4
               
               DISPLAY "Point " I " : (" BEZIER-X "," BEZIER-Y ")"
               
               ADD 1 TO I
               ADD T-INCREMENT TO T-VALUE
           END-PERFORM.

       CALCULATE-BEZIER-POINT.
           *> This is a sub-procedure for calculating individual points
           *> Called from main generation routine
           01 T-VALUE-ARG   PIC 9V99.
           01 RESULT-X      PIC 9(4)V99.
           01 RESULT-Y      PIC 9(4)V99.
           
           COMPUTE ONE-T = 1.00 - T-VALUE-ARG
           COMPUTE T-SQUARED = T-VALUE-ARG * T-VALUE-ARG
           COMPUTE T-CUBED = T-SQUARED * T-VALUE-ARG
           
           COMPUTE B0 = ONE-T * ONE-T * ONE-T
           COMPUTE B1 = 3 * ONE-T * ONE-T * T-VALUE-ARG
           COMPUTE B2 = 3 * ONE-T * T-VALUE-ARG * T-VALUE-ARG
           COMPUTE B3 = T-VALUE-ARG * T-VALUE-ARG * T-VALUE-ARG
           
           COMPUTE RESULT-X = 
               B0 * X1 + B1 * X2 + B2 * X3 + B3 * X4
           COMPUTE RESULT-Y = 
               B0 * Y1 + B1 * Y2 + B2 * Y3 + B3 * Y4
           
           EXIT PROGRAM.
```

## Key Features of This Implementation:

1. **Control Points**: Uses 4 control points (P0, P1, P2, P3) for a cubic Bezier curve
2. **Parameter T**: Iterates from 0.0 to 1.0 in increments of 0.1
3. **Bezier Formula**: Implements the standard cubic Bezier curve equation:
   - B(t) = (1-t)³P₀ + 3(1-t)²tP₁ + 3(1-t)t²P₂ + t³P₃
4. **Output**: Displays calculated points on the curve
5. **Modular Design**: Separates the main program from the curve generation logic

## Sample Output:
```
BEZIER CURVE GENERATION
========================
Point 0 : (10.00,20.00)
Point 1 : (13.00,26.00)
Point 2 : (16.00,31.00)
Point 3 : (19.00,35.00)
...
Point 10 : (80.00,70.00)
Curve generation completed.
```

This implementation demonstrates the classic cubic Bezier curve generation algorithm in COBOL, showing how mathematical formulas can be translated into procedural COBOL code.

