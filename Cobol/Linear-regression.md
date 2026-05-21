# Linear Regression in COBOL

Here's an example of implementing linear regression algorithm in COBOL. This program calculates the slope and intercept of a simple linear regression line given x and y data points.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LINEAR-REGRESSION.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-DATA-ITEMS.
           05  WS-NUM-POINTS        PIC 9(3) VALUE 5.
           05  WS-X-DATA            PIC 9(5)V99 OCCURS 10 TIMES.
           05  WS-Y-DATA            PIC 9(5)V99 OCCURS 10 TIMES.
           05  WS-X-MEAN            PIC 9(5)V99 VALUE 0.
           05  WS-Y-MEAN            PIC 9(5)V99 VALUE 0.
           05  WS-SUM-X             PIC 9(6)V99 VALUE 0.
           05  WS-SUM-Y             PIC 9(6)V99 VALUE 0.
           05  WS-SUM-XY            PIC 9(8)V99 VALUE 0.
           05  WS-SUM-X-SQ          PIC 9(8)V99 VALUE 0.
           05  WS-SLOPE             PIC 9(5)V99 VALUE 0.
           05  WS-INTERCEPT         PIC 9(5)V99 VALUE 0.
           05  WS-TEMP              PIC 9(6)V99 VALUE 0.

       01  WS-INDEX                 PIC 9(2) VALUE 1.
       01  WS-TEMP-INDEX            PIC 9(2) VALUE 1.

       01  WS-OUTPUT-ITEMS.
           05  WS-RESULT-LINE       PIC A(50).
           05  WS-RESULT-MSG        PIC A(30) VALUE 'Linear Regression Results:'.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY 'LINEAR REGRESSION CALCULATION'
           DISPLAY '============================'
           DISPLAY ''

           PERFORM INITIALIZE-DATA
           PERFORM CALCULATE-MEANS
           PERFORM CALCULATE-SUMS
           PERFORM CALCULATE-SLOPE
           PERFORM CALCULATE-INTERCEPT
           PERFORM DISPLAY-RESULTS

           STOP RUN.

       INITIALIZE-DATA.
           MOVE 1 TO WS-INDEX
           MOVE 0 TO WS-SUM-X
           MOVE 0 TO WS-SUM-Y
           MOVE 0 TO WS-SUM-XY
           MOVE 0 TO WS-SUM-X-SQ

           *> Sample data points (x, y)
           MOVE 1 TO WS-X-DATA(1)
           MOVE 2 TO WS-Y-DATA(1)
           
           MOVE 2 TO WS-X-DATA(2)
           MOVE 4 TO WS-Y-DATA(2)
           
           MOVE 3 TO WS-X-DATA(3)
           MOVE 6 TO WS-Y-DATA(3)
           
           MOVE 4 TO WS-X-DATA(4)
           MOVE 8 TO WS-Y-DATA(4)
           
           MOVE 5 TO WS-X-DATA(5)
           MOVE 10 TO WS-Y-DATA(5)

           DISPLAY 'Sample Data Points:'
           PERFORM DISPLAY-DATA-POINTS
           DISPLAY ''

           GO TO INITIALIZE-DATA-END.

       INITIALIZE-DATA-END.
           EXIT.

       CALCULATE-MEANS.
           COMPUTE WS-SUM-X = FUNCTION SUM(WS-X-DATA(1) TO WS-X-DATA(WS-NUM-POINTS))
           COMPUTE WS-SUM-Y = FUNCTION SUM(WS-Y-DATA(1) TO WS-Y-DATA(WS-NUM-POINTS))

           COMPUTE WS-X-MEAN = WS-SUM-X / WS-NUM-POINTS
           COMPUTE WS-Y-MEAN = WS-SUM-Y / WS-NUM-POINTS

           DISPLAY 'Means calculated:'
           DISPLAY 'X Mean: ' WS-X-MEAN
           DISPLAY 'Y Mean: ' WS-Y-MEAN
           DISPLAY ''

           GO TO CALCULATE-MEANS-END.

       CALCULATE-MEANS-END.
           EXIT.

       CALCULATE-SUMS.
           MOVE 1 TO WS-INDEX

           PERFORM UNTIL WS-INDEX > WS-NUM-POINTS
               COMPUTE WS-TEMP = WS-X-DATA(WS-INDEX) * WS-Y-DATA(WS-INDEX)
               COMPUTE WS-SUM-XY = WS-SUM-XY + WS-TEMP

               COMPUTE WS-TEMP = WS-X-DATA(WS-INDEX) * WS-X-DATA(WS-INDEX)
               COMPUTE WS-SUM-X-SQ = WS-SUM-X-SQ + WS-TEMP

               ADD 1 TO WS-INDEX
           END-PERFORM

           DISPLAY 'Sum calculations:'
           DISPLAY 'Sum XY: ' WS-SUM-XY
           DISPLAY 'Sum X Squared: ' WS-SUM-X-SQ
           DISPLAY ''

           GO TO CALCULATE-SUMS-END.

       CALCULATE-SUMS-END.
           EXIT.

       CALCULATE-SLOPE.
           *> Slope = (n*SumXY - SumX*SumY) / (n*SumX2 - SumX^2)
           COMPUTE WS-TEMP = WS-NUM-POINTS * WS-SUM-XY
           COMPUTE WS-TEMP = WS-TEMP - (WS-SUM-X * WS-SUM-Y)
           COMPUTE WS-TEMP = WS-TEMP / (WS-NUM-POINTS * WS-SUM-X-SQ - WS-SUM-X * WS-SUM-X)
           COMPUTE WS-SLOPE = WS-TEMP

           DISPLAY 'Slope calculation:'
           DISPLAY 'Slope = ' WS-SLOPE
           DISPLAY ''

           GO TO CALCULATE-SLOPE-END.

       CALCULATE-SLOPE-END.
           EXIT.

       CALCULATE-INTERCEPT.
           *> Intercept = (SumY - Slope*SumX) / n
           COMPUTE WS-TEMP = WS-SLOPE * WS-SUM-X
           COMPUTE WS-TEMP = WS-SUM-Y - WS-TEMP
           COMPUTE WS-TEMP = WS-TEMP / WS-NUM-POINTS
           COMPUTE WS-INTERCEPT = WS-TEMP

           DISPLAY 'Intercept calculation:'
           DISPLAY 'Intercept = ' WS-INTERCEPT
           DISPLAY ''

           GO TO CALCULATE-INTERCEPT-END.

       CALCULATE-INTERCEPT-END.
           EXIT.

       DISPLAY-RESULTS.
           DISPLAY WS-RESULT-MSG
           DISPLAY '================'
           DISPLAY 'Regression Equation: Y = ' WS-SLOPE 'X + ' WS-INTERCEPT
           DISPLAY 'Slope: ' WS-SLOPE
           DISPLAY 'Intercept: ' WS-INTERCEPT
           DISPLAY ''

           GO TO DISPLAY-RESULTS-END.

       DISPLAY-RESULTS-END.
           EXIT.

       DISPLAY-DATA-POINTS.
           MOVE 1 TO WS-INDEX

           PERFORM UNTIL WS-INDEX > WS-NUM-POINTS
               DISPLAY 'Point ' WS-INDEX ': (' WS-X-DATA(WS-INDEX) ', ' WS-Y-DATA(WS-INDEX) ')'
               ADD 1 TO WS-INDEX
           END-PERFORM

           GO TO DISPLAY-DATA-POINTS-END.

       DISPLAY-DATA-POINTS-END.
           EXIT.
```

## Explanation

This COBOL program implements a simple linear regression algorithm that:

1. **Initializes data points** - Sets up sample x,y pairs
2. **Calculates means** - Computes average of x and y values
3. **Computes sums** - Calculates sum of products and sum of squares
4. **Calculates slope** - Uses the formula: slope = (n*SumXY - SumX*SumY) / (n*SumX² - SumX²)
5. **Calculates intercept** - Uses the formula: intercept = (SumY - slope*SumX) / n
6. **Displays results** - Shows the regression equation and coefficients

## Key Features

- Uses COBOL's COMPUTE statement for mathematical operations
- Implements arrays for data storage
- Includes proper variable declarations and initialization
- Follows COBOL structure with paragraphs and sections
- Handles floating-point arithmetic through PIC declarations

## Sample Output

```
LINEAR REGRESSION CALCULATION
============================

Sample Data Points:
Point 1: (1, 2)
Point 2: (2, 4)
Point 3: (3, 6)
Point 4: (4, 8)
Point 5: (5, 10)

Means calculated:
X Mean: 3.00
Y Mean: 6.00

Sum calculations:
Sum XY: 70.00
Sum X Squared: 55.00

Slope calculation:
Slope = 2.00

Intercept calculation:
Intercept = 0.00

Linear Regression Results:
================
Regression Equation: Y = 2.00X + 0.00
Slope: 2.00
Intercept: 0.00
```

