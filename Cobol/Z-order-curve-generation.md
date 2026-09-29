# Z-Order Curve Generation Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZORDER-GENERATION.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-X-DIMENSION        PIC 9(4) VALUE 8.
       01  WS-Y-DIMENSION        PIC 9(4) VALUE 8.
       01  WS-GRID-SIZE          PIC 9(6) VALUE 64.
       01  WS-POINT-COUNT        PIC 9(6) VALUE 0.
       01  WS-X-VALUE            PIC 9(4) VALUE 0.
       01  WS-Y-VALUE            PIC 9(4) VALUE 0.
       01  WS-Z-ORDER-VALUE      PIC 9(8) VALUE 0.
       01  WS-LEVEL              PIC 9(2) VALUE 0.
       01  WS-TEMP               PIC 9(8) VALUE 0.
       01  WS-RESULT             PIC 9(8) VALUE 0.

       01  WS-GRID-TABLE.
           05  WS-GRID-RECORDS     OCCURS 64 TIMES.
               10  WS-X-COORD      PIC 9(4).
               10  WS-Y-COORD      PIC 9(4).
               10  WS-Z-ORDER      PIC 9(8).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-GRID
           PERFORM GENERATE-Z-ORDER-CURVE
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-GRID.
           MOVE 0 TO WS-POINT-COUNT
           PERFORM VARYING WS-X-VALUE FROM 0 BY 1 
               UNTIL WS-X-VALUE > WS-X-DIMENSION
               PERFORM VARYING WS-Y-VALUE FROM 0 BY 1
                   UNTIL WS-Y-VALUE > WS-Y-DIMENSION
                   ADD 1 TO WS-POINT-COUNT
                   MOVE WS-X-VALUE TO WS-GRID-RECORDS(WS-POINT-COUNT)
                       (1) 
                   MOVE WS-Y-VALUE TO WS-GRID-RECORDS(WS-POINT-COUNT)
                       (2)
           END-PERFORM.

       GENERATE-Z-ORDER-CURVE.
           PERFORM VARYING WS-POINT-COUNT FROM 1 BY 1
               UNTIL WS-POINT-COUNT > WS-GRID-SIZE
               MOVE WS-GRID-RECORDS(WS-POINT-COUNT)(1) TO WS-X-VALUE
               MOVE WS-GRID-RECORDS(WS-POINT-COUNT)(2) TO WS-Y-VALUE
               PERFORM CALCULATE-Z-ORDER
               MOVE WS-Z-ORDER-VALUE TO WS-GRID-RECORDS(WS-POINT-COUNT)
                   (3)
           END-PERFORM.

       CALCULATE-Z-ORDER.
           MOVE 0 TO WS-Z-ORDER-VALUE
           MOVE 0 TO WS-LEVEL
           PERFORM VARYING WS-LEVEL FROM 0 BY 1 
               UNTIL WS-LEVEL >= 8
               COMPUTE WS-TEMP = (WS-X-VALUE / (2 ** WS-LEVEL)) MOD 2
               COMPUTE WS-RESULT = WS-RESULT + (WS-TEMP * (2 ** (2 * WS-LEVEL)))
               COMPUTE WS-TEMP = (WS-Y-VALUE / (2 ** WS-LEVEL)) MOD 2
               COMPUTE WS-RESULT = WS-RESULT + (WS-TEMP * (2 ** (2 * WS-LEVEL + 1)))
           END-PERFORM
           MOVE WS-RESULT TO WS-Z-ORDER-VALUE.

       DISPLAY-RESULTS.
           DISPLAY "Z-ORDER CURVE GENERATION RESULTS"
           DISPLAY "=================================="
           PERFORM VARYING WS-POINT-COUNT FROM 1 BY 1
               UNTIL WS-POINT-COUNT > WS-GRID-SIZE
               DISPLAY "Point (" WS-GRID-RECORDS(WS-POINT-COUNT)(1)
                       "," WS-GRID-RECORDS(WS-POINT-COUNT)(2)
                       ") -> Z-Order: " WS-GRID-RECORDS(WS-POINT-COUNT)(3)
           END-PERFORM.

       END PROGRAM ZORDER-GENERATION.
```

## Explanation

This COBOL program demonstrates the generation of a Z-order curve (also known as Morton order) for a 2D grid. Here's what it does:

### Key Components:
1. **Data Division**: Defines variables for grid dimensions, coordinates, and Z-order values
2. **Working Storage**: Contains arrays to store grid points and their Z-order mappings
3. **Main Logic**: 
   - Initializes a grid with X,Y coordinates
   - Calculates Z-order values using bit interleaving technique
   - Displays the results showing coordinate-to-Z-order mapping

### Algorithm Steps:
1. **Initialize Grid**: Create a 2D grid of points (8×8 in this example)
2. **Z-Order Calculation**: For each point, interleave bits from X and Y coordinates
3. **Display Results**: Show the mapping between original coordinates and Z-order values

### Bit Interleaving Method:
The algorithm uses bit manipulation to create the Z-order curve by:
- Taking bits from X and Y coordinates alternately
- Constructing a single Z-order value that preserves spatial locality

This approach is commonly used in database indexing, image processing, and spatial data structures where maintaining proximity relationships is important.