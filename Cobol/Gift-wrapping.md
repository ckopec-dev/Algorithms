# Gift Wrapping Algorithm in COBOL

The gift wrapping algorithm (also known as Jarvis march) is used to find the convex hull of a set of points. Here's a COBOL implementation:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GIFT-WRAPPING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POINTS-FILE ASSIGN TO "POINTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD POINTS-FILE.
       01 POINT-RECORD.
          05 POINT-X    PIC 9(4).
          05 POINT-Y    PIC 9(4).

       WORKING-STORAGE SECTION.
       01 POINT-TABLE.
          05 POINT-ITEM OCCURS 100 TIMES.
             10 X-COORD   PIC 9(4).
             10 Y-COORD   PIC 9(4).
       01 POINT-COUNT PIC 9(3) VALUE 0.
       01 START-POINT.
          05 START-X   PIC 9(4).
          05 START-Y   PIC 9(4).
       01 HULL-POINTS.
          05 HULL-ITEM OCCURS 100 TIMES.
             10 HULL-X   PIC 9(4).
             10 HULL-Y   PIC 9(4).
       01 HULL-COUNT PIC 9(3) VALUE 0.
       01 CURRENT-POINT.
          05 CURRENT-X PIC 9(4).
          05 CURRENT-Y PIC 9(4).
       01 NEXT-POINT.
          05 NEXT-X PIC 9(4).
          05 NEXT-Y PIC 9(4).
       01 ANGLE PIC 9(5)V99 VALUE 0.
       01 MIN-ANGLE PIC 9(5)V99 VALUE 360.
       01 TEMP-ANGLE PIC 9(5)V99 VALUE 0.
       01 INDEX PIC 9(3) VALUE 0.
       01 I PIC 9(3) VALUE 0.
       01 J PIC 9(3) VALUE 0.
       01 K PIC 9(3) VALUE 0.
       01 DONE PIC X VALUE 'N'.
       01 EOF-FLAG PIC X VALUE 'N'.
       01 LINE-OUTPUT PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-PROGRAM.
           PERFORM READ-POINTS.
           PERFORM FIND-CONVEX-HULL.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-PROGRAM.
           MOVE 0 TO POINT-COUNT.
           MOVE 0 TO HULL-COUNT.
           MOVE 'N' TO EOF-FLAG.
           MOVE 'N' TO DONE.

       READ-POINTS.
           OPEN INPUT POINTS-FILE.
           READ POINTS-FILE INTO POINT-RECORD
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ.
           PERFORM UNTIL EOF-FLAG = 'Y'
               ADD 1 TO POINT-COUNT
               MOVE POINT-X TO X-COORD(POINT-COUNT)
               MOVE POINT-Y TO Y-COORD(POINT-COUNT)
               READ POINTS-FILE INTO POINT-RECORD
                   AT END MOVE 'Y' TO EOF-FLAG
               END-READ
           END-PERFORM.
           CLOSE POINTS-FILE.

       FIND-CONVEX-HULL.
           PERFORM FIND-LEFTMOST-POINT.
           MOVE CURRENT-X TO START-X.
           MOVE CURRENT-Y TO START-Y.
           MOVE CURRENT-X TO CURRENT-X.
           MOVE CURRENT-Y TO CURRENT-Y.
           MOVE 1 TO HULL-COUNT.
           MOVE CURRENT-X TO HULL-X(HULL-COUNT).
           MOVE CURRENT-Y TO HULL-Y(HULL-COUNT).

           PERFORM UNTIL DONE = 'Y'
               PERFORM FIND-NEXT-POINT.
               IF CURRENT-X = START-X AND CURRENT-Y = START-Y
                   MOVE 'Y' TO DONE
               ELSE
                   ADD 1 TO HULL-COUNT
                   MOVE CURRENT-X TO HULL-X(HULL-COUNT)
                   MOVE CURRENT-Y TO HULL-Y(HULL-COUNT)
               END-IF
           END-PERFORM.

       FIND-LEFTMOST-POINT.
           MOVE 1 TO INDEX.
           MOVE X-COORD(1) TO CURRENT-X.
           MOVE Y-COORD(1) TO CURRENT-Y.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > POINT-COUNT
               IF X-COORD(I) < CURRENT-X
                   MOVE X-COORD(I) TO CURRENT-X
                   MOVE Y-COORD(I) TO CURRENT-Y
                   MOVE I TO INDEX
               END-IF
           END-PERFORM.

       FIND-NEXT-POINT.
           MOVE 1 TO INDEX.
           MOVE X-COORD(1) TO NEXT-X.
           MOVE Y-COORD(1) TO NEXT-Y.
           MOVE 360 TO MIN-ANGLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF I NOT = INDEX
                   PERFORM CALCULATE-ANGLE.
                   IF TEMP-ANGLE < MIN-ANGLE OR MIN-ANGLE = 360
                       MOVE TEMP-ANGLE TO MIN-ANGLE
                       MOVE X-COORD(I) TO NEXT-X
                       MOVE Y-COORD(I) TO NEXT-Y
                       MOVE I TO INDEX
                   END-IF
               END-IF
           END-PERFORM.
           MOVE NEXT-X TO CURRENT-X.
           MOVE NEXT-Y TO CURRENT-Y.

       CALCULATE-ANGLE.
           COMPUTE TEMP-ANGLE = ATAN2(Y-COORD(I) - CURRENT-Y,
                                     X-COORD(I) - CURRENT-X)
           COMPUTE TEMP-ANGLE = TEMP-ANGLE * 57.2958  (* Convert to degrees *).
           IF TEMP-ANGLE < 0
               COMPUTE TEMP-ANGLE = TEMP-ANGLE + 360
           END-IF.

       DISPLAY-RESULTS.
           DISPLAY "CONVEX HULL POINTS:".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HULL-COUNT
               MOVE HULL-X(I) TO CURRENT-X
               MOVE HULL-Y(I) TO CURRENT-Y
               STRING "Point " I " (" CURRENT-X "," CURRENT-Y ")"
                      INTO LINE-OUTPUT
               DISPLAY LINE-OUTPUT
           END-PERFORM.

       ATAN2-FUNCTION.
           * This is a simplified version - actual implementation
           * would require more complex trigonometric calculations
           MOVE 0 TO TEMP-ANGLE.
```

## Example Input File (POINTS.DAT)
```
1000
2000
3000
4000
5000
```

## Key Features of this Implementation:

1. **Point Reading**: Reads points from an input file
2. **Convex Hull Calculation**: Implements the gift wrapping algorithm
3. **Angle Calculation**: Uses ATAN2 function to calculate angles
4. **Point Tracking**: Maintains hull points in a separate array
5. **Output Display**: Shows the resulting convex hull points

## Algorithm Steps:
1. Find the leftmost point (starting point)
2. For each point in the hull, find the next point by calculating angles
3. Continue until returning to the starting point
4. Output all hull points

This implementation demonstrates the core concepts of the gift wrapping algorithm in COBOL, though it's simplified for clarity and assumes basic trigonometric functions are available.

