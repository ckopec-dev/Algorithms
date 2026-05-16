# Graham Scan Algorithm in COBOL

Here's a complete implementation of the Graham Scan algorithm for finding the convex hull of a set of points in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRAHAM-SCAN.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "POINTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "HULL.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 X-COORD           PIC 9(4).
          05 Y-COORD           PIC 9(4).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 OUTPUT-X          PIC 9(4).
          05 OUTPUT-Y          PIC 9(4).

       WORKING-STORAGE SECTION.
       01 POINTS.
          05 POINT-ARRAY       OCCURS 100 TIMES.
             10 POINT-X        PIC 9(4).
             10 POINT-Y        PIC 9(4).
       01 STACK.
          05 STACK-ARRAY       OCCURS 100 TIMES.
             10 STACK-X        PIC 9(4).
             10 STACK-Y        PIC 9(4).
       01 STACK-INDEX         PIC 9(3) VALUE 0.
       01 POINT-COUNT         PIC 9(3) VALUE 0.
       01 START-POINT.
          05 START-X          PIC 9(4) VALUE 0.
          05 START-Y          PIC 9(4) VALUE 0.
       01 TEMP-X              PIC 9(4) VALUE 0.
       01 TEMP-Y              PIC 9(4) VALUE 0.
       01 PIVOT-X             PIC 9(4) VALUE 0.
       01 PIVOT-Y             PIC 9(4) VALUE 0.
       01 ANGLE               PIC 9(5)V99 VALUE 0.
       01 DISTANCE            PIC 9(5)V99 VALUE 0.
       01 TEMP-ANGLE          PIC 9(5)V99 VALUE 0.
       01 TEMP-DISTANCE       PIC 9(5)V99 VALUE 0.
       01 I                   PIC 9(3) VALUE 0.
       01 J                   PIC 9(3) VALUE 0.
       01 K                   PIC 9(3) VALUE 0.
       01 L                   PIC 9(3) VALUE 0.
       01 MIN-Y               PIC 9(4) VALUE 9999.
       01 MIN-X               PIC 9(4) VALUE 9999.
       01 MAX-X               PIC 9(4) VALUE 0.
       01 MAX-Y               PIC 9(4) VALUE 0.
       01 CCW-RESULT          PIC 9 VALUE 0.
       01 END-OF-FILE         PIC X VALUE 'N'.
       01 PI                  PIC 9(3)V999999 VALUE 3.141592.
       01 TWO-PI              PIC 9(3)V999999 VALUE 6.283184.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-PROGRAM.
           PERFORM READ-INPUT-POINTS.
           PERFORM SORT-POINTS.
           PERFORM FIND-START-POINT.
           PERFORM SORT-BY-ANGLE.
           PERFORM BUILD-CONVEX-HULL.
           PERFORM WRITE-OUTPUT.
           STOP RUN.

       INITIALIZE-PROGRAM.
           MOVE 0 TO POINT-COUNT.
           MOVE 0 TO STACK-INDEX.
           MOVE 0 TO MIN-Y.
           MOVE 0 TO MIN-X.
           MOVE 0 TO MAX-X.
           MOVE 0 TO MAX-Y.
           MOVE 'N' TO END-OF-FILE.
           MOVE 0 TO START-X.
           MOVE 0 TO START-Y.
           PERFORM INITIALIZE-ARRAYS.

       INITIALIZE-ARRAYS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
               MOVE 0 TO POINT-ARRAY(I).
               MOVE 0 TO STACK-ARRAY(I).
           END-PERFORM.

       READ-INPUT-POINTS.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END MOVE 'Y' TO END-OF-FILE
           END-READ.
           PERFORM UNTIL END-OF-FILE = 'Y'
               ADD 1 TO POINT-COUNT
               MOVE X-COORD TO POINT-ARRAY(POINT-COUNT).
               MOVE Y-COORD TO POINT-ARRAY(POINT-COUNT).
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END MOVE 'Y' TO END-OF-FILE
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

       SORT-POINTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT - 1
               PERFORM VARYING J FROM I + 1 BY 1 UNTIL J > POINT-COUNT
                   IF POINT-ARRAY(J) < POINT-ARRAY(I)
                       MOVE POINT-ARRAY(J) TO TEMP-X
                       MOVE POINT-ARRAY(J) TO TEMP-Y
                       MOVE POINT-ARRAY(I) TO POINT-ARRAY(J)
                       MOVE POINT-ARRAY(I) TO POINT-ARRAY(J)
                       MOVE TEMP-X TO POINT-ARRAY(I)
                       MOVE TEMP-Y TO POINT-ARRAY(I)
                   END-IF
               END-PERFORM
           END-PERFORM.

       FIND-START-POINT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF POINT-ARRAY(I) < MIN-Y OR MIN-Y = 0
                   MOVE POINT-ARRAY(I) TO MIN-Y
                   MOVE POINT-ARRAY(I) TO START-X
                   MOVE POINT-ARRAY(I) TO START-Y
               END-IF
           END-PERFORM.

       SORT-BY-ANGLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF POINT-ARRAY(I) = START-X AND POINT-ARRAY(I) = START-Y
                   MOVE I TO K
                   GO TO SORT-ANGLE-CONTINUE
               END-IF
           END-PERFORM.

       SORT-ANGLE-CONTINUE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF POINT-ARRAY(I) = START-X AND POINT-ARRAY(I) = START-Y
                   CONTINUE
               ELSE
                   COMPUTE ANGLE = ATAN2(POINT-ARRAY(I) - START-Y,
                                        POINT-ARRAY(I) - START-X)
                   COMPUTE DISTANCE = SQRT((POINT-ARRAY(I) - START-X) ** 2 +
                                          (POINT-ARRAY(I) - START-Y) ** 2)
                   PERFORM SORT-INSERT-POINT
               END-IF
           END-PERFORM.

       SORT-INSERT-POINT.
           PERFORM VARYING J FROM I BY -1 UNTIL J <= 1
               IF ANGLE < TEMP-ANGLE
                   MOVE TEMP-ANGLE TO ANGLE(J)
                   MOVE TEMP-DISTANCE TO DISTANCE(J)
                   MOVE POINT-ARRAY(J) TO POINT-ARRAY(J-1)
                   MOVE POINT-ARRAY(J) TO POINT-ARRAY(J-1)
               ELSE
                   GO TO SORT-INSERT-CONTINUE
               END-IF
           END-PERFORM.

       SORT-INSERT-CONTINUE.
           MOVE ANGLE TO TEMP-ANGLE.
           MOVE DISTANCE TO TEMP-DISTANCE.
           MOVE POINT-ARRAY(I) TO POINT-ARRAY(I).

       BUILD-CONVEX-HULL.
           PERFORM ADD-TO-STACK.
           PERFORM ADD-TO-STACK.
           PERFORM ADD-TO-STACK.
           PERFORM VARYING I FROM 4 BY 1 UNTIL I > POINT-COUNT
               PERFORM POP-STACK-IF-NEEDED.
               PERFORM ADD-TO-STACK.
           END-PERFORM.

       ADD-TO-STACK.
           ADD 1 TO STACK-INDEX.
           MOVE POINT-ARRAY(I) TO STACK-ARRAY(STACK-INDEX).
           MOVE POINT-ARRAY(I) TO STACK-ARRAY(STACK-INDEX).

       POP-STACK-IF-NEEDED.
           PERFORM UNTIL STACK-INDEX < 3
               PERFORM CHECK-COUNTER-CLOCKWISE.
               IF CCW-RESULT = 0
                   SUBTRACT 1 FROM STACK-INDEX
               ELSE
                   GO TO POP-CONTINUE
               END-IF
           END-PERFORM.

       POP-CONTINUE.

       CHECK-COUNTER-CLOCKWISE.
           COMPUTE CCW-RESULT = (STACK-ARRAY(STACK-INDEX-1) - STACK-ARRAY(STACK-INDEX-2)) *
                                (STACK-ARRAY(STACK-INDEX) - STACK-ARRAY(STACK-INDEX-2)) -
                                (STACK-ARRAY(STACK-INDEX) - STACK-ARRAY(STACK-INDEX-1)) *
                                (STACK-ARRAY(STACK-INDEX-1) - STACK-ARRAY(STACK-INDEX-2)).

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > STACK-INDEX
               MOVE STACK-ARRAY(I) TO OUTPUT-X
               MOVE STACK-ARRAY(I) TO OUTPUT-Y
               WRITE OUTPUT-RECORD FROM OUTPUT-RECORD
           END-PERFORM.
           CLOSE OUTPUT-FILE.

       ATAN2-FUNCTION.
           IF X = 0 AND Y > 0
               MOVE 90 TO RESULT
           ELSE IF X = 0 AND Y < 0
               MOVE 270 TO RESULT
           ELSE IF X > 0
               COMPUTE RESULT = ATAN(Y/X) * 180 / PI
           ELSE
               COMPUTE RESULT = ATAN(Y/X) * 180 / PI + 180
           END-IF.

       SQRT-FUNCTION.
           COMPUTE RESULT = X ** 0.5.
```

## Input File Format (POINTS.DAT)
```
100 100
200 100
200 200
100 200
150 150
```

## Output File Format (HULL.DAT)
```
100 100
200 100
200 200
100 200
```

## Key Features of This Implementation:

1. **Point Reading**: Reads points from an input file
2. **Sorting**: Sorts points by angle from the starting point
3. **Convex Hull Construction**: Uses the Graham scan algorithm with a stack
4. **Counter-Clockwise Testing**: Determines if three points make a left turn
5. **Output Generation**: Writes the convex hull points to an output file

## Algorithm Steps:

1. Find the bottom-most point (or left-most if tied)
2. Sort all points by polar angle with respect to the bottom-most point
3. Process points in order, maintaining a stack
4. For each point, check if it makes a counter-clockwise turn with the previous two points
5. If not, pop from stack until a valid turn is found
6. Push current point to stack
7. The stack contains the convex hull points

This implementation demonstrates how to translate the mathematical concepts of Graham's scan algorithm into COBOL syntax and logic.

