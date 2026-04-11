# De Boor's Algorithm in COBOL

De Boor's algorithm is used for evaluating B-spline curves. Here's an implementation in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DE-BOOR-ALGORITHM.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  WS-DEGREE              PIC 9(3) VALUE 3.
       01  WS-N-DEGREE            PIC 9(3) VALUE 3.
       01  WS-N-KNOTS             PIC 9(3) VALUE 8.
       01  WS-U                    PIC 9(5)V99 VALUE 0.5.
       01  WS-RESULT-X            PIC 9(5)V99 VALUE 0.
       01  WS-RESULT-Y            PIC 9(5)V99 VALUE 0.
       01  WS-I                   PIC 9(3) VALUE 0.
       01  WS-J                   PIC 9(3) VALUE 0.
       01  WS-K                   PIC 9(3) VALUE 0.
       01  WS-TEMP                PIC 9(5)V99 VALUE 0.
       01  WS-TEMP2               PIC 9(5)V99 VALUE 0.
       01  WS-FLAG                PIC X VALUE 'N'.

       01  WS-KNOTS.
           05  WS-KNOT-VALUE    PIC 9(5)V99 OCCURS 8 TIMES.

       01  WS-CONTROL-POINTS.
           05  WS-CP-X          PIC 9(5)V99 OCCURS 5 TIMES.
           05  WS-CP-Y          PIC 9(5)V99 OCCURS 5 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-VALUES.
           PERFORM DE-BOOR-ALGORITHM.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-VALUES.
           MOVE 0.0 TO WS-KNOTS.
           MOVE 0.0 TO WS-CONTROL-POINTS.
           
           *> Knot vector: [0,0,0,0,1,2,3,3,3,3]
           MOVE 0.0 TO WS-KNOT-VALUE(1).
           MOVE 0.0 TO WS-KNOT-VALUE(2).
           MOVE 0.0 TO WS-KNOT-VALUE(3).
           MOVE 0.0 TO WS-KNOT-VALUE(4).
           MOVE 1.0 TO WS-KNOT-VALUE(5).
           MOVE 2.0 TO WS-KNOT-VALUE(6).
           MOVE 3.0 TO WS-KNOT-VALUE(7).
           MOVE 3.0 TO WS-KNOT-VALUE(8).
           MOVE 3.0 TO WS-KNOT-VALUE(9).
           MOVE 3.0 TO WS-KNOT-VALUE(10).

           *> Control points: (0,0), (1,1), (2,0), (3,1)
           MOVE 0.0 TO WS-CP-X(1).
           MOVE 1.0 TO WS-CP-X(2).
           MOVE 2.0 TO WS-CP-X(3).
           MOVE 3.0 TO WS-CP-X(4).
           MOVE 4.0 TO WS-CP-X(5).

           MOVE 0.0 TO WS-CP-Y(1).
           MOVE 1.0 TO WS-CP-Y(2).
           MOVE 0.0 TO WS-CP-Y(3).
           MOVE 1.0 TO WS-CP-Y(4).
           MOVE 0.0 TO WS-CP-Y(5).

           MOVE 0.5 TO WS-U.

       DE-BOOR-ALGORITHM.
           *> Find the knot span
           PERFORM FIND-KNOT-SPAN.

           *> De Boor's algorithm
           PERFORM INITIALIZE-DE-BOOR.

           *> Main loop
           PERFORM DE-BOOR-LOOP VARYING WS-K FROM 1 BY 1
               UNTIL WS-K > WS-DEGREE.

       FIND-KNOT-SPAN.
           *> Simple binary search for knot span
           MOVE 0 TO WS-I.
           IF WS-U < WS-KNOT-VALUE(1) 
               MOVE 1 TO WS-I
           ELSE IF WS-U >= WS-KNOT-VALUE(WS-N-KNOTS)
               MOVE WS-N-KNOTS TO WS-I
           ELSE
               MOVE 1 TO WS-J.
               MOVE WS-N-KNOTS TO WS-K.
               PERFORM BINARY-SEARCH UNTIL WS-J > WS-K.
           END-IF.

       BINARY-SEARCH.
           COMPUTE WS-K = (WS-J + WS-K) / 2.
           IF WS-U < WS-KNOT-VALUE(WS-K)
               MOVE WS-K TO WS-K
           ELSE
               MOVE WS-K TO WS-J
           END-IF.

       INITIALIZE-DE-BOOR.
           *> Initialize the de Boor points
           MOVE 0 TO WS-TEMP.
           PERFORM VARYING WS-J FROM 0 BY 1
               UNTIL WS-J > WS-DEGREE
               MOVE WS-CP-X(WS-I - WS-DEGREE + WS-J + 1) TO WS-TEMP
               MOVE WS-CP-Y(WS-I - WS-DEGREE + WS-J + 1) TO WS-TEMP2
           END-PERFORM.

       DE-BOOR-LOOP.
           *> De Boor's recursive algorithm
           PERFORM VARYING WS-J FROM 0 BY 1
               UNTIL WS-J > WS-DEGREE - WS-K
               COMPUTE WS-TEMP = (WS-U - WS-KNOT-VALUE(WS-I - WS-J + 1)) /
                                  (WS-KNOT-VALUE(WS-I + 1) - WS-KNOT-VALUE(WS-I - WS-J + 1))
               COMPUTE WS-TEMP2 = (WS-U - WS-KNOT-VALUE(WS-I - WS-J + 1)) /
                                  (WS-KNOT-VALUE(WS-I + 1) - WS-KNOT-VALUE(WS-I - WS-J + 1))
               COMPUTE WS-CP-X(WS-I - WS-J + 1) = 
                   WS-CP-X(WS-I - WS-J + 1) * (1 - WS-TEMP) + 
                   WS-CP-X(WS-I - WS-J + 2) * WS-TEMP
               COMPUTE WS-CP-Y(WS-I - WS-J + 1) = 
                   WS-CP-Y(WS-I - WS-J + 1) * (1 - WS-TEMP2) + 
                   WS-CP-Y(WS-I - WS-J + 2) * WS-TEMP2
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "De Boor Algorithm Results:".
           DISPLAY "U parameter: " WS-U.
           DISPLAY "Knot span: " WS-I.
           DISPLAY "Result X: " WS-CP-X(1).
           DISPLAY "Result Y: " WS-CP-Y(1).

       END PROGRAM DE-BOOR-ALGORITHM.
```

## Key Features of this Implementation:

1. **Data Structure**: Uses arrays for knot vector and control points
2. **Algorithm Steps**:
   - Find knot span using binary search
   - Initialize de Boor points
   - Apply recursive de Boor algorithm
3. **Variables**:
   - `WS-U`: Parameter value for curve evaluation
   - `WS-I`: Knot span index
   - `WS-DEGREE`: Degree of the B-spline
   - Control points and knot vectors stored in arrays

## Notes:
- This is a simplified version for demonstration
- Actual implementation would require more robust error handling
- The algorithm evaluates a B-spline curve at parameter u
- Uses the standard de Boor algorithm for B-spline evaluation

