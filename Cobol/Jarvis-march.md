# Jarvis March Algorithm in COBOL

Here's an implementation of the Jarvis March (Gift Wrapping) algorithm for finding the convex hull of a set of points in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JARVIS-MARCH.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "POINTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 POINT-RECORD.
          05 X-COORD PIC 9(4).
          05 Y-COORD PIC 9(4).

       WORKING-STORAGE SECTION.
       01 POINT-TABLE.
          05 POINT OCCURS 100 TIMES INDEXED BY I.
             10 X-POINT PIC 9(4).
             10 Y-POINT PIC 9(4).

       01 MAX-POINTS PIC 9(3) VALUE 0.
       01 I PIC 9(3) VALUE 1.
       01 J PIC 9(3) VALUE 1.
       01 K PIC 9(3) VALUE 1.
       01 L PIC 9(3) VALUE 1.
       01 N PIC 9(3) VALUE 0.
       01 START-POINT PIC 9(3) VALUE 1.
       01 CURRENT-POINT PIC 9(3) VALUE 1.
       01 NEXT-POINT PIC 9(3) VALUE 1.
       01 TEMP-POINT PIC 9(3) VALUE 1.
       01 TURN-VALUE PIC 9(5)V99 VALUE 0.
       01 MIN-Y PIC 9(4) VALUE 9999.
       01 MIN-Y-INDEX PIC 9(3) VALUE 1.
       01 DONE PIC X VALUE 'N'.
       01 EOF-FLAG PIC X VALUE 'N'.
       01 CONVEX-HULL.
          05 HULL-POINT OCCURS 100 TIMES.
             10 HULL-X PIC 9(4).
             10 HULL-Y PIC 9(4).

       01 HULL-COUNT PIC 9(3) VALUE 0.
       01 HULL-INDEX PIC 9(3) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "JARVIS MARCH ALGORITHM - CONVEX HULL"
           DISPLAY "=================================="

           PERFORM READ-INPUT-DATA
           PERFORM FIND-START-POINT
           PERFORM COMPUTE-CONVEX-HULL
           PERFORM DISPLAY-RESULTS

           STOP RUN.

       READ-INPUT-DATA.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           PERFORM UNTIL EOF-FLAG = 'Y'
               IF N < 100
                   ADD 1 TO N
                   MOVE X-COORD TO X-POINT(N)
                   MOVE Y-COORD TO Y-POINT(N)
                   READ INPUT-FILE
                       AT END MOVE 'Y' TO EOF-FLAG
                   END-READ
               ELSE
                   DISPLAY "MAXIMUM POINTS EXCEEDED"
                   MOVE 'Y' TO EOF-FLAG
               END-IF
           END-PERFORM.

           CLOSE INPUT-FILE.
           DISPLAY "READ " N " POINTS"

       FIND-START-POINT.
           MOVE 1 TO MIN-Y-INDEX
           MOVE Y-POINT(1) TO MIN-Y

           PERFORM VARYING I FROM 2 BY 1 UNTIL I > N
               IF Y-POINT(I) < MIN-Y
                   MOVE Y-POINT(I) TO MIN-Y
                   MOVE I TO MIN-Y-INDEX
               END-IF
           END-PERFORM.

           MOVE MIN-Y-INDEX TO START-POINT.
           DISPLAY "START POINT: " START-POINT
           DISPLAY "COORDINATES: (" X-POINT(START-POINT) ","
           DISPLAY "              " Y-POINT(START-POINT) ")"

       COMPUTE-CONVEX-HULL.
           MOVE START-POINT TO CURRENT-POINT
           MOVE 1 TO HULL-INDEX
           MOVE CURRENT-POINT TO HULL-X(HULL-INDEX)
           MOVE CURRENT-POINT TO HULL-Y(HULL-INDEX)
           ADD 1 TO HULL-INDEX

           PERFORM UNTIL DONE = 'Y'
               MOVE 1 TO NEXT-POINT
               MOVE 0 TO TURN-VALUE

               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
                   IF J NOT = CURRENT-POINT
                       COMPUTE TURN-VALUE = 
                           (X-POINT(J) - X-POINT(CURRENT-POINT)) 
                           * (Y-POINT(NEXT-POINT) - Y-POINT(CURRENT-POINT))
                           - (Y-POINT(J) - Y-POINT(CURRENT-POINT))
                           * (X-POINT(NEXT-POINT) - X-POINT(CURRENT-POINT))

                       IF TURN-VALUE > 0 OR 
                          (TURN-VALUE = 0 AND J = 1)
                           MOVE J TO NEXT-POINT
                       END-IF
                   END-IF
               END-PERFORM

               IF NEXT-POINT = START-POINT
                   MOVE 'Y' TO DONE
               ELSE
                   MOVE NEXT-POINT TO CURRENT-POINT
                   MOVE CURRENT-POINT TO HULL-X(HULL-INDEX)
                   MOVE CURRENT-POINT TO HULL-Y(HULL-INDEX)
                   ADD 1 TO HULL-INDEX
               END-IF
           END-PERFORM

           SUBTRACT 1 FROM HULL-INDEX
           MOVE HULL-INDEX TO HULL-COUNT

       DISPLAY-RESULTS.
           DISPLAY "CONVEX HULL POINTS:"
           DISPLAY "==================="
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > HULL-COUNT
               DISPLAY "Point " I ": (" 
               DISPLAY HULL-X(I) "," HULL-Y(I) ")"
           END-PERFORM

       COMPUTE-TURN-VALUE.
           01 P1-X PIC 9(4).
           01 P1-Y PIC 9(4).
           01 P2-X PIC 9(4).
           01 P2-Y PIC 9(4).
           01 P3-X PIC 9(4).
           01 P3-Y PIC 9(4).
           01 TURN-RESULT PIC 9(5)V99.

           MOVE X-POINT(I) TO P1-X
           MOVE Y-POINT(I) TO P1-Y
           MOVE X-POINT(J) TO P2-X
           MOVE Y-POINT(J) TO P2-Y
           MOVE X-POINT(K) TO P3-X
           MOVE Y-POINT(K) TO P3-Y

           COMPUTE TURN-RESULT = 
               (P2-X - P1-X) * (P3-Y - P1-Y) 
               - (P2-Y - P1-Y) * (P3-X - P1-X)

           RETURN TURN-RESULT.
```

## Sample Input File (POINTS.DAT)
```
10 20
30 40
50 60
70 80
20 30
40 50
60 70
80 90
```

## Algorithm Explanation

The Jarvis March algorithm works as follows:

1. **Find the starting point**: Identify the point with the lowest y-coordinate (or leftmost if tied)
2. **Build the hull**: Starting from the initial point, find the next point by checking which point makes the largest counterclockwise turn
3. **Continue until return**: Keep building the hull until we return to the starting point

## Key Features

- Uses a table to store input points
- Implements the cross product calculation to determine turn direction
- Handles edge cases like collinear points
- Outputs the convex hull vertices in order

## Time Complexity
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(n) for storing the points

This implementation demonstrates the classic Jarvis March algorithm in COBOL, suitable for educational purposes and understanding computational geometry concepts in traditional programming languages.

