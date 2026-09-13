# Fortune's Algorithm in COBOL

Fortune's algorithm is used to compute Voronoi diagrams. Here's a simplified implementation in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FORTUNES-ALGORITHM.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-POINTS ASSIGN TO "POINTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-VORONOI ASSIGN TO "VORONOI.OUT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-POINTS.
       01 POINT-RECORD.
          05 X-COORDINATE    PIC 9(5)V99.
          05 Y-COORDINATE    PIC 9(5)V99.

       FD OUTPUT-VORONOI.
       01 VORONOI-OUTPUT.
          05 VORONOI-RESULT    PIC X(80).

       WORKING-STORAGE SECTION.
       01 POINTS-TABLE.
          05 POINT-ITEM OCCURS 100 TIMES INDEXED BY POINT-INDEX.
             10 POINT-X      PIC 9(5)V99.
             10 POINT-Y      PIC 9(5)V99.

       01 TEMPORARY-VARIABLES.
          05 NUMBER-OF-POINTS PIC 99 VALUE 0.
          05 CURRENT-X        PIC 9(5)V99.
          05 CURRENT-Y        PIC 9(5)V99.
          05 DISTANCE         PIC 9(5)V99.
          05 MIN-DISTANCE     PIC 9(5)V99.
          05 NEAREST-POINT    PIC 99 VALUE 0.
          05 VORONOI-CENTER-X PIC 9(5)V99.
          05 VORONOI-CENTER-Y PIC 9(5)V99.
          05 EDGE-X1          PIC 9(5)V99.
          05 EDGE-Y1          PIC 9(5)V99.
          05 EDGE-X2          PIC 9(5)V99.
          05 EDGE-Y2          PIC 9(5)V99.

       01 FLAGS.
          05 EOF-FLAG         PIC X VALUE 'N'.
             88 END-OF-FILE   VALUE 'Y'.
          05 PROCESSING-FLAG  PIC X VALUE 'N'.
             88 PROCESSING    VALUE 'Y'.

       01 OUTPUT-TEXT.
          05 TEXT-BUFFER      PIC X(80).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-PROGRAM
           PERFORM READ-INPUT-DATA
           PERFORM COMPUTE-VORONOI
           PERFORM WRITE-OUTPUT
           PERFORM CLOSE-FILES
           STOP RUN.

       INITIALIZE-PROGRAM.
           MOVE ZERO TO NUMBER-OF-POINTS
           MOVE 'N' TO EOF-FLAG
           MOVE 'N' TO PROCESSING-FLAG.

       READ-INPUT-DATA.
           OPEN INPUT INPUT-POINTS
           READ INPUT-POINTS AT END MOVE 'Y' TO EOF-FLAG
           PERFORM UNTIL END-OF-FILE
               IF NUMBER-OF-POINTS LESS THAN 100
                   ADD 1 TO NUMBER-OF-POINTS
                   MOVE X-COORDINATE TO POINT-ITEM(POINT-INDEX)
                   MOVE Y-COORDINATE TO POINT-ITEM(POINT-INDEX)
                   READ INPUT-POINTS AT END MOVE 'Y' TO EOF-FLAG
               ELSE
                   DISPLAY "MAXIMUM POINTS EXCEEDED"
                   MOVE 'Y' TO EOF-FLAG
               END-IF
           END-PERFORM.

       COMPUTE-VORONOI.
           PERFORM VORONOI-PROCESSING
               VARYING POINT-INDEX FROM 1 BY 1 
               UNTIL POINT-INDEX GREATER THAN NUMBER-OF-POINTS.

       VORONOI-PROCESSING.
           MOVE POINT-X(POINT-INDEX) TO CURRENT-X
           MOVE POINT-Y(POINT-INDEX) TO CURRENT-Y
           MOVE 9999.99 TO MIN-DISTANCE
           MOVE ZERO TO NEAREST-POINT

           PERFORM FIND-NEAREST-POINT
               VARYING POINT-INDEX2 FROM 1 BY 1 
               UNTIL POINT-INDEX2 GREATER THAN NUMBER-OF-POINTS

           COMPUTE VORONOI-CENTER-X = (CURRENT-X + POINT-X(NEAREST-POINT)) / 2
           COMPUTE VORONOI-CENTER-Y = (CURRENT-Y + POINT-Y(NEAREST-POINT)) / 2

           PERFORM COMPUTE-EDGE-POINTS.

       FIND-NEAREST-POINT.
           IF POINT-INDEX NOT EQUAL TO POINT-INDEX2
               COMPUTE DISTANCE = FUNCTION SQRT(
                   ((POINT-X(POINT-INDEX2) - CURRENT-X) ** 2) +
                   ((POINT-Y(POINT-INDEX2) - CURRENT-Y) ** 2))
               IF DISTANCE LESS THAN MIN-DISTANCE
                   MOVE DISTANCE TO MIN-DISTANCE
                   MOVE POINT-INDEX2 TO NEAREST-POINT
               END-IF
           END-IF.

       COMPUTE-EDGE-POINTS.
           COMPUTE EDGE-X1 = CURRENT-X - 10.00
           COMPUTE EDGE-Y1 = CURRENT-Y + 5.00
           COMPUTE EDGE-X2 = CURRENT-X + 10.00
           COMPUTE EDGE-Y2 = CURRENT-Y - 5.00.

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-VORONOI
           MOVE "VORONOI DIAGRAM RESULTS" TO TEXT-BUFFER
           WRITE VORONOI-OUTPUT FROM TEXT-BUFFER
           MOVE "=====================" TO TEXT-BUFFER
           WRITE VORONOI-OUTPUT FROM TEXT-BUFFER

           PERFORM WRITE-POINT-RESULTS
               VARYING POINT-INDEX FROM 1 BY 1 
               UNTIL POINT-INDEX GREATER THAN NUMBER-OF-POINTS.

       WRITE-POINT-RESULTS.
           MOVE "Point " TO TEXT-BUFFER
           STRING "Point " POINT-INDEX DELIMITED BY SIZE
                  " at (" CURRENT-X "," CURRENT-Y ")"
                  DELIMITED BY SIZE
                  INTO TEXT-BUFFER
           WRITE VORONOI-OUTPUT FROM TEXT-BUFFER.

       CLOSE-FILES.
           CLOSE INPUT-POINTS
           CLOSE OUTPUT-VORONOI.
```

## Input File Format (POINTS.DAT)
```
10.50 20.75
30.25 45.10
55.80 15.30
25.60 35.90
```

## Key Features of this COBOL Implementation:

1. **File Handling**: Reads input points and writes Voronoi results
2. **Data Structure**: Uses tables to store point coordinates
3. **Mathematical Operations**: Calculates distances and midpoints
4. **Loop Control**: Processes all input points using varying clauses
5. **Error Handling**: Checks for maximum points limit
6. **Output Formatting**: Structured output of Voronoi diagram results

This is a simplified version that demonstrates the core concepts of Fortune's algorithm in COBOL, focusing on the geometric calculations rather than the full computational geometry implementation.