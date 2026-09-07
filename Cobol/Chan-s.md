# Chan's Algorithm Implementation in COBOL

Here's an example implementation of Chan's algorithm for computing convex hull using COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHANS-ALGORITHM.
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
       01 POINT-RECORD.
          05 X-COORDINATE    PIC 9(5)V99.
          05 Y-COORDINATE    PIC 9(5)V99.

       FD OUTPUT-FILE.
       01 HULL-RECORD.
          05 HULL-X          PIC 9(5)V99.
          05 HULL-Y          PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 POINTS-TABLE.
          05 POINT-ITEM OCCURS 100 TIMES INDEXED BY I.
             10 POINT-X     PIC 9(5)V99.
             10 POINT-Y     PIC 9(5)V99.

       01 HULL-TABLE.
          05 HULL-ITEM OCCURS 100 TIMES INDEXED BY J.
             10 HULL-X-ITEM PIC 9(5)V99.
             10 HULL-Y-ITEM PIC 9(5)V99.

       01 VARIABLES.
          05 POINT-COUNT      PIC 99 VALUE 0.
          05 K                PIC 99 VALUE 0.
          05 I                PIC 99 VALUE 0.
          05 J                PIC 99 VALUE 0.
          05 L                PIC 99 VALUE 0.
          05 M                PIC 99 VALUE 0.
          05 N                PIC 99 VALUE 0.
          05 TEMP-X           PIC 9(5)V99.
          05 TEMP-Y           PIC 9(5)V99.
          05 ANGLE            PIC 9(5)V99.
          05 MIN-ANGLE        PIC 9(5)V99 VALUE 999.99.
          05 MAX-ANGLE        PIC 9(5)V99 VALUE -999.99.
          05 CROSS-PRODUCT    PIC 9(5)V99.
          05 DISTANCE         PIC 9(5)V99.
          05 MIN-DISTANCE     PIC 9(5)V99 VALUE 99999.99.
          05 MAX-DISTANCE     PIC 9(5)V99 VALUE -99999.99.

       01 FLAGS.
          05 IS-CONVEX        PIC X VALUE 'N'.
             88 CONVEX       VALUE 'Y'.
             88 NOT-CONVEX   VALUE 'N'.
          05 DONE             PIC X VALUE 'N'.
             88 COMPLETED    VALUE 'Y'.
             88 NOT-COMPLETED VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-PROGRAM.
           PERFORM READ-INPUT-DATA.
           PERFORM COMPUTE-CONVEX-HULL.
           PERFORM WRITE-OUTPUT-DATA.
           STOP RUN.

       INITIALIZE-PROGRAM.
           MOVE 0 TO POINT-COUNT.
           MOVE 0 TO K.
           MOVE 0 TO I.
           MOVE 0 TO J.
           MOVE 0 TO L.
           MOVE 0 TO M.
           MOVE 'N' TO IS-CONVEX.
           MOVE 'N' TO DONE.

       READ-INPUT-DATA.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE AT END GO TO END-OF-FILE.
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO POINT-COUNT
               MOVE X-COORDINATE TO POINT-ITEM(POINT-COUNT) OF POINTS-TABLE
               MOVE Y-COORDINATE TO POINT-ITEM(POINT-COUNT) OF POINTS-TABLE
               READ INPUT-FILE AT END GO TO END-OF-FILE
           END-PERFORM.
           CLOSE INPUT-FILE.

       END-OF-FILE.
           IF POINT-COUNT = 0 THEN
               DISPLAY "NO POINTS READ"
               STOP RUN
           END-IF.

       COMPUTE-CONVEX-HULL.
           PERFORM SORT-POINTS-ALPHA.
           PERFORM GIFT-WRAPPER-ALGORITHM.
           PERFORM CHANS-ALGORITHM-IMPLEMENTATION.

       SORT-POINTS-ALPHA.
           SORT POINTS-TABLE BY POINT-X ASCENDING.

       GIFT-WRAPPER-ALGORITHM.
           IF POINT-COUNT < 3 THEN
               MOVE POINT-COUNT TO K
               PERFORM COPY-TO-HULL
               GO TO END-GIFT-WRAPPER
           END-IF.

           PERFORM FIND-LEFTMOST-POINT.
           PERFORM FIND-RIGHTMOST-POINT.
           PERFORM FIND-LOWEST-POINT.
           PERFORM FIND-HIGHEST-POINT.

       END-GIFT-WRAPPER.
           CONTINUE.

       CHANS-ALGORITHM-IMPLEMENTATION.
           COMPUTE-K.
           PERFORM DIVIDE-CONVEX-HULL.
           PERFORM MERGE-CONVEX-HULLS.

       COMPUTE-K.
           COMPUTE K = FUNCTION CEILING(FUNCTION LOG(POINT-COUNT) / FUNCTION LOG(2)).

       DIVIDE-CONVEX-HULL.
           IF POINT-COUNT <= 3 THEN
               MOVE POINT-COUNT TO K
               PERFORM COPY-TO-HULL
               GO TO END-DIVIDE
           END-IF.

           COMPUTE N = POINT-COUNT / K.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM COMPUTE-SUBSET-CONVEX-HULL
           END-PERFORM.

       END-DIVIDE.
           CONTINUE.

       COMPUTE-SUBSET-CONVEX-HULL.
           IF (I * K) <= POINT-COUNT THEN
               PERFORM GIFT-WRAPPER ON POINTS-TABLE(1) THRU POINTS-TABLE(I*K)
           ELSE
               PERFORM GIFT-WRAPPER ON POINTS-TABLE(1) THRU POINTS-TABLE(POINT-COUNT)
           END-IF.

       MERGE-CONVEX-HULLS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > K
               PERFORM MERGE-CONVEX-HULL-PAIR
           END-PERFORM.

       MERGE-CONVEX-HULL-PAIR.
           PERFORM COMPUTE-MERGE-POINT.
           PERFORM UPDATE-HULL-POINT.

       COMPUTE-MERGE-POINT.
           COMPUTE ANGLE = FUNCTION ATAN2((POINT-Y - HULL-Y-ITEM),
                                          (POINT-X - HULL-X-ITEM)).

       UPDATE-HULL-POINT.
           IF ANGLE < MIN-ANGLE THEN
               MOVE ANGLE TO MIN-ANGLE
           END-IF.

       COPY-TO-HULL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               MOVE POINT-X TO HULL-X-ITEM(I)
               MOVE POINT-Y TO HULL-Y-ITEM(I)
           END-PERFORM.

       WRITE-OUTPUT-DATA.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > K
               WRITE HULL-RECORD FROM HULL-ITEM(J)
           END-PERFORM.
           CLOSE OUTPUT-FILE.

       COMPUTE-CROSS-PRODUCT.
           COMPUTE CROSS-PRODUCT = 
               (POINT-X - POINT-X) * (POINT-Y - POINT-Y) -
               (POINT-Y - POINT-Y) * (POINT-X - POINT-X).

       COMPUTE-DISTANCE.
           COMPUTE DISTANCE = FUNCTION SQRT(
               (POINT-X - POINT-X) ** 2 +
               (POINT-Y - POINT-Y) ** 2
           ).
```

## Key Features of This Implementation:

1. **Data Structure**: Uses tables to store points and hull vertices
2. **Algorithm Components**:
   - Point reading from input file
   - Sorting points by x-coordinate
   - Gift wrapping algorithm for small sets
   - Division into subsets (k)
   - Merging of convex hulls

3. **Main Steps**:
   - Read input data
   - Sort points
   - Apply Chan's algorithm logic
   - Write results to output file

4. **Variables Used**:
   - Point coordinates (X, Y)
   - Hull storage tables
   - Angle calculations
   - Distance computations
   - Loop counters and flags

This implementation demonstrates the core concepts of Chan's algorithm while adhering to COBOL syntax and structure.