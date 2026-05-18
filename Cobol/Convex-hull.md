# Convex Hull Algorithm in COBOL

Here's an example implementation of the Graham Scan algorithm for finding the convex hull using COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVEX-HULL.
       AUTHOR. COBOL EXAMPLE.

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
           05 POINT-X    PIC 9(5)V99.
           05 POINT-Y    PIC 9(5)V99.

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           05 OUTPUT-X   PIC 9(5)V99.
           05 OUTPUT-Y   PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 MAX-POINTS           PIC 9(3) VALUE 100.
       01 POINT-COUNT          PIC 9(3) VALUE 0.
       01 STACK-DEPTH          PIC 9(3) VALUE 0.
       01 I, J, K              PIC 9(3) VALUE 0.
       01 TEMP-X, TEMP-Y       PIC 9(5)V99.
       01 MIN-Y                PIC 9(5)V99.
       01 MIN-INDEX            PIC 9(3) VALUE 0.
       01 ANGLE                PIC 9(5)V99.
       01 CROSS-PRODUCT        PIC 9(5)V99.
       01 SWAP-FLAG            PIC 9 VALUE 0.
       01 EOF-FLAG             PIC 9 VALUE 0.

       01 POINT-TABLE.
           05 POINT-ITEM OCCURS 100 TIMES.
               10 X-COORD    PIC 9(5)V99.
               10 Y-COORD    PIC 9(5)V99.
               10 ANGLE-ITEM PIC 9(5)V99.

       01 STACK-TABLE.
           05 STACK-ITEM OCCURS 100 TIMES.
               10 STACK-X    PIC 9(5)V99.
               10 STACK-Y    PIC 9(5)V99.

       01 POINT-ARRAY.
           05 POINT-ARRAY-ITEM OCCURS 100 TIMES.
               10 ARRAY-X    PIC 9(5)V99.
               10 ARRAY-Y    PIC 9(5)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "CONVEX HULL CALCULATION"
           PERFORM READ-INPUT-DATA
           PERFORM SORT-POINTS
           PERFORM FIND-CONVEX-HULL
           PERFORM WRITE-OUTPUT
           STOP RUN.

       READ-INPUT-DATA.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE AT END MOVE 1 TO EOF-FLAG
           PERFORM UNTIL EOF-FLAG = 1
               IF POINT-COUNT < MAX-POINTS
                   ADD 1 TO POINT-COUNT
                   MOVE POINT-X TO POINT-ITEM(POINT-COUNT) X-COORD
                   MOVE POINT-Y TO POINT-ITEM(POINT-COUNT) Y-COORD
                   READ INPUT-FILE AT END MOVE 1 TO EOF-FLAG
               ELSE
                   DISPLAY "MAX POINTS EXCEEDED"
                   MOVE 1 TO EOF-FLAG
               END-IF
           END-PERFORM
           CLOSE INPUT-FILE.

       SORT-POINTS.
           PERFORM SORT-POINT-ARRAY
           PERFORM FIND-MIN-Y-POINT.

       SORT-POINT-ARRAY.
           SORT POINT-TABLE
               BY X-COORD
               ASCENDING Y-COORD
           GIVING POINT-TABLE.

       FIND-MIN-Y-POINT.
           MOVE 99999.99 TO MIN-Y
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               IF POINT-ITEM(I) Y-COORD < MIN-Y
                   MOVE POINT-ITEM(I) Y-COORD TO MIN-Y
                   MOVE I TO MIN-INDEX
               END-IF
           END-PERFORM.

       FIND-CONVEX-HULL.
           PERFORM INITIALIZE-STACK
           PERFORM GRAHAM-SCAN.

       INITIALIZE-STACK.
           MOVE 0 TO STACK-DEPTH
           MOVE POINT-ITEM(MIN-INDEX) X-COORD TO STACK-ITEM(1) X-COORD
           MOVE POINT-ITEM(MIN-INDEX) Y-COORD TO STACK-ITEM(1) Y-COORD
           ADD 1 TO STACK-DEPTH.

       GRAHAM-SCAN.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > POINT-COUNT
               PERFORM COMPUTE-ANGLES
               PERFORM REMOVE-WRONG-TURNS
           END-PERFORM.

       COMPUTE-ANGLES.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > POINT-COUNT
               IF J NOT = MIN-INDEX
                   COMPUTE ANGLE = ATAN2(
                       POINT-ITEM(J) Y-COORD - POINT-ITEM(MIN-INDEX) Y-COORD,
                       POINT-ITEM(J) X-COORD - POINT-ITEM(MIN-INDEX) X-COORD
                   )
                   MOVE ANGLE TO POINT-ITEM(J) ANGLE-ITEM
               END-IF
           END-PERFORM.

       REMOVE-WRONG-TURNS.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > POINT-COUNT
               IF STACK-DEPTH >= 2
                   PERFORM CHECK-CROSS-PRODUCT
                   IF CROSS-PRODUCT < 0
                       SUBTRACT 1 FROM STACK-DEPTH
                   END-IF
               END-IF
           END-PERFORM.

       CHECK-CROSS-PRODUCT.
           COMPUTE CROSS-PRODUCT =
               (STACK-ITEM(STACK-DEPTH) X-COORD - 
                STACK-ITEM(STACK-DEPTH-1) X-COORD) *
               (POINT-ITEM(K) Y-COORD - 
                STACK-ITEM(STACK-DEPTH-1) Y-COORD) -
               (STACK-ITEM(STACK-DEPTH) Y-COORD - 
                STACK-ITEM(STACK-DEPTH-1) Y-COORD) *
               (POINT-ITEM(K) X-COORD - 
                STACK-ITEM(STACK-DEPTH-1) X-COORD).

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > STACK-DEPTH
               MOVE STACK-ITEM(I) X-COORD TO OUTPUT-X
               MOVE STACK-ITEM(I) Y-COORD TO OUTPUT-Y
               WRITE OUTPUT-RECORD
           END-PERFORM
           CLOSE OUTPUT-FILE
           DISPLAY "CONVEX HULL POINTS WRITTEN TO HULL.DAT".
```

## Sample Input File (POINTS.DAT)
```
10.00 20.00
30.00 40.00
50.00 60.00
70.00 80.00
25.00 35.00
```

## Key Features of This Implementation:

1. **Data Structure**: Uses arrays to store points and stack for hull calculation
2. **Graham Scan Algorithm**: Implements the standard convex hull algorithm
3. **Point Sorting**: Sorts points by x-coordinate and y-coordinate
4. **Angle Calculation**: Computes angles for point ordering
5. **Cross Product**: Uses cross product to determine turn direction
6. **Stack Management**: Maintains stack of hull points

## Algorithm Steps:
1. Read all input points
2. Sort points by x-coordinate (and y-coordinate for ties)
3. Find the bottom-most point
4. Sort remaining points by polar angle
5. Build hull using Graham scan technique
6. Output convex hull points

This is a simplified implementation showing the core concepts. A production version would include more robust error handling and optimization.

