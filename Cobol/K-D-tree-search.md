# K-D Tree Search Algorithm in COBOL

Below is an example implementation of a K-D tree search algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KD-TREE-SEARCH.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KD-TREE-FILE ASSIGN TO "KD-TREE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  KD-TREE-FILE.
       01  TREE-RECORD.
           05  NODE-ID             PIC 9(5).
           05  COORDINATE-X        PIC 9(5)V99.
           05  COORDINATE-Y        PIC 9(5)V99.
           05  SPLIT-DIMENSION     PIC 9.
           05  LEFT-CHILD          PIC 9(5).
           05  RIGHT-CHILD         PIC 9(5).

       WORKING-STORAGE SECTION.
       01  SEARCH-POINT.
           05  SEARCH-X            PIC 9(5)V99.
           05  SEARCH-Y            PIC 9(5)V99.

       01  CURRENT-NODE.
           05  NODE-ID-TEMP        PIC 9(5).
           05  X-COORD             PIC 9(5)V99.
           05  Y-COORD             PIC 9(5)V99.
           05  DIMENSION-TEMP      PIC 9.
           05  LEFT-CHILD-TEMP     PIC 9(5).
           05  RIGHT-CHILD-TEMP    PIC 9(5).

       01  DISTANCE-SQUARED    PIC 9(10)V99.
       01  MIN-DISTANCE        PIC 9(10)V99 VALUE 9999999999.99.
       01  FOUND-FLAG          PIC X VALUE 'N'.
       01  MAX-DEPTH           PIC 99 VALUE 10.
       01  CURRENT-DEPTH       PIC 99 VALUE 0.
       01  SEARCH-DIMENSION    PIC 9 VALUE 1.
       01  TEMP-DISTANCE       PIC 9(10)V99.
       01  EOF-FLAG            PIC X VALUE 'N'.
       01  RECORD-COUNT        PIC 9(6) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "K-D TREE SEARCH ALGORITHM"
           DISPLAY "=========================="

           MOVE 50.00 TO SEARCH-X
           MOVE 75.00 TO SEARCH-Y

           CALL "KD-TREE-LOAD" USING SEARCH-X, SEARCH-Y
           CALL "KD-TREE-SEARCH-ALG" USING SEARCH-X, SEARCH-Y

           DISPLAY "Search completed"
           STOP RUN.

       KD-TREE-LOAD.
           *> Load tree data from file
           OPEN INPUT KD-TREE-FILE
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ KD-TREE-FILE
                   AT END MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       PERFORM PROCESS-NODE
               END-READ
           END-PERFORM
           CLOSE KD-TREE-FILE.

       PROCESS-NODE.
           ADD 1 TO RECORD-COUNT
           DISPLAY "Processing node " NODE-ID

       KD-TREE-SEARCH-ALG.
           *> Main search algorithm implementation
           MOVE 1 TO CURRENT-DEPTH
           MOVE 1 TO SEARCH-DIMENSION
           MOVE 'N' TO FOUND-FLAG

           CALL "SEARCH-RECURSIVE" USING 
               BY VALUE 1,
               BY REFERENCE SEARCH-X,
               BY REFERENCE SEARCH-Y,
               BY REFERENCE MIN-DISTANCE

       SEARCH-RECURSIVE.
           *> Recursive search function
           01  NODE-ID-PARAM        PIC 9(5).
           01  X-SEARCH             PIC 9(5)V99.
           01  Y-SEARCH             PIC 9(5)V99.
           01  MIN-DISTANCE-PARAM   PIC 9(10)V99.

           MOVE FUNCTION TRIM(CURRENT-NODE-ID) TO NODE-ID-PARAM
           MOVE SEARCH-X TO X-SEARCH
           MOVE SEARCH-Y TO Y-SEARCH
           MOVE MIN-DISTANCE TO MIN-DISTANCE-PARAM

           *> Check if we've reached maximum depth
           IF CURRENT-DEPTH > MAX-DEPTH THEN
               GO TO SEARCH-RECURSIVE-END
           END-IF

           *> Calculate distance to current node
           COMPUTE DISTANCE-SQUARED = 
               (X-SEARCH - X-COORD) * (X-SEARCH - X-COORD) +
               (Y-SEARCH - Y-COORD) * (Y-SEARCH - Y-COORD)

           *> Update minimum distance if this is closer
           IF DISTANCE-SQUARED < MIN-DISTANCE-PARAM THEN
               MOVE DISTANCE-SQUARED TO MIN-DISTANCE-PARAM
               MOVE 'Y' TO FOUND-FLAG
           END-IF

           *> Determine which subtree to search first
           IF SEARCH-DIMENSION = 1 THEN
               IF X-SEARCH <= X-COORD THEN
                   *> Search left subtree first
                   CALL "SEARCH-RECURSIVE" 
                       USING FUNCTION TRIM(LEFT-CHILD-TEMP),
                             BY REFERENCE X-SEARCH,
                             BY REFERENCE Y-SEARCH,
                             BY REFERENCE MIN-DISTANCE-PARAM
               ELSE
                   *> Search right subtree first
                   CALL "SEARCH-RECURSIVE"
                       USING FUNCTION TRIM(RIGHT-CHILD-TEMP),
                             BY REFERENCE X-SEARCH,
                             BY REFERENCE Y-SEARCH,
                             BY REFERENCE MIN-DISTANCE-PARAM
               END-IF
           ELSE
               IF Y-SEARCH <= Y-COORD THEN
                   *> Search left subtree first
                   CALL "SEARCH-RECURSIVE"
                       USING FUNCTION TRIM(LEFT-CHILD-TEMP),
                             BY REFERENCE X-SEARCH,
                             BY REFERENCE Y-SEARCH,
                             BY REFERENCE MIN-DISTANCE-PARAM
               ELSE
                   *> Search right subtree first
                   CALL "SEARCH-RECURSIVE"
                       USING FUNCTION TRIM(RIGHT-CHILD-TEMP),
                             BY REFERENCE X-SEARCH,
                             BY REFERENCE Y-SEARCH,
                             BY REFERENCE MIN-DISTANCE-PARAM
               END-IF
           END-IF

           SEARCH-RECURSIVE-END.
           EXIT.

       DISPLAY-RESULTS.
           DISPLAY "Closest point found"
           DISPLAY "Distance: " MIN-DISTANCE
           DISPLAY "Found flag: " FOUND-FLAG
```

## Key Features of this Implementation:

1. **Data Structure**: Uses a file-based approach to represent the K-D tree nodes
2. **Recursive Search**: Implements the core search algorithm recursively
3. **Dimensional Splitting**: Alternates between x and y dimensions for splitting
4. **Distance Calculation**: Computes Euclidean distance squared for efficiency
5. **Optimization**: Searches the most promising subtree first based on the search point

## How it Works:

1. The program loads tree data from a file
2. It performs a recursive search through the K-D tree
3. At each node, it calculates the distance to the search point
4. It determines which subtree to explore first based on the current dimension
5. The algorithm continues until the minimum distance is found or maximum depth is reached

This implementation demonstrates the fundamental concepts of K-D tree searching in COBOL, though actual performance would be significantly slower than optimized implementations in modern languages.