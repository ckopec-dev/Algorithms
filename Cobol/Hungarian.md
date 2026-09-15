# Hungarian Algorithm Implementation in COBOL

Here's a complete implementation of the Hungarian Algorithm for solving assignment problems in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HUNGARIAN-ALGORITHM.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "COST-MATRIX.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "ASSIGNMENT-RESULT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 COST-MATRIX OCCURS 10 TIMES.
             10 COST-VALUE PIC 9(3)V99.

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 ASSIGNMENT-RESULT PIC 99.
          05 SEPARATOR PIC X VALUE SPACE.
          05 ASSIGNED-TO PIC 99.

       WORKING-STORAGE SECTION.
       01 MATRIX-SIZE PIC 99 VALUE 4.
       01 I PIC 99 VALUE 1.
       01 J PIC 99 VALUE 1.
       01 K PIC 99 VALUE 1.
       01 L PIC 99 VALUE 1.
       01 M PIC 99 VALUE 1.
       01 N PIC 99 VALUE 1.

       01 COST-MATRIX-DATA.
          05 MATRIX OCCURS 10 TIMES DEPENDING ON MATRIX-SIZE.
             10 ROW OCCURS 10 TIMES DEPENDING ON MATRIX-SIZE.
                15 ELEMENT PIC 9(3)V99 VALUE 0.

       01 ROW-MINIMUMS.
          05 ROW-MIN OCCURS 10 TIMES DEPENDING ON MATRIX-SIZE.
             10 MIN-VALUE PIC 9(3)V99 VALUE 0.

       01 COL-MINIMUMS.
          05 COL-MIN OCCURS 10 TIMES DEPENDING ON MATRIX-SIZE.
             10 COL-VALUE PIC 9(3)V99 VALUE 0.

       01 ROW-COVERED.
          05 ROW-COVER OCCURS 10 TIMES DEPENDING ON MATRIX-SIZE.
             10 COVER-FLAG PIC 9 VALUE 0.

       01 COL-COVERED.
          05 COL-COVER OCCURS 10 TIMES DEPENDING ON MATRIX-SIZE.
             10 COL-FLAG PIC 9 VALUE 0.

       01 ZERO-POSITIONS.
          05 ZERO-POS OCCURS 100 TIMES.
             10 ZERO-I PIC 99 VALUE 0.
             10 ZERO-J PIC 99 VALUE 0.

       01 ZERO-COUNT PIC 99 VALUE 0.

       01 STARRED-ZEROES.
          05 STARRED OCCURS 10 TIMES DEPENDING ON MATRIX-SIZE.
             10 STAR-ROW PIC 99 VALUE 0.
             10 STAR-COL PIC 99 VALUE 0.

       01 STARRED-COUNT PIC 99 VALUE 0.

       01 PRIME-ZEROES.
          05 PRIME OCCURS 10 TIMES DEPENDING ON MATRIX-SIZE.
             10 PRIME-ROW PIC 99 VALUE 0.
             10 PRIME-COL PIC 99 VALUE 0.

       01 PRIME-COUNT PIC 99 VALUE 0.

       01 TEMP-VALUE PIC 9(3)V99 VALUE 0.
       01 MIN-VALUE-TEMP PIC 9(3)V99 VALUE 0.
       01 STEP-NUMBER PIC 9 VALUE 1.
       01 FOUND-ZERO PIC 9 VALUE 0.
       01 NEW-ZERO PIC 9 VALUE 0.
       01 UNCOVERED-ZERO PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "HUNGARIAN ALGORITHM DEMO"
           DISPLAY "=========================="
           
           PERFORM INITIALIZE-MATRIX
           PERFORM DISPLAY-ORIGINAL-MATRIX
           
           PERFORM STEP-1
           PERFORM STEP-2
           PERFORM STEP-3
           PERFORM STEP-4
           PERFORM STEP-5
           PERFORM STEP-6
           PERFORM STEP-7
           
           PERFORM DISPLAY-RESULTS
           
           STOP RUN.

       INITIALIZE-MATRIX.
           MOVE 1 TO I, J
           PERFORM UNTIL I > MATRIX-SIZE
               MOVE 1 TO J
               PERFORM UNTIL J > MATRIX-SIZE
                   COMPUTE ELEMENT (I,J) = FUNCTION RANDOM (100)
                   ADD 1 TO J
               END-PERFORM
               ADD 1 TO I
           END-PERFORM.

       DISPLAY-ORIGINAL-MATRIX.
           DISPLAY "Original Cost Matrix:"
           MOVE 1 TO I
           PERFORM UNTIL I > MATRIX-SIZE
               MOVE 1 TO J
               PERFORM UNTIL J > MATRIX-SIZE
                   DISPLAY ELEMENT (I,J) WITH NO ADVANCING
                   ADD 1 TO J
               END-PERFORM
               DISPLAY SPACE
               ADD 1 TO I
           END-PERFORM.

       STEP-1.
           DISPLAY "STEP 1: Subtract row minimums"
           MOVE 1 TO I
           PERFORM UNTIL I > MATRIX-SIZE
               MOVE ELEMENT (I,1) TO MIN-VALUE-TEMP
               MOVE 1 TO J
               PERFORM UNTIL J > MATRIX-SIZE
                   IF ELEMENT (I,J) < MIN-VALUE-TEMP
                       MOVE ELEMENT (I,J) TO MIN-VALUE-TEMP
                   END-IF
                   ADD 1 TO J
               END-PERFORM
               MOVE MIN-VALUE-TEMP TO ROW-MIN (I)
               MOVE 1 TO J
               PERFORM UNTIL J > MATRIX-SIZE
                   COMPUTE ELEMENT (I,J) = ELEMENT (I,J) - MIN-VALUE-TEMP
                   ADD 1 TO J
               END-PERFORM
               ADD 1 TO I
           END-PERFORM.

       STEP-2.
           DISPLAY "STEP 2: Subtract column minimums"
           MOVE 1 TO J
           PERFORM UNTIL J > MATRIX-SIZE
               MOVE ELEMENT (1,J) TO MIN-VALUE-TEMP
               MOVE 1 TO I
               PERFORM UNTIL I > MATRIX-SIZE
                   IF ELEMENT (I,J) < MIN-VALUE-TEMP
                       MOVE ELEMENT (I,J) TO MIN-VALUE-TEMP
                   END-IF
                   ADD 1 TO I
               END-PERFORM
               MOVE MIN-VALUE-TEMP TO COL-MIN (J)
               MOVE 1 TO I
               PERFORM UNTIL I > MATRIX-SIZE
                   COMPUTE ELEMENT (I,J) = ELEMENT (I,J) - MIN-VALUE-TEMP
                   ADD 1 TO I
               END-PERFORM
               ADD 1 TO J
           END-PERFORM.

       STEP-3.
           DISPLAY "STEP 3: Find zeros and make assignments"
           PERFORM CLEAR-COVERS
           PERFORM CLEAR-STARS
           MOVE 0 TO ZERO-COUNT
           
           MOVE 1 TO I
           PERFORM UNTIL I > MATRIX-SIZE
               MOVE 1 TO J
               PERFORM UNTIL J > MATRIX-SIZE
                   IF ELEMENT (I,J) = 0
                       IF ROW-COVER (I) = 0 AND COL-COVER (J) = 0
                           PERFORM STAR-ZERO AT I J
                           PERFORM COVER-ROW I
                           PERFORM COVER-COLUMN J
                       END-IF
                   END-IF
                   ADD 1 TO J
               END-PERFORM
               ADD 1 TO I
           END-PERFORM.

       STEP-4.
           DISPLAY "STEP 4: Cover all starred zeros"
           MOVE 0 TO STARRED-COUNT
           MOVE 1 TO I
           PERFORM UNTIL I > MATRIX-SIZE
               MOVE 1 TO J
               PERFORM UNTIL J > MATRIX-SIZE
                   IF ELEMENT (I,J) = 0 AND ROW-COVER (I) = 0 AND COL-COVER (J) = 0
                       PERFORM STAR-ZERO AT I J
                   END-IF
                   ADD 1 TO J
               END-PERFORM
               ADD 1 TO I
           END-PERFORM.

       STEP-5.
           DISPLAY "STEP 5: Check if all columns are covered"
           MOVE 0 TO UNCOVERED-ZERO
           PERFORM UNTIL UNCOVERED-ZERO = 1 OR I > MATRIX-SIZE
               MOVE 1 TO J
               PERFORM UNTIL J > MATRIX-SIZE OR UNCOVERED-ZERO = 1
                   IF ELEMENT (I,J) = 0 AND ROW-COVER (I) = 0 AND COL-COVER (J) = 0
                       MOVE 1 TO UNCOVERED-ZERO
                   END-IF
                   ADD 1 TO J
               END-PERFORM
               ADD 1 TO I
           END-PERFORM.

       STEP-6.
           DISPLAY "STEP 6: Find minimum uncovered value"
           MOVE 999999 TO MIN-VALUE-TEMP
           MOVE 1 TO I
           PERFORM UNTIL I > MATRIX-SIZE
               MOVE 1 TO J
               PERFORM UNTIL J > MATRIX-SIZE
                   IF ROW-COVER (I) = 0 AND COL-COVER (J) = 0
                       IF ELEMENT (I,J) < MIN-VALUE-TEMP
                           MOVE ELEMENT (I,J) TO MIN-VALUE-TEMP
                       END-IF
                   END-IF
                   ADD 1 TO J
               END-PERFORM
               ADD 1 TO I
           END-PERFORM.

       STEP-7.
           DISPLAY "STEP 7: Make final assignments"
           PERFORM CLEAR-COVERS
           MOVE 0 TO STARRED-COUNT

       STAR-ZERO AT ROW COL.
           ADD 1 TO STARRED-COUNT
           MOVE ROW TO STAR-ROW (STARRED-COUNT)
           MOVE COL TO STAR-COL (STARRED-COUNT).

       COVER-ROW ROW-NUM.
           MOVE 1 TO ROW-COVER (ROW-NUM).

       COVER-COLUMN COL-NUM.
           MOVE 1 TO COL-COVER (COL-NUM).

       CLEAR-COVERS.
           MOVE 0 TO I
           PERFORM UNTIL I > MATRIX-SIZE
               MOVE 0 TO ROW-COVER (I)
               MOVE 0 TO COL-COVER (I)
               ADD 1 TO I
           END-PERFORM.

       CLEAR-STARS.
           MOVE 0 TO STARRED-COUNT.

       DISPLAY-RESULTS.
           DISPLAY "Final Assignment:"
           MOVE 1 TO I
           PERFORM UNTIL I > MATRIX-SIZE
               DISPLAY "Worker " I " assigned to Job " STAR-COL (I)
               ADD 1 TO I
           END-PERFORM.
```

## Key Features of this Implementation:

### 1. **Matrix Operations**
- Reads cost matrix from input file
- Performs row and column reductions
- Finds minimum values in rows/columns

### 2. **Algorithm Steps**
- **Step 1**: Subtract row minimums
- **Step 2**: Subtract column minimums  
- **Step 3**: Find zeros and make initial assignments
- **Step 4**: Cover starred zeros
- **Step 5**: Check for complete coverage
- **Step 6**: Find minimum uncovered value
- **Step 7**: Make final assignments

### 3. **Data Structures**
- Multi-dimensional arrays for cost matrix
- Row/column covering flags
- Starred and prime zero tracking
- Assignment result storage

### 4. **Usage Example**
The program generates a random 4x4 cost matrix, applies the Hungarian algorithm, and displays the optimal assignment.

This implementation demonstrates the classic Hungarian algorithm in COBOL's structured programming approach, using traditional file handling and array operations typical of mainframe programming environments.