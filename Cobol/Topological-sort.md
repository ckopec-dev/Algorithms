# Topological Sort Algorithm in COBOL

Here's an example of implementing a topological sort algorithm in COBOL using Kahn's algorithm approach:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOPOLOGICAL-SORT.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "DEPENDENCIES.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 DEPENDENT-ITEM    PIC X(10).
          05 DEPENDENCY-ITEM   PIC X(10).

       WORKING-STORAGE SECTION.
       01 MAX-NODES           PIC 9(3) VALUE 20.
       01 NODES-COUNT         PIC 9(3) VALUE 0.
       01 QUEUE-LENGTH        PIC 9(3) VALUE 0.
       01 QUEUE-INDEX         PIC 9(3) VALUE 1.
       01 CURRENT-NODE        PIC X(10).
       01 TEMP-NODE           PIC X(10).
       01 TEMP-DEP            PIC X(10).
       01 I                   PIC 9(3).
       01 J                   PIC 9(3).
       01 K                   PIC 9(3).
       01 TEMP-INT            PIC 9(3).
       01 EOF-FLAG            PIC X VALUE 'N'.
       01 SORTED-FLAG         PIC X VALUE 'N'.

       01 NODE-TABLE.
          05 NODE-ITEM         PIC X(10) OCCURS 20 TIMES.
          05 NODE-DEPS         PIC 9(3) OCCURS 20 TIMES.
          05 NODE-INDEG        PIC 9(3) OCCURS 20 TIMES.
          05 NODE-INDEX        PIC 9(3) OCCURS 20 TIMES.

       01 QUEUE-TABLE.
          05 QUEUE-ITEM        PIC X(10) OCCURS 20 TIMES.

       01 SORTED-TABLE.
          05 SORTED-ITEM       PIC X(10) OCCURS 20 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-DATA.
           PERFORM READ-DEPENDENCIES.
           PERFORM CALCULATE-INDEG.
           PERFORM BUILD-QUEUE.
           PERFORM TOPSORT-ALGORITHM.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO NODES-COUNT.
           MOVE 0 TO QUEUE-LENGTH.
           MOVE 1 TO QUEUE-INDEX.
           MOVE 'N' TO EOF-FLAG.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
               MOVE 0 TO NODE-DEPS(I)
               MOVE 0 TO NODE-INDEG(I)
               MOVE SPACES TO NODE-ITEM(I)
               MOVE SPACES TO QUEUE-ITEM(I)
               MOVE SPACES TO SORTED-ITEM(I)
           END-PERFORM.

       READ-DEPENDENCIES.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ.
           PERFORM UNTIL EOF-FLAG = 'Y'
               PERFORM PROCESS-DEPENDENCY
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END MOVE 'Y' TO EOF-FLAG
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

       PROCESS-DEPENDENCY.
           MOVE DEPENDENT-ITEM TO TEMP-NODE.
           MOVE DEPENDENCY-ITEM TO TEMP-DEP.
           
           PERFORM FIND-OR-ADD-NODE TEMP-NODE.
           PERFORM FIND-OR-ADD-NODE TEMP-DEP.
           
           PERFORM ADD-DEPENDENCY.

       FIND-OR-ADD-NODE.
           MOVE TEMP-NODE TO CURRENT-NODE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NODES-COUNT
               IF NODE-ITEM(I) = CURRENT-NODE
                   GO TO NODE-FOUND
               END-IF
           END-PERFORM.
           
           ADD 1 TO NODES-COUNT.
           MOVE CURRENT-NODE TO NODE-ITEM(NODES-COUNT).
           MOVE NODES-COUNT TO NODE-INDEX(NODES-COUNT).
           
           NODE-FOUND.

       ADD-DEPENDENCY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NODES-COUNT
               IF NODE-ITEM(I) = TEMP-NODE
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > NODES-COUNT
                       IF NODE-ITEM(J) = TEMP-DEP
                           ADD 1 TO NODE-INDEG(J)
                           ADD 1 TO NODE-DEPS(I)
                           GO TO DEPENDENCY-ADDED
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.
           DEPENDENCY-ADDED.

       CALCULATE-INDEG.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NODES-COUNT
               IF NODE-INDEG(I) = 0
                   ADD 1 TO QUEUE-LENGTH
                   MOVE NODE-ITEM(I) TO QUEUE-ITEM(QUEUE-LENGTH)
               END-IF
           END-PERFORM.

       BUILD-QUEUE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NODES-COUNT
               IF NODE-INDEG(I) = 0
                   ADD 1 TO QUEUE-LENGTH
                   MOVE NODE-ITEM(I) TO QUEUE-ITEM(QUEUE-LENGTH)
               END-IF
           END-PERFORM.

       TOPSORT-ALGORITHM.
           PERFORM UNTIL QUEUE-LENGTH = 0
               MOVE QUEUE-ITEM(1) TO CURRENT-NODE.
               MOVE CURRENT-NODE TO SORTED-ITEM(NODES-COUNT - QUEUE-LENGTH + 1).
               PERFORM REMOVE-FROM-QUEUE.
               PERFORM PROCESS-NEIGHBORS.
           END-PERFORM.

       REMOVE-FROM-QUEUE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > QUEUE-LENGTH
               IF I < QUEUE-LENGTH
                   MOVE QUEUE-ITEM(I + 1) TO QUEUE-ITEM(I)
               ELSE
                   MOVE SPACES TO QUEUE-ITEM(I)
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM QUEUE-LENGTH.

       PROCESS-NEIGHBORS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NODES-COUNT
               IF NODE-ITEM(I) = CURRENT-NODE
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > NODES-COUNT
                       IF NODE-ITEM(J) = NODE-ITEM(I)
                           SUBTRACT 1 FROM NODE-INDEG(J)
                           IF NODE-INDEG(J) = 0
                               ADD 1 TO QUEUE-LENGTH
                               MOVE NODE-ITEM(J) TO QUEUE-ITEM(QUEUE-LENGTH)
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "TOPOLOGICAL SORT RESULT:".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > (NODES-COUNT - QUEUE-LENGTH)
               DISPLAY "Step " I ": " SORTED-ITEM(I)
           END-PERFORM.
           DISPLAY "Topological sort completed successfully.".
```

## Sample Input File (DEPENDENCIES.DAT)
```
A B
B C
C D
A D
```

## Key Features of this Implementation:

1. **Data Structures**: Uses tables to store nodes, dependencies, and queue
2. **Algorithm Steps**:
   - Read dependencies from input file
   - Calculate in-degrees for each node
   - Build initial queue with nodes having in-degree 0
   - Process nodes using queue (Kahn's algorithm)
   - Output topological order

3. **Key Variables**:
   - `NODE-TABLE`: Stores all nodes and their properties
   - `QUEUE-TABLE`: Implements the queue for processing
   - `SORTED-TABLE`: Stores final topological order

4. **Error Handling**: Includes basic EOF handling and validation

This implementation demonstrates how to perform topological sorting in COBOL, which is useful for dependency resolution in build systems, task scheduling, and other applications where ordered processing is required.

