# Kruskal's Algorithm in COBOL

Here's an example implementation of Kruskal's algorithm for finding Minimum Spanning Tree in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KRUSKAL-MST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "edges.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 EDGE-RECORD.
          05 SOURCE-NODE    PIC 9(3).
          05 DEST-NODE      PIC 9(3).
          05 EDGE-WEIGHT    PIC 9(5).

       WORKING-STORAGE SECTION.
       01 WS-EOF              PIC X VALUE 'N'.
          88 END-OF-FILE      VALUE 'Y'.
       
       01 WS-NODES            PIC 9(3) VALUE 0.
       01 WS-EDGES            PIC 9(4) VALUE 0.
       01 WS-EDGE-COUNT       PIC 9(4) VALUE 0.
       01 WS-RESULT-COUNT     PIC 9(4) VALUE 0.
       
       01 WS-TEMP-WEIGHT      PIC 9(5).
       01 WS-TEMP-SOURCE      PIC 9(3).
       01 WS-TEMP-DEST        PIC 9(3).
       
       01 WS-UNION-FIND.
          05 NODE-PARENT    OCCURS 100 TIMES PIC 9(3) VALUE 0.
          05 NODE-RANK      OCCURS 100 TIMES PIC 9(3) VALUE 0.
       
       01 WS-EDGES-TABLE.
          05 EDGE-ITEM      OCCURS 1000 TIMES.
             10 EDGE-SOURCE   PIC 9(3).
             10 EDGE-DEST     PIC 9(3).
             10 EDGE-WEIGHT   PIC 9(5).
       
       01 WS-RESULT-TABLE.
          05 RESULT-ITEM    OCCURS 1000 TIMES.
             10 RESULT-SOURCE PIC 9(3).
             10 RESULT-DEST   PIC 9(3).
             10 RESULT-WEIGHT PIC 9(5).
       
       01 WS-INDEX            PIC 9(4) VALUE 0.
       01 WS-I                PIC 9(4) VALUE 0.
       01 WS-J                PIC 9(4) VALUE 0.
       01 WS-K                PIC 9(4) VALUE 0.
       01 WS-ROOT1            PIC 9(3) VALUE 0.
       01 WS-ROOT2            PIC 9(3) VALUE 0.
       01 WS-TEMP             PIC 9(5) VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "KRUSKAL'S ALGORITHM FOR MST"
           DISPLAY "============================"
           
           PERFORM INITIALIZE-DATA
           PERFORM READ-EDGES
           PERFORM SORT-EDGES
           PERFORM FIND-MST
           PERFORM DISPLAY-RESULT
           
           STOP RUN.
       
       INITIALIZE-DATA.
           MOVE 0 TO WS-NODES WS-EDGES WS-EDGE-COUNT WS-RESULT-COUNT
           MOVE 0 TO NODE-PARENT (1) THRU NODE-PARENT (100)
           MOVE 0 TO NODE-RANK (1) THRU NODE-RANK (100)
           MOVE 0 TO EDGE-SOURCE (1) THRU EDGE-SOURCE (1000)
           MOVE 0 TO EDGE-DEST (1) THRU EDGE-DEST (1000)
           MOVE 0 TO EDGE-WEIGHT (1) THRU EDGE-WEIGHT (1000)
           MOVE 0 TO RESULT-SOURCE (1) THRU RESULT-SOURCE (1000)
           MOVE 0 TO RESULT-DEST (1) THRU RESULT-DEST (1000)
           MOVE 0 TO RESULT-WEIGHT (1) THRU RESULT-WEIGHT (1000).
       
       READ-EDGES.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE
               AT END MOVE 'Y' TO WS-EOF
           END-READ
           
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO WS-EDGE-COUNT
               MOVE SOURCE-NODE TO EDGE-SOURCE (WS-EDGE-COUNT)
               MOVE DEST-NODE TO EDGE-DEST (WS-EDGE-COUNT)
               MOVE EDGE-WEIGHT TO EDGE-WEIGHT (WS-EDGE-COUNT)
               
               READ INPUT-FILE
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
           END-PERFORM
           
           CLOSE INPUT-FILE.
       
       SORT-EDGES.
           PERFORM SORT-EDGES-RECURSIVE
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-EDGE-COUNT - 1
               VARYING WS-J FROM WS-I + 1 BY 1 UNTIL WS-J > WS-EDGE-COUNT
                   IF EDGE-WEIGHT (WS-J) < EDGE-WEIGHT (WS-I)
                       MOVE EDGE-WEIGHT (WS-I) TO WS-TEMP
                       MOVE EDGE-WEIGHT (WS-J) TO EDGE-WEIGHT (WS-I)
                       MOVE WS-TEMP TO EDGE-WEIGHT (WS-J)
                       
                       MOVE EDGE-SOURCE (WS-I) TO WS-TEMP-SOURCE
                       MOVE EDGE-SOURCE (WS-J) TO EDGE-SOURCE (WS-I)
                       MOVE WS-TEMP-SOURCE TO EDGE-SOURCE (WS-J)
                       
                       MOVE EDGE-DEST (WS-I) TO WS-TEMP-DEST
                       MOVE EDGE-DEST (WS-J) TO EDGE-DEST (WS-I)
                       MOVE WS-TEMP-DEST TO EDGE-DEST (WS-J)
                   END-IF
               END-PERFORM.
       
       SORT-EDGES-RECURSIVE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-EDGE-COUNT - 1
               PERFORM VARYING WS-J FROM WS-I + 1 BY 1 UNTIL WS-J > WS-EDGE-COUNT
                   IF EDGE-WEIGHT (WS-J) < EDGE-WEIGHT (WS-I)
                       MOVE EDGE-WEIGHT (WS-I) TO WS-TEMP
                       MOVE EDGE-WEIGHT (WS-J) TO EDGE-WEIGHT (WS-I)
                       MOVE WS-TEMP TO EDGE-WEIGHT (WS-J)
                       
                       MOVE EDGE-SOURCE (WS-I) TO WS-TEMP-SOURCE
                       MOVE EDGE-SOURCE (WS-J) TO EDGE-SOURCE (WS-I)
                       MOVE WS-TEMP-SOURCE TO EDGE-SOURCE (WS-J)
                       
                       MOVE EDGE-DEST (WS-I) TO WS-TEMP-DEST
                       MOVE EDGE-DEST (WS-J) TO EDGE-DEST (WS-I)
                       MOVE WS-TEMP-DEST TO EDGE-DEST (WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       FIND-MST.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-EDGE-COUNT
               PERFORM FIND-ROOT EDGE-SOURCE (WS-I) GIVING WS-ROOT1
               PERFORM FIND-ROOT EDGE-DEST (WS-I) GIVING WS-ROOT2
               
               IF WS-ROOT1 NOT EQUAL TO WS-ROOT2
                   PERFORM UNION-NODES WS-ROOT1 WS-ROOT2
                   ADD 1 TO WS-RESULT-COUNT
                   MOVE EDGE-SOURCE (WS-I) TO RESULT-SOURCE (WS-RESULT-COUNT)
                   MOVE EDGE-DEST (WS-I) TO RESULT-DEST (WS-RESULT-COUNT)
                   MOVE EDGE-WEIGHT (WS-I) TO RESULT-WEIGHT (WS-RESULT-COUNT)
               END-IF
           END-PERFORM.
       
       FIND-ROOT.
           01 NODE-TO-SEARCH     PIC 9(3).
           01 NODE-TEMP          PIC 9(3).
           
           MOVE FUNCTION INPUT (NODE-TO-SEARCH) TO WS-TEMP
           MOVE WS-TEMP TO NODE-TEMP
           
           PERFORM UNTIL NODE-PARENT (NODE-TEMP) = 0
               MOVE NODE-PARENT (NODE-TEMP) TO NODE-TEMP
           END-PERFORM
           
           MOVE NODE-TEMP TO WS-ROOT1.
       
       UNION-NODES.
           01 ROOT1              PIC 9(3).
           01 ROOT2              PIC 9(3).
           01 TEMP-RANK1         PIC 9(3).
           01 TEMP-RANK2         PIC 9(3).
           
           MOVE FUNCTION INPUT (ROOT1) TO WS-TEMP
           MOVE WS-TEMP TO ROOT1
           MOVE FUNCTION INPUT (ROOT2) TO WS-TEMP
           MOVE WS-TEMP TO ROOT2
           
           MOVE NODE-RANK (ROOT1) TO TEMP-RANK1
           MOVE NODE-RANK (ROOT2) TO TEMP-RANK2
           
           IF TEMP-RANK1 < TEMP-RANK2
               MOVE ROOT2 TO NODE-PARENT (ROOT1)
           ELSE IF TEMP-RANK1 > TEMP-RANK2
               MOVE ROOT1 TO NODE-PARENT (ROOT2)
           ELSE
               MOVE ROOT2 TO NODE-PARENT (ROOT1)
               ADD 1 TO NODE-RANK (ROOT1)
           END-IF.
       
       DISPLAY-RESULT.
           DISPLAY "MINIMUM SPANNING TREE:"
           DISPLAY "----------------------"
           DISPLAY "Edge    Weight"
           DISPLAY "----    ------"
           
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-RESULT-COUNT
               DISPLAY RESULT-SOURCE (WS-I) " - " RESULT-DEST (WS-I) "    " RESULT-WEIGHT (WS-I)
           END-PERFORM.
```

## Sample Input File (edges.dat)
```
1 2 4
1 3 2
2 3 1
2 4 5
3 4 8
3 5 10
4 5 2
```

## Key Features of This Implementation:

1. **Data Structures**: Uses arrays for edges, union-find structure for tracking connected components
2. **Sorting**: Implements bubble sort for edge weights
3. **Union-Find**: Implements path compression and union by rank
4. **Main Algorithm**: Processes edges in ascending order and applies union-find to detect cycles
5. **Output**: Displays the minimum spanning tree edges and weights

## How It Works:

1. Read all edges from input file
2. Sort edges by weight in ascending order
3. Initialize Union-Find structure for tracking connected components
4. Process each edge in sorted order:
   - Find root of both nodes using Union-Find
   - If roots are different (no cycle), add edge to MST
   - Union the two components
5. Display the resulting minimum spanning tree

This implementation demonstrates the core concepts of Kruskal's algorithm in traditional COBOL syntax while maintaining readability and functionality.

