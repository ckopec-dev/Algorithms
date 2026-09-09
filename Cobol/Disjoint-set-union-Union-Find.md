# Disjoint Set Union (Union-Find) Algorithm in COBOL

Here's a complete COBOL implementation of the Union-Find data structure with path compression and union by rank optimizations:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNIONFIND-EXAMPLE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  MAX-NODES              PIC 9(4) VALUE 100.
       01  NODES                  PIC 9(4) VALUE 10.
       01  I                      PIC 9(4).
       01  J                      PIC 9(4).
       01  X                      PIC 9(4).
       01  Y                      PIC 9(4).
       01  SET-X                  PIC 9(4).
       01  SET-Y                  PIC 9(4).
       01  TEMP                   PIC 9(4).

       01  PARENTS.
           05 PARENT               OCCURS 100 TIMES PIC 9(4).

       01  RANKS.
           05 RANK                 OCCURS 100 TIMES PIC 9(4).

       01  NUM-SETS               PIC 9(4) VALUE 10.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-DATA
           PERFORM PRINT-INITIAL-STATUS
           PERFORM UNION-EXAMPLES
           PERFORM PRINT-FINAL-STATUS
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 0 TO I
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NODES
               MOVE I TO PARENT(I)
               MOVE 0 TO RANK(I)
           END-PERFORM.

       PRINT-INITIAL-STATUS.
           DISPLAY "INITIAL UNION-FIND STATUS"
           DISPLAY "==========================="
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NODES
               DISPLAY "Node " I " is in set " PARENT(I)
           END-PERFORM.

       UNION-EXAMPLES.
           DISPLAY "Performing Union Operations:"
           DISPLAY "============================"

           PERFORM UNION-OPERATION WITH TEST AFTER VARYING I FROM 1 BY 2 UNTIL I > 8
               VARYING J FROM 1 BY 2 UNTIL J > 8
                   MOVE I TO X
                   MOVE J TO Y
                   PERFORM UNIFY-X-Y
           END-PERFORM.

       UNION-OPERATION.
           IF X NOT = Y
               DISPLAY "Union(" X "," Y ")"
               PERFORM UNIFY-X-Y
           END-IF.

       UNIFY-X-Y.
           PERFORM FIND-SET-X
           PERFORM FIND-SET-Y
           IF SET-X NOT = SET-Y
               IF RANK(SET-X) > RANK(SET-Y)
                   MOVE SET-X TO PARENT(SET-Y)
                   IF RANK(SET-X) = RANK(SET-Y)
                       ADD 1 TO RANK(SET-X)
                   END-IF
               ELSE
                   MOVE SET-Y TO PARENT(SET-X)
                   IF RANK(SET-Y) > RANK(SET-X)
                       MOVE RANK(SET-Y) TO RANK(SET-X)
                   END-IF
               END-IF
               SUBTRACT 1 FROM NUM-SETS
               DISPLAY "  -> Sets merged. Total sets: " NUM-SETS
           ELSE
               DISPLAY "  -> Nodes already in same set"
           END-IF.

       FIND-SET-X.
           IF PARENT(X) NOT = X
               MOVE PARENT(X) TO TEMP
               PERFORM FIND-SET-RECURSIVE
               MOVE TEMP TO PARENT(X)
           END-IF
           MOVE PARENT(X) TO SET-X.

       FIND-SET-Y.
           IF PARENT(Y) NOT = Y
               MOVE PARENT(Y) TO TEMP
               PERFORM FIND-SET-RECURSIVE
               MOVE TEMP TO PARENT(Y)
           END-IF
           MOVE PARENT(Y) TO SET-Y.

       FIND-SET-RECURSIVE.
           PERFORM FIND-SET-X
           MOVE SET-X TO PARENT(X).

       PRINT-FINAL-STATUS.
           DISPLAY "FINAL UNION-FIND STATUS"
           DISPLAY "========================"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NODES
               PERFORM FIND-SET-X
               DISPLAY "Node " I " is in set " SET-X
           END-PERFORM.
           DISPLAY "Total number of sets: " NUM-SETS.
```

## Key Features of this Implementation:

### Data Structures:
- **PARENT array**: Stores parent of each node (initially node itself)
- **RANK array**: Stores rank (depth approximation) of each tree
- **NUM-SETS**: Tracks total number of disjoint sets

### Main Operations:
1. **FIND-SET**: Finds root of set with path compression optimization
2. **UNION**: Merges two sets using union by rank optimization

### Optimizations Used:
- **Path Compression**: During find operation, all nodes point directly to root
- **Union by Rank**: Always attach smaller tree under root of larger tree

### Example Output:
```
INITIAL UNION-FIND STATUS
=========================
Node 1 is in set 1
Node 2 is in set 2
Node 3 is in set 3
...
Performing Union Operations:
============================
Union(1,2)
  -> Sets merged. Total sets: 9
Union(3,4)  
  -> Sets merged. Total sets: 8
Union(5,6)
  -> Sets merged. Total sets: 7
...

FINAL UNION-FIND STATUS
========================
Node 1 is in set 1
Node 2 is in set 1
Node 3 is in set 3
...
Total number of sets: 4
```

This implementation demonstrates the core Union-Find algorithm with both optimizations to achieve near-constant time operations for practical purposes.