# Dantzig's Simplex Algorithm in COBOL

Here's an example implementation of Dantzig's Simplex algorithm in COBOL for solving linear programming problems:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLEX-ALGORITHM.
       AUTHOR. Linear Programming Example.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "SIMPLEX.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 PROBLEM-TYPE     PIC X(10).
          05 NUM-VARIABLES    PIC 9(3).
          05 NUM-CONSTRAINTS  PIC 9(3).
          05 COEFFICIENTS     PIC 9(5)V9(2) OCCURS 20 TIMES.
          05 RHS              PIC 9(5)V9(2) OCCURS 10 TIMES.

       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
          05 WS-NUM-VARS      PIC 9(3) VALUE 3.
          05 WS-NUM-CONSTR    PIC 9(3) VALUE 2.
          05 WS-COEFFICIENTS  PIC 9(5)V9(2) OCCURS 10 TIMES.
          05 WS-RHS           PIC 9(5)V99 VALUE 0.
          05 WS-BASIS         PIC 9(3) OCCURS 10 TIMES.
          05 WS-PIVOT-ROW     PIC 9(3).
          05 WS-PIVOT-COL     PIC 9(3).
          05 WS-PIVOT-ELEM    PIC 9(5)V9(2).
          05 WS-OPTIMAL       PIC X VALUE 'N'.
          05 WS-ITERATION     PIC 9(3) VALUE 0.

       01 WS-TABLEAU.
          05 TABLEAU-ROW      OCCURS 10 TIMES.
             10 TABLEAU-ELEM  PIC 9(5)V9(2) OCCURS 10 TIMES.

       01 WS-TEMPORARY.
          05 WS-TEMP-COEFF    PIC 9(5)V9(2).
          05 WS-TEMP-RHS      PIC 9(5)V9(2).
          05 WS-TEMP-BASIS    PIC 9(3).
          05 WS-TEMP-INDEX    PIC 9(3).

       01 WS-RESULTS.
          05 WS-OPTIMAL-SOLUTION PIC 9(5)V9(2) OCCURS 10 TIMES.
          05 WS-OBJECTIVE-VALUE  PIC 9(5)V9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "DANTZIG'S SIMPLEX ALGORITHM"
           DISPLAY "=========================="
           
           PERFORM INITIALIZE-SIMPLEX
           PERFORM SOLVE-SIMPLEX
           PERFORM DISPLAY-RESULTS
           
           STOP RUN.

       INITIALIZE-SIMPLEX.
           MOVE 0 TO WS-ITERATION
           MOVE "N" TO WS-OPTIMAL
           
           DISPLAY "Initializing Simplex Tableau..."
           
           *> Example problem: Maximize 3x1 + 2x2
           *> Subject to: x1 + x2 <= 4
           *>             2x1 + x2 <= 6
           *>             x1, x2 >= 0
           
           *> Initialize tableau with slack variables
           MOVE 3 TO WS-NUM-VARS
           MOVE 2 TO WS-NUM-CONSTR
           
           *> Objective function coefficients (negated for maximization)
           MOVE -3 TO TABLEAU-ELEM(1,1)
           MOVE -2 TO TABLEAU-ELEM(1,2)
           MOVE 0 TO TABLEAU-ELEM(1,3)
           MOVE 0 TO TABLEAU-ELEM(1,4)
           
           *> Constraint coefficients
           MOVE 1 TO TABLEAU-ELEM(2,1)
           MOVE 1 TO TABLEAU-ELEM(2,2)
           MOVE 1 TO TABLEAU-ELEM(2,3)
           MOVE 0 TO TABLEAU-ELEM(2,4)
           MOVE 4 TO TABLEAU-ELEM(2,5)
           
           MOVE 2 TO TABLEAU-ELEM(3,1)
           MOVE 1 TO TABLEAU-ELEM(3,2)
           MOVE 0 TO TABLEAU-ELEM(3,3)
           MOVE 1 TO TABLEAU-ELEM(3,4)
           MOVE 6 TO TABLEAU-ELEM(3,5)
           
           *> Initialize basis (slack variables)
           MOVE 3 TO WS-BASIS(1)
           MOVE 4 TO WS-BASIS(2)
           
           DISPLAY "Initial Tableau:"
           PERFORM DISPLAY-TABLEAU
           .

       SOLVE-SIMPLEX.
           PERFORM UNTIL WS-OPTIMAL = "Y"
               ADD 1 TO WS-ITERATION
               DISPLAY "Iteration " WS-ITERATION
               
               PERFORM FIND-ENTERING-VARIABLE
               IF WS-PIVOT-COL = 0
                   MOVE "Y" TO WS-OPTIMAL
                   DISPLAY "Optimal solution found"
                   GO TO END-SIMPLEX
               END-IF
               
               PERFORM FIND-LEAVING-VARIABLE
               IF WS-PIVOT-ROW = 0
                   DISPLAY "Problem is unbounded"
                   GO TO END-SIMPLEX
               END-IF
               
               PERFORM PIVOT-OPERATION
               PERFORM DISPLAY-TABLEAU
           END-PERFORM
           .

       FIND-ENTERING-VARIABLE.
           *> Find most negative coefficient in objective row
           MOVE 0 TO WS-PIVOT-COL
           MOVE 0 TO WS-TEMP-COEFF
           
           PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
               UNTIL WS-TEMP-INDEX > WS-NUM-VARS
               IF TABLEAU-ELEM(1,WS-TEMP-INDEX) < WS-TEMP-COEFF
                   MOVE TABLEAU-ELEM(1,WS-TEMP-INDEX) TO WS-TEMP-COEFF
                   MOVE WS-TEMP-INDEX TO WS-PIVOT-COL
               END-IF
           END-PERFORM
           
           DISPLAY "Entering variable column: " WS-PIVOT-COL
           .

       FIND-LEAVING-VARIABLE.
           *> Apply minimum ratio test
           MOVE 0 TO WS-PIVOT-ROW
           MOVE 999999 TO WS-TEMP-RHS
           
           PERFORM VARYING WS-TEMP-INDEX FROM 2 BY 1
               UNTIL WS-TEMP-INDEX > WS-NUM-CONSTR + 1
               IF TABLEAU-ELEM(WS-TEMP-INDEX,WS-PIVOT-COL) > 0
                   DIVIDE TABLEAU-ELEM(WS-TEMP-INDEX,WS-NUM-VARS + 1)
                           BY TABLEAU-ELEM(WS-TEMP-INDEX,WS-PIVOT-COL)
                           GIVING WS-TEMP-COEFF
                   IF WS-TEMP-COEFF < WS-TEMP-RHS
                       MOVE WS-TEMP-COEFF TO WS-TEMP-RHS
                       MOVE WS-TEMP-INDEX TO WS-PIVOT-ROW
                   END-IF
               END-IF
           END-PERFORM
           
           DISPLAY "Leaving variable row: " WS-PIVOT-ROW
           .

       PIVOT-OPERATION.
           *> Perform pivot operation
           MOVE TABLEAU-ELEM(WS-PIVOT-ROW,WS-PIVOT-COL) TO WS-PIVOT-ELEM
           
           *> Divide pivot row by pivot element
           PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
               UNTIL WS-TEMP-INDEX > WS-NUM-VARS + 1
               DIVIDE TABLEAU-ELEM(WS-PIVOT-ROW,WS-TEMP-INDEX)
                       BY WS-PIVOT-ELEM
                       GIVING TABLEAU-ELEM(WS-PIVOT-ROW,WS-TEMP-INDEX)
           END-PERFORM
           
           *> Update other rows
           PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
               UNTIL WS-TEMP-INDEX > WS-NUM-CONSTR + 1
               IF WS-TEMP-INDEX NOT = WS-PIVOT-ROW
                   MOVE TABLEAU-ELEM(WS-TEMP-INDEX,WS-PIVOT-COL) TO WS-TEMP-COEFF
                   PERFORM VARYING WS-TEMP-INDEX2 FROM 1 BY 1
                       UNTIL WS-TEMP-INDEX2 > WS-NUM-VARS + 1
                       COMPUTE TABLEAU-ELEM(WS-TEMP-INDEX,WS-TEMP-INDEX2)
                           = TABLEAU-ELEM(WS-TEMP-INDEX,WS-TEMP-INDEX2)
                           - (WS-TEMP-COEFF * TABLEAU-ELEM(WS-PIVOT-ROW,WS-TEMP-INDEX2))
                   END-PERFORM
               END-IF
           END-PERFORM
           
           *> Update basis
           MOVE WS-PIVOT-COL TO WS-BASIS(WS-PIVOT-ROW)
           .

       DISPLAY-TABLEAU.
           DISPLAY "------------------------"
           DISPLAY "Tableau Iteration " WS-ITERATION
           DISPLAY "------------------------"
           DISPLAY "Variables: " WS-NUM-VARS " Constraints: " WS-NUM-CONSTR
           
           PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
               UNTIL WS-TEMP-INDEX > WS-NUM-CONSTR + 1
               PERFORM VARYING WS-TEMP-INDEX2 FROM 1 BY 1
                   UNTIL WS-TEMP-INDEX2 > WS-NUM-VARS + 1
                   DISPLAY TABLEAU-ELEM(WS-TEMP-INDEX,WS-TEMP-INDEX2) " "
               END-PERFORM
               DISPLAY ""
           END-PERFORM
           DISPLAY "------------------------"
           .

       DISPLAY-RESULTS.
           DISPLAY "Optimal Solution:"
           DISPLAY "================="
           DISPLAY "Objective value: " TABLEAU-ELEM(1,WS-NUM-VARS + 1)
           DISPLAY "Optimal variables:"
           
           PERFORM VARYING WS-TEMP-INDEX FROM 1 BY 1
               UNTIL WS-TEMP-INDEX > WS-NUM-VARS
               IF WS-BASIS(1) = WS-TEMP-INDEX OR WS-BASIS(2) = WS-TEMP-INDEX
                   DISPLAY "X" WS-TEMP-INDEX ": " TABLEAU-ELEM(WS-TEMP-INDEX,WS-NUM-VARS + 1)
               END-IF
           END-PERFORM
           .

       END-SIMPLEX.
           EXIT.
```

## Example Input Data (SIMPLEX.DAT)

```
MAXIMIZE 3 2
2 3
1 1 0 0 4
2 1 0 1 6
```

## Key Features of this Implementation

1. **Tableau Representation**: Uses a 2D array to represent the simplex tableau
2. **Initialization**: Sets up the initial tableau with slack variables
3. **Pivot Operations**: Implements the core simplex pivot operations
4. **Optimality Test**: Checks for optimality using negative coefficients
5. **Ratio Test**: Implements minimum ratio test for leaving variable selection
6. **Iteration Tracking**: Tracks and displays each iteration of the algorithm

## Algorithm Steps

1. **Initialization**: Set up the initial tableau
2. **Optimality Check**: Look for negative coefficients in objective row
3. **Entering Variable**: Select variable with most negative coefficient
4. **Leaving Variable**: Apply minimum ratio test
5. **Pivot Operation**: Perform row operations to update tableau
6. **Repeat**: Continue until optimal solution is found

This COBOL implementation demonstrates the classic simplex algorithm for solving linear programming problems, though in practice, modern implementations would use more sophisticated data structures and optimization techniques.

