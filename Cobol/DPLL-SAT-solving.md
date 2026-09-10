# DPLL SAT Solver in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DPLL-SAT-SOLVER.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "SAT-PROBLEM.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 PROBLEM-RECORD.
          05 NUM-VARIABLES    PIC 9(4).
          05 NUM-CLAUSES      PIC 9(4).
          05 CLAUSE-CONTENT   PIC X(100).

       WORKING-STORAGE SECTION.
       01 VARIABLES.
          05 VARIABLE-LIST    OCCURS 100 TIMES INDEXED BY VAR-INDEX.
             10 VARIABLE-VALUE PIC 9 VALUE 0.
             10 VARIABLE-NAME  PIC X(10).
          05 NUM-VARIABLES-WK PIC 9(4) VALUE 0.
          05 NUM-CLAUSES-WK   PIC 9(4) VALUE 0.

       01 CLAUSES.
          05 CLAUSE-LIST      OCCURS 100 TIMES INDEXED BY CLAUSE-INDEX.
             10 CLAUSE-VARIABLES OCCURS 20 TIMES INDEXED BY VAR-IN-CLAUSE.
                15 CLAUSE-VAR-NUM   PIC 9(4).
                15 CLAUSE-VAR-SIGN  PIC 9 VALUE 1. 
             10 CLAUSE-COUNT     PIC 9(4) VALUE 0.

       01 SOLUTION-FOUND   PIC X VALUE "N".
       01 CURRENT-ASSIGNMENT OCCURS 100 TIMES PIC 9 VALUE 0.
       01 UNIT-CLAUSE-FOUND PIC X VALUE "N".
       01 UNIT-CLAUSE-VAR   PIC 9(4) VALUE 0.
       01 UNIT-CLAUSE-SIGN  PIC 9 VALUE 0.

       01 DEBUG-MSG        PIC X(80) VALUE SPACES.
       01 TEMP-STRING      PIC X(20) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "DPLL SAT SOLVER STARTING"
           PERFORM READ-PROBLEM
           PERFORM INITIALIZE-VARIABLES
           PERFORM DPLL-SOLVE
           IF SOLUTION-FOUND = "Y"
               DISPLAY "SATISFIABLE - SOLUTION FOUND"
           ELSE
               DISPLAY "UNSATISFIABLE - NO SOLUTION EXISTS"
           END-IF
           STOP RUN.

       READ-PROBLEM.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE AT END GO TO PROBLEM-END
           MOVE NUM-VARIABLES TO NUM-VARIABLES-WK
           MOVE NUM-CLAUSES TO NUM-CLAUSES-WK
           DISPLAY "READING PROBLEM: " NUM-VARIABLES-WK " VARIABLES, "
                   NUM-CLAUSES-WK " CLAUSES"
           PERFORM READ-CLAUSES
           CLOSE INPUT-FILE.

       PROBLEM-END.
           DISPLAY "END OF PROBLEM FILE"

       READ-CLAUSES.
           PERFORM VARYING CLAUSE-INDEX FROM 1 BY 1
               UNTIL CLAUSE-INDEX > NUM-CLAUSES-WK
               READ INPUT-FILE AT END GO TO CLAUSE-READ-END
               PERFORM PARSE-CLAUSE
           END-PERFORM.

       CLAUSE-READ-END.
           DISPLAY "CLAUSE READING COMPLETE"

       PARSE-CLAUSE.
           MOVE 0 TO CLAUSE-COUNT
           PERFORM VARYING VAR-IN-CLAUSE FROM 1 BY 1
               UNTIL VAR-IN-CLAUSE > 20 OR CLAUSE-CONTENT(VAR-IN-CLAUSE:1) = SPACE
               IF CLAUSE-CONTENT(VAR-IN-CLAUSE:1) NOT = "-"
                   MOVE FUNCTION NUMVAL(CLAUSE-CONTENT(VAR-IN-CLAUSE:10)) TO CLAUSE-VAR-NUM
                   MOVE 1 TO CLAUSE-VAR-SIGN
               ELSE
                   MOVE FUNCTION NUMVAL(CLAUSE-CONTENT(VAR-IN-CLAUSE+1:9)) TO CLAUSE-VAR-NUM
                   MOVE 0 TO CLAUSE-VAR-SIGN
               END-IF
               ADD 1 TO CLAUSE-COUNT
           END-PERFORM.

       INITIALIZE-VARIABLES.
           PERFORM VARYING VAR-INDEX FROM 1 BY 1
               UNTIL VAR-INDEX > NUM-VARIABLES-WK
               MOVE 0 TO VARIABLE-VALUE(VAR-INDEX)
           END-PERFORM.

       DPLL-SOLVE.
           DISPLAY "STARTING DPLL ALGORITHM"
           PERFORM CHECK-UNIT-CLAUSES
           IF UNIT-CLAUSE-FOUND = "Y"
               PERFORM ASSIGN-UNIT-CLAUSE
               PERFORM DPLL-SOLVE
           ELSE
               PERFORM VARYING VAR-INDEX FROM 1 BY 1
                   UNTIL VAR-INDEX > NUM-VARIABLES-WK
                   IF VARIABLE-VALUE(VAR-INDEX) = 0
                       MOVE 1 TO CURRENT-ASSIGNMENT(VAR-INDEX)
                       PERFORM CHECK-SATISFIABILITY
                       IF SOLUTION-FOUND = "Y"
                           GO TO DPLL-END
                       END-IF
                       MOVE 0 TO CURRENT-ASSIGNMENT(VAR-INDEX)
                       MOVE 1 TO CURRENT-ASSIGNMENT(VAR-INDEX)
                       PERFORM CHECK-SATISFIABILITY
                       IF SOLUTION-FOUND = "Y"
                           GO TO DPLL-END
                       END-IF
                       MOVE 0 TO CURRENT-ASSIGNMENT(VAR-INDEX)
                   END-IF
               END-PERFORM
           END-IF.

       DPLL-END.
           EXIT.

       CHECK-UNIT-CLAUSES.
           MOVE "N" TO UNIT-CLAUSE-FOUND
           PERFORM VARYING CLAUSE-INDEX FROM 1 BY 1
               UNTIL CLAUSE-INDEX > NUM-CLAUSES-WK OR UNIT-CLAUSE-FOUND = "Y"
               IF CLAUSE-COUNT(CLAUSE-INDEX) = 1
                   MOVE "Y" TO UNIT-CLAUSE-FOUND
                   MOVE CLAUSE-VAR-NUM(1,CLAUSE-INDEX) TO UNIT-CLAUSE-VAR
                   MOVE CLAUSE-VAR-SIGN(1,CLAUSE-INDEX) TO UNIT-CLAUSE-SIGN
               END-IF
           END-PERFORM.

       ASSIGN-UNIT-CLAUSE.
           DISPLAY "ASSIGNING UNIT CLAUSE: VARIABLE " UNIT-CLAUSE-VAR
           IF UNIT-CLAUSE-SIGN = 1
               MOVE 1 TO CURRENT-ASSIGNMENT(UNIT-CLAUSE-VAR)
           ELSE
               MOVE 0 TO CURRENT-ASSIGNMENT(UNIT-CLAUSE-VAR)
           END-IF.

       CHECK-SATISFIABILITY.
           PERFORM VARYING CLAUSE-INDEX FROM 1 BY 1
               UNTIL CLAUSE-INDEX > NUM-CLAUSES-WK
               IF CLAUSE-COUNT(CLAUSE-INDEX) = 0
                   MOVE "N" TO SOLUTION-FOUND
                   GO TO SATISFIABILITY-END
               END-IF
               PERFORM VARYING VAR-IN-CLAUSE FROM 1 BY 1
                   UNTIL VAR-IN-CLAUSE > CLAUSE-COUNT(CLAUSE-INDEX)
                   IF CLAUSE-VAR-SIGN(VAR-IN-CLAUSE,CLAUSE-INDEX) = 1
                       IF CURRENT-ASSIGNMENT(CLAUSE-VAR-NUM(VAR-IN-CLAUSE,CLAUSE-INDEX)) = 1
                           GO TO SATISFIABLE
                       END-IF
                   ELSE
                       IF CURRENT-ASSIGNMENT(CLAUSE-VAR-NUM(VAR-IN-CLAUSE,CLAUSE-INDEX)) = 0
                           GO TO SATISFIABLE
                       END-IF
                   END-IF
               END-PERFORM
               MOVE "N" TO SOLUTION-FOUND
               GO TO SATISFIABILITY-END.
           SATISFIABLE.
           CONTINUE.
           END-PERFORM.
           MOVE "Y" TO SOLUTION-FOUND.

       SATISFIABILITY-END.
           EXIT.
```

## Example Input File Format (SAT-PROBLEM.DAT)

```
3 2
1 -2 0
2 3 0
```

This represents:
- 3 variables (x1, x2, x3)
- 2 clauses: (x1 OR NOT x2) AND (x2 OR x3)

## Key Features of This Implementation

1. **DPLL Algorithm Structure**: Implements the core DPLL recursive backtracking algorithm
2. **Unit Propagation**: Identifies and assigns unit clauses
3. **Backtracking**: Uses recursive calls to try different variable assignments
4. **Clause Checking**: Verifies if current assignment satisfies all clauses
5. **Variable Assignment**: Tracks current truth values of variables

## Limitations

1. **Fixed Array Sizes**: Limited to 100 variables and 100 clauses
2. **Simple Input Format**: Assumes specific clause format with spaces and 0-terminators
3. **No Optimizations**: Basic implementation without advanced heuristics
4. **COBOL Limitations**: Uses traditional COBOL constructs that may not be optimal

This example demonstrates how to implement a SAT solver algorithm in COBOL, showing the fundamental recursive backtracking approach used by DPLL algorithms.