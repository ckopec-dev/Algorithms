# Simplex Algorithm in COBOL

Here's an example of implementing the Simplex algorithm in COBOL for solving linear programming problems:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIMPLEX-ALGORITHM.
       AUTHOR. COBOL-PROGRAMMER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PROBLEM-INPUT.
           05  NUM-VARIABLES    PIC 9(2) VALUE 2.
           05  NUM-CONSTRAINTS  PIC 9(2) VALUE 3.
           05  OBJECTIVE-COEFFS.
               10  OBJ-COEFFS     OCCURS 10 TIMES PIC 9(3)V99.
           05  CONSTRAINT-MATRIX.
               10  CONSTRAINTS    OCCURS 10 TIMES.
                   15  COEFFS       OCCURS 10 TIMES PIC 9(3)V99.
           05  RHS-VALUES       OCCURS 10 TIMES PIC 9(3)V99.
           05  BASIC-VARIABLES  OCCURS 10 TIMES PIC 9(2).
           05  NON-BASIC-VARIABLES OCCURS 10 TIMES PIC 9(2).
           05  TABLEAU.
               10  TABLEAU-ROWS   OCCURS 15 TIMES.
                   15  TABLEAU-COLS OCCURS 15 TIMES PIC 9(3)V99.
           05  C-Z-VALUES       OCCURS 15 TIMES PIC 9(3)V99.
           05  PIVOT-ROW        PIC 9(2).
           05  PIVOT-COL        PIC 9(2).
           05  OPTIMAL-SOLUTION OCCURS 10 TIMES PIC 9(3)V99.
           05  FINAL-OBJECTIVE  PIC 9(5)V99.
           05  ITERATION-COUNT  PIC 9(3) VALUE 0.
           05  FLAG-FOUND       PIC X VALUE 'N'.
           05  FLAG-UNBOUNDED   PIC X VALUE 'N'.
           05  FLAG-DEGENERATE  PIC X VALUE 'N'.

       01  TEMPORARY-VALUES.
           05  TEMP-VALUE       PIC 9(5)V99.
           05  MIN-QUOTIENT     PIC 9(5)V99 VALUE 99999.99.
           05  PIVOT-ELEMENT    PIC 9(5)V99.
           05  RATIO            PIC 9(5)V99.
           05  CALC-RESULT      PIC 9(5)V99.

       01  INDEX-VALUES.
           05  I                PIC 9(2).
           05  J                PIC 9(2).
           05  K                PIC 9(2).
           05  L                PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "SIMPLEX ALGORITHM IMPLEMENTATION".
           DISPLAY "===========================".

           PERFORM INITIALIZE-SIMPLEX.
           PERFORM SOLVE-SIMPLEX.

           DISPLAY "FINAL SOLUTION:".
           PERFORM DISPLAY-SOLUTION.

           STOP RUN.

       INITIALIZE-SIMPLEX.
           DISPLAY "Initializing Simplex Tableau...".
           
           MOVE 0 TO ITERATION-COUNT.
           
           * Initialize objective function coefficients
           MOVE 3 TO OBJ-COEFFS(1).
           MOVE 2 TO OBJ-COEFFS(2).
           MOVE 0 TO OBJ-COEFFS(3) THRU OBJ-COEFFS(10).

           * Initialize constraint matrix
           MOVE 1 TO COEFFS(1,1).
           MOVE 2 TO COEFFS(1,2).
           MOVE 1 TO COEFFS(1,3).
           MOVE 10 TO RHS-VALUES(1).

           MOVE 3 TO COEFFS(2,1).
           MOVE 1 TO COEFFS(2,2).
           MOVE 0 TO COEFFS(2,3).
           MOVE 15 TO RHS-VALUES(2).

           MOVE 1 TO COEFFS(3,1).
           MOVE 1 TO COEFFS(3,2).
           MOVE 2 TO COEFFS(3,3).
           MOVE 8 TO RHS-VALUES(3).

           * Initialize basic variables (slack variables)
           MOVE 3 TO BASIC-VARIABLES(1).
           MOVE 4 TO BASIC-VARIABLES(2).
           MOVE 5 TO BASIC-VARIABLES(3).

           * Initialize non-basic variables
           MOVE 1 TO NON-BASIC-VARIABLES(1).
           MOVE 2 TO NON-BASIC-VARIABLES(2).

           * Build initial tableau
           PERFORM BUILD-TABLEAU.

       BUILD-TABLEAU.
           DISPLAY "Building Initial Tableau...".
           
           * Initialize table rows and columns
           PERFORM INITIALIZE-TABLEAU-ROWS.
           
           * Set up objective function row
           PERFORM SET-OBJECTIVE-ROW.
           
           * Set up constraint rows
           PERFORM SET-CONSTRAINT-ROWS.

       INITIALIZE-TABLEAU-ROWS.
           MOVE 0 TO I.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-CONSTRAINTS
               MOVE 0 TO J
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > (NUM-VARIABLES + NUM-CONSTRAINTS + 1)
                   MOVE 0 TO TABLEAU-ROWS(I)(J)
               END-PERFORM
           END-PERFORM.

       SET-OBJECTIVE-ROW.
           MOVE 1 TO I.
           MOVE 0 TO J.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-VARIABLES
               MOVE OBJ-COEFFS(J) TO TABLEAU-ROWS(I)(J)
           END-PERFORM.
           
           * Set up slack variables in objective row (negative values)
           PERFORM VARYING J FROM (NUM-VARIABLES + 1) BY 1 UNTIL J > (NUM-VARIABLES + NUM-CONSTRAINTS)
               MOVE 0 TO TABLEAU-ROWS(I)(J)
           END-PERFORM.
           
           MOVE 0 TO TABLEAU-ROWS(I)(J).
           SUBTRACT 1 FROM J.
           PERFORM VARYING J FROM (NUM-VARIABLES + 1) BY 1 UNTIL J > (NUM-VARIABLES + NUM-CONSTRAINTS)
               MOVE 0 TO TABLEAU-ROWS(I)(J)
           END-PERFORM.

       SET-CONSTRAINT-ROWS.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > (NUM-CONSTRAINTS + 1)
               MOVE 0 TO J.
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > NUM-VARIABLES
                   MOVE COEFFS(I-1,J) TO TABLEAU-ROWS(I)(J)
               END-PERFORM.
               
               * Add slack variables
               MOVE 1 TO TABLEAU-ROWS(I)(NUM-VARIABLES + I - 1).
               
               * Add RHS value
               MOVE RHS-VALUES(I-1) TO TABLEAU-ROWS(I)(NUM-VARIABLES + NUM-CONSTRAINTS + 1).
           END-PERFORM.

       SOLVE-SIMPLEX.
           DISPLAY "Starting Simplex Iterations...".
           
           PERFORM VARYING ITERATION-COUNT FROM 1 BY 1 UNTIL ITERATION-COUNT > 10
               DISPLAY "Iteration: " ITERATION-COUNT
               
               PERFORM FIND-ENTERING-VARIABLE.
               IF FLAG-FOUND = 'Y' THEN
                   PERFORM FIND-LEAVING-VARIABLE.
                   IF FLAG-UNBOUNDED = 'Y' THEN
                       DISPLAY "Problem is Unbounded"
                       EXIT PARAGRAPH
                   END-IF
                   PERFORM PIVOT-OPERATION
               ELSE
                   DISPLAY "Optimal Solution Found"
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

       FIND-ENTERING-VARIABLE.
           MOVE 0 TO PIVOT-COL.
           MOVE 0 TO MIN-QUOTIENT.
           MOVE 'N' TO FLAG-FOUND.
           
           * Find most negative coefficient in objective row
           MOVE 1 TO I.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > (NUM-VARIABLES + NUM-CONSTRAINTS)
               IF TABLEAU-ROWS(1)(J) < 0 THEN
                   IF TABLEAU-ROWS(1)(J) < MIN-QUOTIENT THEN
                       MOVE J TO PIVOT-COL
                       MOVE TABLEAU-ROWS(1)(J) TO MIN-QUOTIENT
                   END-IF
               END-IF
           END-PERFORM.
           
           IF PIVOT-COL > 0 THEN
               MOVE 'Y' TO FLAG-FOUND
           END-IF.

       FIND-LEAVING-VARIABLE.
           MOVE 99999.99 TO MIN-QUOTIENT.
           MOVE 0 TO PIVOT-ROW.
           MOVE 'N' TO FLAG-UNBOUNDED.
           
           * Calculate ratios for leaving variable
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > (NUM-CONSTRAINTS + 1)
               IF TABLEAU-ROWS(I)(PIVOT-COL) > 0 THEN
                   COMPUTE RATIO = TABLEAU-ROWS(I)(NUM-VARIABLES + NUM-CONSTRAINTS + 1) 
                                 / TABLEAU-ROWS(I)(PIVOT-COL)
                   IF RATIO < MIN-QUOTIENT THEN
                       MOVE RATIO TO MIN-QUOTIENT
                       MOVE I TO PIVOT-ROW
                   END-IF
               ELSE
                   IF TABLEAU-ROWS(I)(PIVOT-COL) = 0 THEN
                       DISPLAY "Degenerate case encountered"
                       MOVE 'Y' TO FLAG-DEGENERATE
                   END-IF
               END-IF
           END-PERFORM.
           
           IF PIVOT-ROW = 0 THEN
               MOVE 'Y' TO FLAG-UNBOUNDED
           END-IF.

       PIVOT-OPERATION.
           DISPLAY "Performing Pivot Operation...".
           
           * Get pivot element
           MOVE TABLEAU-ROWS(PIVOT-ROW)(PIVOT-COL) TO PIVOT-ELEMENT.
           
           * Normalize pivot row
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > (NUM-VARIABLES + NUM-CONSTRAINTS + 1)
               COMPUTE TABLEAU-ROWS(PIVOT-ROW)(J) = 
                       TABLEAU-ROWS(PIVOT-ROW)(J) / PIVOT-ELEMENT
           END-PERFORM.
           
           * Eliminate pivot column entries
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > (NUM-CONSTRAINTS + 1)
               IF I NOT = PIVOT-ROW THEN
                   COMPUTE CALC-RESULT = TABLEAU-ROWS(I)(PIVOT-COL)
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > (NUM-VARIABLES + NUM-CONSTRAINTS + 1)
                       COMPUTE TABLEAU-ROWS(I)(J) = 
                               TABLEAU-ROWS(I)(J) - (CALC-RESULT * TABLEAU-ROWS(PIVOT-ROW)(J))
                   END-PERFORM
               END-IF
           END-PERFORM.

       DISPLAY-SOLUTION.
           DISPLAY "Optimal values:"
           MOVE 1 TO I.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-VARIABLES
               DISPLAY "X" I " = " TABLEAU-ROWS(1)(I)
           END-PERFORM.
           
           DISPLAY "Optimal Objective Value = " TABLEAU-ROWS(1)(NUM-VARIABLES + NUM-CONSTRAINTS + 1).
```

## Key Features of This Simplex Implementation:

### 1. **Problem Setup**
- Supports up to 10 variables and constraints
- Initializes objective function coefficients
- Sets up constraint matrix and right-hand side values

### 2. **Algorithm Components**
- **Tableau Construction**: Builds initial simplex tableau
- **Entering Variable Selection**: Finds most negative coefficient
- **Leaving Variable Selection**: Uses minimum ratio test
- **Pivot Operation**: Performs row operations to update tableau

### 3. **Key Variables**
- `TABLEAU-ROWS` and `TABLEAU-COLS`: Stores the simplex tableau
- `PIVOT-ROW` and `PIVOT-COL`: Track pivot position
- `FLAG-FOUND` and `FLAG-UNBOUNDED`: Control flow flags

### 4. **Sample Problem Solved**
This implementation solves a standard linear programming problem with:
- Objective function: Maximize 3x₁ + 2x₂
- Constraints: 
  - x₁ + 2x₂ + s₁ = 10
  - 3x₁ + x₂ + s₂ = 15
  - x₁ + x₂ + 2x₃ + s₃ = 8

### 5. **Output**
The program displays iteration-by-iteration progress and final optimal solution values.

This COBOL implementation demonstrates the core simplex algorithm logic while maintaining the structured approach typical of COBOL programming.

