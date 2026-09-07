# Clarke-Wright Savings Algorithm in COBOL

Here's a COBOL implementation of the Clarke-Wright savings algorithm for vehicle routing problems:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLARKE-WRIGHT-SAVINGS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "CUSTOMERS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "RESULTS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       01  CUSTOMER-RECORD.
           05  CUST-NUMBER     PIC 9(4).
           05  CUST-X          PIC 9(4).
           05  CUST-Y          PIC 9(4).

       FD  OUTPUT-FILE.
       01  OUTPUT-LINE         PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-CUSTOMER-COUNT   PIC 9(3) VALUE 0.
       01  WS-DISTANCE-MATRIX.
           05  DIST-ROW OCCURS 50 TIMES.
               10  DIST-COL OCCURS 50 TIMES PIC 9(4).
       01  WS-SAVINGS-MATRIX.
           05  SAVINGS-ROW OCCURS 50 TIMES.
               10  SAVINGS-COL OCCURS 50 TIMES PIC 9(4)V99.
       01  WS-ROUTE-LIST.
           05  ROUTE-ENTRY OCCURS 50 TIMES.
               10  ROUTE-NUMBER PIC 9(3).
               10  ROUTE-CUSTOMERS OCCURS 20 TIMES PIC 9(4).
       01  WS-ROUTE-COUNT      PIC 9(3) VALUE 0.
       01  WS-ROUTED-CUSTOMERS.
           05  CUSTOMER-FLAG OCCURS 50 TIMES PIC 9 VALUE 0.
       01  WS-SAVINGS-THRESHOLD PIC 9(4)V99 VALUE 0.00.
       
       01  WS-TEMP-VARIABLES.
           05  I                   PIC 9(3) VALUE 0.
           05  J                   PIC 9(3) VALUE 0.
           05  K                   PIC 9(3) VALUE 0.
           05  WS-DISTANCE-I-J     PIC 9(4)V99.
           05  WS-DISTANCE-I-0     PIC 9(4)V99.
           05  WS-DISTANCE-0-J     PIC 9(4)V99.
           05  WS-SAVINGS          PIC 9(4)V99.
           05  WS-CURRENT-BEST     PIC 9(4)V99 VALUE 9999.99.
           05  WS-FOUND            PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "CLARKE-WRIGHT SAVINGS ALGORITHM"
           DISPLAY "================================"

           PERFORM INITIALIZE-DATA
           PERFORM CALCULATE-DISTANCES
           PERFORM CALCULATE-SAVINGS
           PERFORM SORT-SAVINGS
           PERFORM BUILD-ROUTES

           DISPLAY "ROUTE BUILDING COMPLETED"
           STOP RUN.

       INITIALIZE-DATA.
           OPEN INPUT CUSTOMER-FILE
           READ CUSTOMER-FILE AT END GO TO EOF-PROCESS
           MOVE 1 TO WS-CUSTOMER-COUNT
           MOVE 0 TO I
           MOVE 0 TO J

       READ-LOOP.
           IF CUSTOMER-RECORD NOT = SPACES THEN
               ADD 1 TO WS-CUSTOMER-COUNT
           ELSE
               GO TO EOF-PROCESS
           END-IF
           READ CUSTOMER-FILE AT END GO TO EOF-PROCESS
           GO TO READ-LOOP.

       EOF-PROCESS.
           CLOSE CUSTOMER-FILE.

       CALCULATE-DISTANCES.
           DISPLAY "Calculating distance matrix..."
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-CUSTOMER-COUNT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CUSTOMER-COUNT
                   IF I = J THEN
                       MOVE 0 TO DIST-ROW(I) (J)
                   ELSE
                       COMPUTE WS-DISTANCE-I-J =
                           FUNCTION SQRT ((CUST-X (I) - CUST-X (J)) ** 2 +
                                          (CUST-Y (I) - CUST-Y (J)) ** 2)
                       MOVE WS-DISTANCE-I-J TO DIST-ROW(I) (J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       CALCULATE-SAVINGS.
           DISPLAY "Calculating savings matrix..."
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > WS-CUSTOMER-COUNT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > I - 1
                   COMPUTE WS-DISTANCE-I-0 = DIST-ROW(I) (1)
                   COMPUTE WS-DISTANCE-0-J = DIST-ROW(1) (J)
                   COMPUTE WS-SAVINGS = WS-DISTANCE-I-0 + WS-DISTANCE-0-J -
                                        DIST-ROW(I) (J)
                   MOVE WS-SAVINGS TO SAVINGS-ROW(J) (I)
               END-PERFORM
           END-PERFORM.

       SORT-SAVINGS.
           DISPLAY "Sorting savings in descending order..."
           * This would be a sorting routine to sort by savings values
           * For simplicity, we'll assume the savings are pre-sorted or use
           * external sorting method.

       BUILD-ROUTES.
           DISPLAY "Building routes using savings algorithm..."
           MOVE 0 TO WS-ROUTE-COUNT

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-CUSTOMER-COUNT - 1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CUSTOMER-COUNT - 1
                   IF SAVINGS-ROW(J) (I) > WS-SAVINGS-THRESHOLD THEN
                       PERFORM CHECK-AND-MERGE I J
                   END-IF
               END-PERFORM
           END-PERFORM.

       CHECK-AND-MERGE.
           * This is a simplified version of the merge logic
           DISPLAY "Checking merge for customers ", I, " and ", J
           IF CUSTOMER-FLAG(I) = 0 AND CUSTOMER-FLAG(J) = 0 THEN
               PERFORM MERGE-ROUTES I J
           END-IF.

       MERGE-ROUTES.
           * Simplified merging logic - in practice this would be more complex
           DISPLAY "Merging customers ", I, " and ", J
           MOVE 1 TO CUSTOMER-FLAG(I)
           MOVE 1 TO CUSTOMER-FLAG(J)
           ADD 1 TO WS-ROUTE-COUNT.

       END PROGRAM CLARKE-WRIGHT-SAVINGS.
```

## Key Features of this Implementation:

### 1. **Data Structure**
- Customer file with coordinates (X, Y)
- Distance matrix for all customer pairs
- Savings matrix calculation
- Route tracking and merging

### 2. **Algorithm Steps**
1. **Initialize** - Read customer data
2. **Calculate Distances** - Compute Euclidean distances between all customers
3. **Compute Savings** - Apply Clarke-Wright formula: S(i,j) = d(0,i) + d(0,j) - d(i,j)
4. **Sort Savings** - Rank savings in descending order
5. **Build Routes** - Merge routes based on highest savings

### 3. **Key Variables**
- `DIST-ROW` and `DIST-COL`: Distance matrix storage
- `SAVINGS-ROW` and `SAVINGS-COL`: Savings calculation matrix
- `CUSTOMER-FLAG`: Tracks which customers are already routed
- `WS-ROUTE-COUNT`: Number of routes formed

### 4. **Important Notes**
- This is a simplified version - full implementation would include route merging logic
- The sorting routine needs to be enhanced for proper savings ranking
- Route validation and capacity constraints are not included in this basic example
- Error handling and file I/O validation should be added for production use

This COBOL implementation demonstrates the core principles of the Clarke-Wright savings algorithm while respecting the structured programming style typical of COBOL applications.