# Traveling Salesman Problem (TSP) in COBOL

Here's a COBOL implementation of a simplified TSP algorithm using a brute force approach to find the shortest route:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSP-SOLUTION.
       AUTHOR. AI Assistant.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TOUR-FILE ASSIGN TO "tour-data.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TOUR-FILE.
       01  TOUR-RECORD.
           05  TOUR-ID            PIC 9(3).
           05  CITY-1             PIC 9(2).
           05  CITY-2             PIC 9(2).
           05  DISTANCE           PIC 9(4).

       WORKING-STORAGE SECTION.
       01  WS-TOTAL-CITIES    PIC 9(2) VALUE 5.
       01  WS-DISTANCE-MATRIX.
           05  DIST-ROWS OCCURS 5 TIMES.
               10  DIST-COLS OCCURS 5 TIMES PIC 9(3).
       01  WS-PERMUTATION.
           05  PERM-ITEMS OCCURS 5 TIMES PIC 9(2).
       01  WS-TEMP-PERMUTATION.
           05  TEMP-ITEMS OCCURS 5 TIMES PIC 9(2).
       01  WS-ROUTE-DISTANCE  PIC 9(4) VALUE 0.
       01  WS-MIN-DISTANCE    PIC 9(4) VALUE 9999.
       01  WS-MIN-ROUTE.
           05  MIN-ROUTE-ITEMS OCCURS 5 TIMES PIC 9(2).
       01  WS-INDEX           PIC 9(2) VALUE 1.
       01  WS-I               PIC 9(2).
       01  WS-J               PIC 9(2).
       01  WS-K               PIC 9(2).
       01  WS-TEMP            PIC 9(2).
       01  WS-FOUND           PIC X VALUE 'N'.
       01  WS-RESULT          PIC X VALUE 'N'.
       01  WS-START-CITY      PIC 9(2) VALUE 1.
       01  WS-STOP-FLAG       PIC X VALUE 'N'.
       01  WS-OUTPUT-STRING   PIC X(80).
       01  WS-TEMP-STRING     PIC X(10).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "TRAVELING SALESMAN PROBLEM SOLUTION"
           DISPLAY "====================================="
           
           PERFORM INITIALIZE-DISTANCE-MATRIX
           PERFORM GENERATE-ALL-PERMUTATIONS
           PERFORM DISPLAY-RESULTS
           
           STOP RUN.

       INITIALIZE-DISTANCE-MATRIX.
           MOVE 0 TO DIST-ROWS(1) DIST-ROWS(2) DIST-ROWS(3)
               DIST-ROWS(4) DIST-ROWS(5)
           
           MOVE 0 TO DIST-COLS(1,1) DIST-COLS(2,2) DIST-COLS(3,3)
               DIST-COLS(4,4) DIST-COLS(5,5)
           
           MOVE 0 TO DIST-COLS(1,2) DIST-COLS(2,1)
           MOVE 0 TO DIST-COLS(1,3) DIST-COLS(3,1)
           MOVE 0 TO DIST-COLS(1,4) DIST-COLS(4,1)
           MOVE 0 TO DIST-COLS(1,5) DIST-COLS(5,1)
           MOVE 0 TO DIST-COLS(2,3) DIST-COLS(3,2)
           MOVE 0 TO DIST-COLS(2,4) DIST-COLS(4,2)
           MOVE 0 TO DIST-COLS(2,5) DIST-COLS(5,2)
           MOVE 0 TO DIST-COLS(3,4) DIST-COLS(4,3)
           MOVE 0 TO DIST-COLS(3,5) DIST-COLS(5,3)
           MOVE 0 TO DIST-COLS(4,5) DIST-COLS(5,4)
           
           MOVE 0 TO DIST-COLS(1,2) DIST-COLS(2,1) VALUE 10
           MOVE 0 TO DIST-COLS(1,3) DIST-COLS(3,1) VALUE 15
           MOVE 0 TO DIST-COLS(1,4) DIST-COLS(4,1) VALUE 20
           MOVE 0 TO DIST-COLS(1,5) DIST-COLS(5,1) VALUE 25
           MOVE 0 TO DIST-COLS(2,3) DIST-COLS(3,2) VALUE 35
           MOVE 0 TO DIST-COLS(2,4) DIST-COLS(4,2) VALUE 25
           MOVE 0 TO DIST-COLS(2,5) DIST-COLS(5,2) VALUE 30
           MOVE 0 TO DIST-COLS(3,4) DIST-COLS(4,3) VALUE 30
           MOVE 0 TO DIST-COLS(3,5) DIST-COLS(5,3) VALUE 20
           MOVE 0 TO DIST-COLS(4,5) DIST-COLS(5,4) VALUE 15.

       GENERATE-ALL-PERMUTATIONS.
           PERFORM WITH TEST BEFORE
               VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-TOTAL-CITIES
               PERFORM INITIALIZE-PERMUTATION
               PERFORM PERMUTE-RECURSIVE
               PERFORM CALCULATE-ROUTE-DISTANCE
               PERFORM UPDATE-MINIMUM
               PERFORM DISPLAY-PROGRESS
           END-VARYING.

       INITIALIZE-PERMUTATION.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-TOTAL-CITIES
               MOVE WS-J TO PERM-ITEMS(WS-J)
           END-VARYING.

       PERMUTE-RECURSIVE.
           IF WS-I = WS-TOTAL-CITIES
               PERFORM CALCULATE-ROUTE-DISTANCE
               PERFORM UPDATE-MINIMUM
           ELSE
               PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J > WS-TOTAL-CITIES
                   PERFORM SWAP-ITEMS
                   PERFORM PERMUTE-RECURSIVE
                   PERFORM SWAP-ITEMS
               END-VARYING
           END-IF.

       SWAP-ITEMS.
           MOVE PERM-ITEMS(WS-I) TO WS-TEMP
           MOVE PERM-ITEMS(WS-J) TO PERM-ITEMS(WS-I)
           MOVE WS-TEMP TO PERM-ITEMS(WS-J).

       CALCULATE-ROUTE-DISTANCE.
           MOVE 0 TO WS-ROUTE-DISTANCE
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J >= WS-TOTAL-CITIES
               COMPUTE WS-ROUTE-DISTANCE = WS-ROUTE-DISTANCE
                   + DIST-COLS(PERM-ITEMS(WS-J), PERM-ITEMS(WS-J+1))
           END-VARYING
           COMPUTE WS-ROUTE-DISTANCE = WS-ROUTE-DISTANCE
               + DIST-COLS(PERM-ITEMS(WS-TOTAL-CITIES), WS-START-CITY).

       UPDATE-MINIMUM.
           IF WS-ROUTE-DISTANCE < WS-MIN-DISTANCE
               MOVE WS-ROUTE-DISTANCE TO WS-MIN-DISTANCE
               PERFORM COPY-PERMUTATION-TO-MIN
           END-IF.

       COPY-PERMUTATION-TO-MIN.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-TOTAL-CITIES
               MOVE PERM-ITEMS(WS-J) TO MIN-ROUTE-ITEMS(WS-J)
           END-VARYING.

       DISPLAY-PROGRESS.
           IF WS-I = 1 OR WS-I = 5 OR WS-I = 20 OR WS-I = 100
               DISPLAY "Progress: " WS-I " permutations checked"
           END-IF.

       DISPLAY-RESULTS.
           DISPLAY " "
           DISPLAY "OPTIMAL TOUR FOUND:"
           DISPLAY "---------------------"
           DISPLAY "Minimum Distance: " WS-MIN-DISTANCE
           DISPLAY "Optimal Route: "
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-TOTAL-CITIES
               STRING MIN-ROUTE-ITEMS(WS-J) " -> " INTO WS-OUTPUT-STRING
               DISPLAY WS-OUTPUT-STRING
           END-VARYING
           DISPLAY "Back to starting city: " WS-START-CITY.

       END PROGRAM TSP-SOLUTION.
```

## Key Features of This TSP Implementation:

1. **Distance Matrix**: 5x5 matrix representing distances between cities
2. **Brute Force Approach**: Generates all possible permutations of cities
3. **Optimization**: Keeps track of the minimum distance found
4. **Progress Tracking**: Shows progress during computation
5. **Result Display**: Shows optimal route and minimum distance

## How It Works:

1. **Initialization**: Sets up the distance matrix between cities
2. **Permutation Generation**: Creates all possible city visit orders
3. **Distance Calculation**: Computes total distance for each route
4. **Optimization**: Updates the best solution found so far
5. **Output**: Displays the optimal route and minimum distance

## Notes:

- This is a simplified version for demonstration purposes
- For larger datasets, more efficient algorithms like dynamic programming or heuristic approaches would be needed
- The time complexity is O(n! × n) for the brute force approach
- This implementation uses 5 cities for demonstration but can be easily modified

This COBOL program demonstrates fundamental TSP solving concepts while maintaining the structured approach typical of COBOL programming.

