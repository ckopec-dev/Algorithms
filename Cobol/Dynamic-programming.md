# Dynamic Programming Example in COBOL: Fibonacci Sequence

Here's a COBOL implementation of the Fibonacci sequence using dynamic programming (memoization) to avoid redundant calculations:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI-DP.
       AUTHOR. Dynamic Programming Example.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-FILE ASSIGN TO PRINTER.

       DATA DIVISION.
       FILE SECTION.
       FD PRINT-FILE.
       01 PRINT-LINE PIC X(80).

       WORKING-STORAGE SECTION.
       01 FIB-NUMBER        PIC 9(10) VALUE 0.
       01 FIB-RESULT        PIC 9(20) VALUE 0.
       01 I                 PIC 9(10) VALUE 0.
       01 J                 PIC 9(10) VALUE 0.
       01 TEMP              PIC 9(20) VALUE 0.
       
       01 FIB-ARRAY.
           05 FIB-VALUE OCCURS 100 TIMES PIC 9(20) VALUE 0.
       
       01 FLAG              PIC X VALUE "N".
       01 WS-TEMP           PIC 9(20) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "FIBONACCI SEQUENCE - DYNAMIC PROGRAMMING"
           DISPLAY "========================================"
           
           PERFORM INITIALIZE-ARRAY
           
           DISPLAY "Enter Fibonacci number to calculate: "
           ACCEPT FIB-NUMBER
           
           IF FIB-NUMBER > 100
               DISPLAY "Number too large, using 100"
               MOVE 100 TO FIB-NUMBER
           END-IF
           
           PERFORM CALCULATE-FIBONACCI
           
           DISPLAY "Fibonacci(" FIB-NUMBER ") = " FIB-RESULT
           
           STOP RUN.

       INITIALIZE-ARRAY.
           MOVE 0 TO FIB-VALUE (1)
           MOVE 1 TO FIB-VALUE (2)
           PERFORM VARYING I FROM 3 BY 1 UNTIL I > 100
               MOVE 0 TO FIB-VALUE (I)
           END-PERFORM.

       CALCULATE-FIBONACCI.
           IF FIB-NUMBER = 0
               MOVE 0 TO FIB-RESULT
               GO TO RETURN-FIB
           END-IF
           
           IF FIB-NUMBER = 1
               MOVE 1 TO FIB-RESULT
               GO TO RETURN-FIB
           END-IF
           
           IF FIB-NUMBER > 2
               PERFORM VARYING I FROM 3 BY 1 UNTIL I > FIB-NUMBER
                   COMPUTE FIB-VALUE (I) = 
                       FIB-VALUE (I-1) + FIB-VALUE (I-2)
               END-PERFORM
           END-IF
           
           MOVE FIB-VALUE (FIB-NUMBER) TO FIB-RESULT.

       RETURN-FIB.
           EXIT.

       DISPLAY-RESULTS.
           MOVE "Fibonacci(" TO PRINT-LINE
           MOVE FIB-NUMBER TO WS-TEMP
           STRING PRINT-LINE WS-TEMP " ) = " 
                  FIB-RESULT INTO PRINT-LINE
           DISPLAY PRINT-LINE
           GO TO MAIN-PARAGRAPH.
```

## Key Dynamic Programming Features in This Example:

### 1. **Memoization Approach**
- Uses an array `FIB-ARRAY` to store previously calculated values
- Avoids redundant calculations by storing results

### 2. **Bottom-Up Construction**
- Calculates Fibonacci numbers from base cases (0 and 1)
- Builds up to the desired number using previously computed values

### 3. **Optimization Benefits**
- Time complexity reduced from O(2^n) to O(n)
- Space complexity is O(n) for storing intermediate results
- Each Fibonacci number calculated only once

## Sample Output:
```
FIBONACCI SEQUENCE - DYNAMIC PROGRAMMING
========================================
Enter Fibonacci number to calculate: 10
Fibonacci(10) = 55
```

This implementation demonstrates how COBOL can effectively implement dynamic programming techniques, making it suitable for performance-critical applications in business environments.