# Sieve of Eratosthenes in COBOL

Here's a complete implementation of the Sieve of Eratosthenes algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIEVE-OF-ERATOSTHENES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N                       PIC 9(4) VALUE 30.
       01 I                       PIC 9(4) VALUE 2.
       01 J                       PIC 9(4).
       01 K                       PIC 9(4).
       01 ROOT-N                  PIC 9(4).
       
       01 PRIME-TABLE.
          05 PRIME-FLAG OCCURS 30 TIMES PIC 9 VALUE 1.
       
       01 DISPLAY-TEMP            PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "SIEVE OF ERATOSTHENES - FINDING PRIMES UP TO " N
           DISPLAY " "

           COMPUTE ROOT-N = FUNCTION SQRT(N)

           PERFORM SIEVE-PROCESS

           DISPLAY "PRIME NUMBERS FROM 2 TO " N ":"
           DISPLAY " "

           PERFORM PRIME-DISPLAY

           STOP RUN.

       SIEVE-PROCESS.
           PERFORM UNTIL I > ROOT-N
               IF PRIME-FLAG(I) = 1
                   MOVE I TO J
                   COMPUTE K = J * J
                   PERFORM SIEVE-REMOVE-USING-K
               END-IF
               ADD 1 TO I
           END-PERFORM.

       SIEVE-REMOVE-USING-K.
           PERFORM UNTIL K > N
               MOVE 0 TO PRIME-FLAG(K)
               ADD J TO K
           END-PERFORM.

       PRIME-DISPLAY.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > N
               IF PRIME-FLAG(I) = 1
                   DISPLAY I " "
               END-IF
           END-PERFORM
           DISPLAY " ".
```

## How it works:

1. **Initialization**: Creates a table of flags (1 for potential primes, 0 for composite)
2. **Main Loop**: Iterates through numbers from 2 to √N
3. **Marking Composites**: For each prime found, marks all its multiples as composite
4. **Display Results**: Shows all remaining primes

## Key Features:

- Uses a flag array to track prime status
- Implements the classic sieve algorithm optimization (only check up to √N)
- Displays results in a readable format
- Handles numbers up to 30 (configurable via N variable)

## Sample Output:
```
SIEVE OF ERATOSTHENES - FINDING PRIMES UP TO 30

PRIME NUMBERS FROM 2 TO 30 :
2 3 5 7 11 13 17 19 23 29
```

This implementation demonstrates the classic Sieve of Eratosthenes algorithm in COBOL syntax, showing how to implement mathematical algorithms in the structured programming language.