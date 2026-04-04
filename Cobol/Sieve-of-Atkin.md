# Sieve of Atkin Algorithm in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIEVE-OF-ATKIN.
       AUTHOR. Generated Example.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  N                   PIC 9(6) VALUE 1000.
       01  I                   PIC 9(6).
       01  J                   PIC 9(6).
       01  K                   PIC 9(6).
       01  X                   PIC 9(6).
       01  Y                   PIC 9(6).
       01  Z                   PIC 9(6).
       01  TEMP                PIC 9(6).
       01  FLAG                PIC 9 VALUE 0.
       01  COUNT               PIC 9(4) VALUE 0.
       01  PRIME-FLAG          PIC X(1000) VALUE SPACES.
       01  PRIME-ARRAY         PIC 9(6) OCCURS 1000 TIMES.
       01  PRIME-COUNT         PIC 9(4) VALUE 0.
       01  DISPLAY-PRIME       PIC 9(6).
       01  DISPLAY-COUNT       PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Sieve of Atkin Algorithm"
           DISPLAY "Finding primes up to: " N
           DISPLAY " "

           PERFORM INITIALIZE-FLAGS
           PERFORM ATKIN-ALGORITHM
           PERFORM DISPLAY-PRIMES
           STOP RUN.

       INITIALIZE-FLAGS.
           MOVE "Y" TO PRIME-FLAG(1:1)
           MOVE "N" TO PRIME-FLAG(2:1)
           MOVE "N" TO PRIME-FLAG(3:1)
           MOVE "N" TO PRIME-FLAG(4:1)
           MOVE "N" TO PRIME-FLAG(5:1)
           MOVE "N" TO PRIME-FLAG(6:1)
           MOVE "N" TO PRIME-FLAG(7:1)
           MOVE "N" TO PRIME-FLAG(8:1)
           MOVE "N" TO PRIME-FLAG(9:1)
           MOVE "N" TO PRIME-FLAG(10:1)

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               IF I > 10
                   MOVE "N" TO PRIME-FLAG(I:1)
               END-IF
           END-PERFORM.

       ATKIN-ALGORITHM.
           PERFORM ATKIN-STEP-1
           PERFORM ATKIN-STEP-2
           PERFORM ATKIN-STEP-3
           PERFORM ATKIN-STEP-4
           PERFORM ATKIN-STEP-5.

       ATKIN-STEP-1.
           MOVE 1 TO I
           PERFORM VARYING X FROM 1 BY 1 UNTIL X * X > N
               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y * Y > N
                   COMPUTE Z = (4 * X * X) + (Y * Y)
                   IF Z <= N
                       IF FUNCTION MOD(Z, 12) = 1 OR FUNCTION MOD(Z, 12) = 5
                           IF PRIME-FLAG(Z:1) = "N"
                               MOVE "Y" TO PRIME-FLAG(Z:1)
                           ELSE
                               MOVE "N" TO PRIME-FLAG(Z:1)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       ATKIN-STEP-2.
           MOVE 1 TO I
           PERFORM VARYING X FROM 1 BY 1 UNTIL X * X > N
               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y * Y > N
                   COMPUTE Z = (3 * X * X) + (Y * Y)
                   IF Z <= N
                       IF FUNCTION MOD(Z, 12) = 7
                           IF PRIME-FLAG(Z:1) = "N"
                               MOVE "Y" TO PRIME-FLAG(Z:1)
                           ELSE
                               MOVE "N" TO PRIME-FLAG(Z:1)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       ATKIN-STEP-3.
           MOVE 1 TO I
           PERFORM VARYING X FROM 1 BY 1 UNTIL X * X > N
               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y * Y > N
                   COMPUTE Z = (3 * X * X) - (Y * Y)
                   IF Z <= N AND X > Y
                       IF FUNCTION MOD(Z, 12) = 11
                           IF PRIME-FLAG(Z:1) = "N"
                               MOVE "Y" TO PRIME-FLAG(Z:1)
                           ELSE
                               MOVE "N" TO PRIME-FLAG(Z:1)
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       ATKIN-STEP-4.
           PERFORM VARYING I FROM 5 BY 1 UNTIL I * I > N
               IF PRIME-FLAG(I:1) = "Y"
                   PERFORM VARYING J FROM I * I BY I * I UNTIL J > N
                       MOVE "N" TO PRIME-FLAG(J:1)
                   END-PERFORM
               END-IF
           END-PERFORM.

       ATKIN-STEP-5.
           MOVE 1 TO PRIME-COUNT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               IF PRIME-FLAG(I:1) = "Y"
                   MOVE I TO PRIME-ARRAY(PRIME-COUNT)
                   ADD 1 TO PRIME-COUNT
               END-IF
           END-PERFORM.

       DISPLAY-PRIMES.
           DISPLAY "Prime numbers up to " N ":"
           DISPLAY " "

           PERFORM VARYING DISPLAY-COUNT FROM 1 BY 1 UNTIL DISPLAY-COUNT > PRIME-COUNT
               COMPUTE DISPLAY-PRIME = PRIME-ARRAY(DISPLAY-COUNT)
               DISPLAY DISPLAY-PRIME
           END-PERFORM.

           DISPLAY " "
           DISPLAY "Total count: " PRIME-COUNT.
```

## Algorithm Explanation

This COBOL implementation of the Sieve of Atkin algorithm:

1. **Initialization**: Sets up flags for numbers 1 through N
2. **Step 1**: Marks numbers of the form 4x² + y² = n where n mod 12 = 1 or 5
3. **Step 2**: Marks numbers of the form 3x² + y² = n where n mod 12 = 7
4. **Step 3**: Marks numbers of the form 3x² - y² = n where n mod 12 = 11 and x > y
5. **Step 4**: Eliminates multiples of squares (like 25, 49, 121, etc.)
6. **Step 5**: Collects all marked primes into an array

The algorithm is more efficient than the traditional Sieve of Eratosthenes for large numbers, with a time complexity of approximately O(N / log log N).

