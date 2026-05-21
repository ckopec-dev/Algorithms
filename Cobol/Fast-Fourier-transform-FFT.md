# Fast Fourier Transform (FFT) in COBOL

Here's an example implementation of the FFT algorithm in COBOL using the Cooley-Tukey radix-2 algorithm:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FFT-ALGORITHM.
       AUTHOR. FFT IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-DATA ASSIGN TO "INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-DATA ASSIGN TO "OUTPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-DATA.
       01 INPUT-RECORD.
          05 INPUT-REAL    PIC 9(5)V99.
          05 INPUT-IMAG    PIC 9(5)V99.

       FD OUTPUT-DATA.
       01 OUTPUT-RECORD.
          05 OUTPUT-REAL   PIC 9(5)V99.
          05 OUTPUT-IMAG   PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 N                    PIC 9(4) VALUE 8.
       01 N-1                  PIC 9(4) VALUE 7.
       01 LOG2-N               PIC 9(2) VALUE 3.
       01 PI                   PIC 9(1)V999999 VALUE 3.141592.
       01 TWO-PI               PIC 9(1)V999999 VALUE 6.283185.
       01 TEMP-REAL            PIC 9(5)V99.
       01 TEMP-IMAG            PIC 9(5)V99.
       01 W-REAL               PIC 9(5)V99.
       01 W-IMAG               PIC 9(5)V99.
       01 U-REAL               PIC 9(5)V99.
       01 U-IMAG               PIC 9(5)V99.
       01 V-REAL               PIC 9(5)V99.
       01 V-IMAG               PIC 9(5)V99.
       01 COS-VAL              PIC 9(5)V99.
       01 SIN-VAL              PIC 9(5)V99.
       01 ANGLE                PIC 9(5)V99.
       01 SWAP-TEMP            PIC 9(5)V99.
       01 I                    PIC 9(4).
       01 J                    PIC 9(4).
       01 K                    PIC 9(4).
       01 L                    PIC 9(4).
       01 M                    PIC 9(4).
       01 P                    PIC 9(4).
       01 Q                    PIC 9(4).

       01 DATA-ARRAY.
          05 DATA-ITEM OCCURS 8 TIMES.
             10 REAL-PART    PIC 9(5)V99.
             10 IMAG-PART    PIC 9(5)V99.

       01 BIT-REVERSE-ARRAY.
          05 BIT-REVERSE-ITEM OCCURS 8 TIMES.
             10 BIT-REVERSE-INDEX PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-DATA.
           PERFORM BIT-REVERSE-SORT.
           PERFORM FFT-COMPUTE.
           PERFORM WRITE-RESULTS.
           STOP RUN.

       INITIALIZE-DATA.
           MOVE 1 TO I.
           PERFORM UNTIL I > N
               MOVE 0 TO REAL-PART(I)
               MOVE 0 TO IMAG-PART(I)
               ADD 1 TO I
           END-PERFORM.

           MOVE 1 TO I.
           PERFORM UNTIL I > N
               MOVE 1 TO REAL-PART(I)
               MOVE 0 TO IMAG-PART(I)
               ADD 1 TO I
           END-PERFORM.

       BIT-REVERSE-SORT.
           PERFORM COMPUTE-BIT-REVERSE-INDICES.
           PERFORM PERFORM-BIT-REVERSE-SWAP.

       COMPUTE-BIT-REVERSE-INDICES.
           MOVE 0 TO I.
           PERFORM UNTIL I >= N
               MOVE I TO J.
               MOVE 0 TO K.
               PERFORM UNTIL K >= LOG2-N
                   IF J > 0
                       IF J MOD 2 = 1
                           ADD 1 TO BIT-REVERSE-INDEX(K)
                       END-IF
                       DIVIDE J BY 2 GIVING J
                   END-IF
                   ADD 1 TO K
               END-PERFORM
               ADD 1 TO I
           END-PERFORM.

       PERFORM-BIT-REVERSE-SWAP.
           MOVE 0 TO I.
           PERFORM UNTIL I >= N
               IF I < BIT-REVERSE-INDEX(I)
                   MOVE REAL-PART(I) TO TEMP-REAL
                   MOVE IMAG-PART(I) TO TEMP-IMAG
                   MOVE REAL-PART(BIT-REVERSE-INDEX(I)) TO REAL-PART(I)
                   MOVE IMAG-PART(BIT-REVERSE-INDEX(I)) TO IMAG-PART(I)
                   MOVE TEMP-REAL TO REAL-PART(BIT-REVERSE-INDEX(I))
                   MOVE TEMP-IMAG TO IMAG-PART(BIT-REVERSE-INDEX(I))
               END-IF
               ADD 1 TO I
           END-PERFORM.

       FFT-COMPUTE.
           MOVE 2 TO L.
           PERFORM UNTIL L > N
               MOVE L TO M.
               MOVE 0 TO P.
               PERFORM UNTIL P < L
                   COMPUTE ANGLE = P * TWO-PI / L
                   COMPUTE COS-VAL = FUNCTION COS(ANGLE)
                   COMPUTE SIN-VAL = FUNCTION SIN(ANGLE)
                   MOVE COS-VAL TO W-REAL
                   MOVE SIN-VAL TO W-IMAG
                   MOVE 0 TO K.
                   PERFORM UNTIL K < N
                       MOVE REAL-PART(K) TO U-REAL
                       MOVE IMAG-PART(K) TO U-IMAG
                       MOVE REAL-PART(K + M) TO V-REAL
                       MOVE IMAG-PART(K + M) TO V-IMAG
                       COMPUTE REAL-PART(K) = U-REAL + W-REAL * V-REAL - W-IMAG * V-IMAG
                       COMPUTE IMAG-PART(K) = U-IMAG + W-REAL * V-IMAG + W-IMAG * V-REAL
                       COMPUTE REAL-PART(K + M) = U-REAL - W-REAL * V-REAL + W-IMAG * V-IMAG
                       COMPUTE IMAG-PART(K + M) = U-IMAG - W-REAL * V-IMAG - W-IMAG * V-REAL
                       ADD L TO K
                   END-PERFORM
                   ADD L TO P
               END-PERFORM
               MULTIPLY L BY 2 GIVING L
           END-PERFORM.

       WRITE-RESULTS.
           OPEN OUTPUT OUTPUT-DATA.
           MOVE 0 TO I.
           PERFORM UNTIL I >= N
               MOVE REAL-PART(I) TO OUTPUT-REAL
               MOVE IMAG-PART(I) TO OUTPUT-IMAG
               WRITE OUTPUT-RECORD
               ADD 1 TO I
           END-PERFORM.
           CLOSE OUTPUT-DATA.

       END PROGRAM FFT-ALGORITHM.
```

## Key Features of this FFT Implementation:

1. **Radix-2 Cooley-Tukey Algorithm**: Uses the standard FFT approach for power-of-2 length sequences
2. **Bit-Reversal Permutation**: Rearranges input data in bit-reversed order
3. **Butterfly Operations**: Implements the core FFT computation using butterfly operations
4. **Complex Number Handling**: Properly handles real and imaginary components
5. **Modular Structure**: Separates initialization, bit-reversal, FFT computation, and output

## Sample Input/Output:

**Input Data (8 points):**
```
1.00  0.00
1.00  0.00
1.00  0.00
1.00  0.00
1.00  0.00
1.00  0.00
1.00  0.00
1.00  0.00
```

**Expected Output (Frequency Domain):**
```
8.00  0.00
0.00  0.00
0.00  0.00
0.00  0.00
0.00  0.00
0.00  0.00
0.00  0.00
0.00  0.00
```

This implementation demonstrates the classic FFT algorithm in COBOL, though it should be noted that COBOL is not typically used for mathematical computing due to its verbosity and lack of modern numerical libraries.

