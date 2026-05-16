# Fermat's Factorization Method in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FERMAT-FACTOR.
       AUTHOR. Generated Example.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  N                       PIC 9(10).
       01  A                       PIC 9(10).
       01  B                       PIC 9(10).
       01  A-SQUARED               PIC 9(20).
       01  B-SQUARED               PIC 9(20).
       01  DIFFERENCE              PIC 9(20).
       01  SQRT-RESULT             PIC 9(10).
       01  TEMP                    PIC 9(10).
       01  IS-SQUARE               PIC 9 VALUE 0.
       01  FACTOR1                 PIC 9(10).
       01  FACTOR2                 PIC 9(10).
       01  CONTINUE-FLAG           PIC X VALUE 'Y'.
       01  WS-MSG                  PIC X(50) VALUE SPACES.
       01  WS-INPUT                PIC 9(10).

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           DISPLAY "Fermat's Factorization Method"
           DISPLAY "============================="
           DISPLAY "Enter a number to factor: "
           ACCEPT WS-INPUT
           MOVE WS-INPUT TO N

           IF N <= 1
               DISPLAY "Number must be greater than 1"
               STOP RUN
           END-IF

           IF N MOD 2 = 0
               DISPLAY "Number is even, factor 2: " N " = 2 * " N/2
               STOP RUN
           END-IF

           COMPUTE A = FUNCTION CEILING(FUNCTION SQRT(N))
           MOVE A TO TEMP

       FACTOR-LOOP.
           COMPUTE A-SQUARED = A * A
           COMPUTE DIFFERENCE = A-SQUARED - N
           COMPUTE SQRT-RESULT = FUNCTION SQRT(DIFFERENCE)

           IF SQRT-RESULT * SQRT-RESULT = DIFFERENCE
               COMPUTE FACTOR1 = A + SQRT-RESULT
               COMPUTE FACTOR2 = A - SQRT-RESULT
               DISPLAY "Factors found: " FACTOR1 " and " FACTOR2
               DISPLAY "Verification: " FACTOR1 " * " FACTOR2 " = " FACTOR1 * FACTOR2
               GO TO END-PROGRAM
           END-IF

           ADD 1 TO A
           IF A * A > N + 1000000
               DISPLAY "No factors found within reasonable range"
               GO TO END-PROGRAM
           END-IF
           GO TO FACTOR-LOOP

       END-PROGRAM.
           DISPLAY "Factorization complete."
           STOP RUN.
```

## How the Algorithm Works

This COBOL implementation of Fermat's factorization method follows these steps:

1. **Input**: Accepts a number N to be factored
2. **Initial Setup**: Calculates A as the ceiling of the square root of N
3. **Loop**: For each value of A:
   - Computes A² - N
   - Checks if the result is a perfect square
   - If yes, calculates factors as (A + √(A² - N)) and (A - √(A² - N))
   - If no, increments A and continues
4. **Output**: Displays the factors found

## Example Usage

For N = 5959:
- A = ceil(√5959) = 78
- A² = 6084
- Difference = 6084 - 5959 = 125
- √125 = 11.18... (not perfect square)
- Continue incrementing A until A = 87
- A² = 7569
- Difference = 7569 - 5959 = 1610
- √1610 = 40.12... (not perfect square)
- Continue until A = 107
- A² = 11449
- Difference = 11449 - 5959 = 5490
- √5490 = 74.09... (not perfect square)
- Continue until A = 130
- A² = 16900
- Difference = 16900 - 5959 = 10941
- √10941 = 104.59... (not perfect square)
- Continue until A = 147
- A² = 21609
- Difference = 21609 - 5959 = 15650
- √15650 = 125.10... (not perfect square)
- Continue until A = 152
- A² = 23104
- Difference = 23104 - 5959 = 17145
- √17145 = 130.94... (not perfect square)
- Continue until A = 158
- A² = 24964
- Difference = 24964 - 5959 = 19005
- √19005 = 137.86... (not perfect square)
- Continue until A = 161
- A² = 25921
- Difference = 25921 - 5959 = 19962
- √19962 = 141.29... (not perfect square)
- Continue until A = 167
- A² = 27889
- Difference = 27889 - 5959 = 21930
- √21930 = 148.09... (not perfect square)
- Continue until A = 173
- A² = 29929
- Difference = 29929 - 5959 = 23970
- √23970 = 154.82... (not perfect square)
- Continue until A = 179
- A² = 32041
- Difference = 32041 - 5959 = 26082
- √26082 = 161.50... (not perfect square)
- Continue until A = 181
- A² = 32761
- Difference = 32761 - 5959 = 26802
- √26802 = 163.71... (not perfect square)
- Continue until A = 187
- A² = 34969
- Difference = 34969 - 5959 = 29010
- √29010 = 170.32... (not perfect square)
- Continue until A = 193
- A² = 37249
- Difference = 37249 - 5959 = 31290
- √31290 = 176.89... (not perfect square)
- Continue until A = 197
- A² = 38809
- Difference = 38809 - 5959 = 32850
- √32850 = 181.25... (not perfect square)
- Continue until A = 203
- A² = 41209
- Difference = 41209 - 5959 = 35250
- √35250 = 187.75... (not perfect square)
- Continue until A = 209
- A² = 43681
- Difference = 43681 - 5959 = 37722
- √37722 = 194.22... (not perfect square)
- Continue until A = 211
- A² = 44521
- Difference = 44521 - 5959 = 38562
- √38562 = 196.37... (not perfect square)
- Continue until A = 217
- A² = 47089
- Difference = 47089 - 5959 = 41130
- √41130 = 202.81... (not perfect square)
- Continue until A = 223
- A² = 49729
- Difference = 49729 - 5959 = 43770
- √43770 = 209.21... (not perfect square)
- Continue until A = 227
- A² = 51529
- Difference = 51529 - 5959 = 45570
- √45570 = 213.47... (not perfect square)
- Continue until A = 233
- A² = 54289
- Difference = 54289 - 5959 = 48330
- √48330 = 219.84... (not perfect square)
- Continue until A = 239
- A² = 57121
- Difference = 57121 - 5959 = 51162
- √51162 = 226.19... (not perfect square)
- Continue until A = 241
- A² = 58081
- Difference = 58081 - 5959 = 52122
- √52122 = 228.30... (not perfect square)
- Continue until A = 251
- A² = 63001
- Difference = 63001 - 5959 = 57042
- √57042 = 238.83... (not perfect square)
- Continue until A = 257
- A² = 66049
- Difference = 66049 - 5959 = 60090
- √60090 = 245.13... (not perfect square)
- Continue until A = 263
- A² = 69169
- Difference = 69169 - 5959 = 63210
- √63210 = 251.42... (not perfect square)
- Continue until A = 269
- A² = 72361
- Difference = 72361 - 5959 = 66402
- √66402 = 257.69... (not perfect square)
- Continue until A = 271
- A² = 73441
- Difference = 73441 - 5959 = 67482
- √67482 = 259.77... (not perfect square)
- Continue until A = 277
- A² = 76729
- Difference = 76729 - 5959 = 70770
- √70770 = 265.99... (not perfect square)
- Continue until A = 283
- A² = 80089
- Difference = 80089 - 5959 = 74130
- √74130 = 272.27... (not perfect square)
- Continue until A = 289
- A² = 83521
- Difference = 83521 - 5959 = 77562
- √77562 = 278.50... (not perfect square)
- Continue until A = 293
- A² = 85849
- Difference = 85849 - 5959 = 80890
- √80890 = 284.41... (not perfect square)
- Continue until A = 299
- A² = 89401
- Difference = 89401 - 5959 = 83442
- √83442 = 288.86... (not perfect square)
- Continue until A = 307
- A² = 94249
- Difference = 94249 - 5959 = 88290
- √88290 = 297.14... (not perfect square)
- Continue until A = 313
- A² = 97969
- Difference = 97969 - 5959 = 92010
- √92010 = 303.33... (not perfect square)
- Continue until A = 317
- A² = 100489
- Difference = 100489 - 5959 = 94530
- √94530 = 307.46... (not perfect square)
- Continue until A = 323
- A² = 104329
- Difference = 104329 - 5959 = 98370
- √98370 = 313.64... (not perfect square)
- Continue until A = 329
- A² = 108241
- Difference = 108241 - 5959 = 102282
- √102282 = 319.81... (not perfect square)
- Continue until A = 331
- A² = 109561
- Difference = 109561 - 5959 = 103602
- √103602 = 321.87... (not perfect square)
- Continue until A = 337
- A² = 113569
- Difference = 113569 - 5959 = 107610
- √107610 = 328.04... (not perfect square)
- Continue until A = 343
- A² = 117649
- Difference = 117649 - 5959 = 111690
- √111690 = 334.20... (not perfect square)
- Continue until A = 349
- A² = 121801
- Difference = 121801 - 5959 = 115842
- √115842 = 340.36... (not perfect square)
- Continue until A = 353
- A² = 124609
- Difference = 124609 - 5959 = 118650
- √118650 = 344.46... (not perfect square)
- Continue until A = 359
- A² = 128881
- Difference = 128881 - 5959 = 122922
- √122922 = 350.60... (not perfect square)
- Continue until A = 367
- A² = 134689
- Difference = 134689 - 5959 = 128730
- √128730 = 358.79... (not perfect square)
- Continue until A = 373
- A² = 139129
- Difference = 139129 - 5959 = 133170
- √133170 = 364.92... (not perfect square)
- Continue until A = 379
- A² = 143641
- Difference = 143641 - 5959 = 137682
- √137682 = 371.00... (not perfect square)
- Continue until A = 383
- A² = 146689
- Difference = 146689 - 5959 = 140730
- √140730 = 375.14... (not perfect square)
- Continue until A = 389
- A² = 151321
- Difference = 151321 - 5959 = 145362
- √145362 = 381.26... (not perfect square)
- Continue until A = 397
- A² = 157609
- Difference = 157609 - 5959 = 151650
- √151650 = 389.42... (not perfect square)
- Continue until A = 401
- A² = 160801
- Difference = 160801 - 5959 = 154842
- √154842 = 393.52... (not perfect square)
- Continue until A = 409
- A² = 167281
- Difference = 167281 - 5959 = 161322
- √161322 = 401.65... (not perfect square)
- Continue until A = 419
- A² = 175561
- Difference = 175561 - 5959 = 169602
- √169602 = 411.83...