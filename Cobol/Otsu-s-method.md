# Otsu's Method Algorithm in COBOL

Here's an implementation of Otsu's method for automatic threshold selection in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OTSU-METHOD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IMAGE-FILE ASSIGN TO "IMAGE.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD IMAGE-FILE.
       01 IMAGE-RECORD.
          05 PIXEL-VALUE PIC 99.

       WORKING-STORAGE SECTION.
       01 THRESHOLD-VALUE PIC 99 VALUE 0.
       01 TOTAL-PIXELS PIC 9(8) VALUE 0.
       01 HISTOGRAM.
          05 HISTOGRAM-ENTRY PIC 9(6) OCCURS 256 TIMES.
       01 PROBABILITY.
          05 PROBABILITY-ENTRY PIC 9V999999 OCCURS 256 TIMES.
       01 WEIGHT-BACKGROUND PIC 9V999999 VALUE 0.
       01 WEIGHT-FOREGROUND PIC 9V999999 VALUE 0.
       01 MEAN-BACKGROUND PIC 9V999999 VALUE 0.
       01 MEAN-FOREGROUND PIC 9V999999 VALUE 0.
       01 BETWEEN-VARIANCE PIC 9V999999 VALUE 0.
       01 MAX-VARIANCE PIC 9V999999 VALUE 0.
       01 MAX-THRESHOLD PIC 99 VALUE 0.
       01 SUM-BACKGROUND PIC 9(8) VALUE 0.
       01 SUM-FOREGROUND PIC 9(8) VALUE 0.
       01 COUNT-BACKGROUND PIC 9(8) VALUE 0.
       01 COUNT-FOREGROUND PIC 9(8) VALUE 0.
       01 I PIC 99 VALUE 0.
       01 J PIC 99 VALUE 0.
       01 K PIC 99 VALUE 0.
       01 TEMP-VALUE PIC 99 VALUE 0.
       01 EOF-FLAG PIC X VALUE 'N'.
           88 END-OF-FILE VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM INITIALIZE-HISTOGRAM
           PERFORM READ-IMAGE-FILE
           PERFORM CALCULATE-THRESHOLD
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-HISTOGRAM.
           MOVE 0 TO TOTAL-PIXELS
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > 255
               MOVE 0 TO HISTOGRAM-ENTRY(I)
           END-PERFORM.

       READ-IMAGE-FILE.
           OPEN INPUT IMAGE-FILE
           READ IMAGE-FILE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ
           PERFORM UNTIL END-OF-FILE
               ADD 1 TO TOTAL-PIXELS
               ADD 1 TO HISTOGRAM-ENTRY(PIXEL-VALUE)
               READ IMAGE-FILE
                   AT END MOVE 'Y' TO EOF-FLAG
               END-READ
           END-PERFORM
           CLOSE IMAGE-FILE.

       CALCULATE-THRESHOLD.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 255
               COMPUTE PROBABILITY-ENTRY(I) = 
                   HISTOGRAM-ENTRY(I) / TOTAL-PIXELS
           END-PERFORM

           PERFORM VARYING THRESHOLD-VALUE FROM 1 BY 1 UNTIL THRESHOLD-VALUE > 254
               MOVE 0 TO WEIGHT-BACKGROUND
               MOVE 0 TO WEIGHT-FOREGROUND
               MOVE 0 TO SUM-BACKGROUND
               MOVE 0 TO SUM-FOREGROUND
               MOVE 0 TO COUNT-BACKGROUND
               MOVE 0 TO COUNT-FOREGROUND

               PERFORM VARYING J FROM 0 BY 1 UNTIL J = THRESHOLD-VALUE
                   ADD PROBABILITY-ENTRY(J) TO WEIGHT-BACKGROUND
                   COMPUTE SUM-BACKGROUND = SUM-BACKGROUND + 
                       (J * PROBABILITY-ENTRY(J))
                   ADD 1 TO COUNT-BACKGROUND
               END-PERFORM

               PERFORM VARYING J FROM THRESHOLD-VALUE BY 1 UNTIL J > 255
                   ADD PROBABILITY-ENTRY(J) TO WEIGHT-FOREGROUND
                   COMPUTE SUM-FOREGROUND = SUM-FOREGROUND + 
                       (J * PROBABILITY-ENTRY(J))
                   ADD 1 TO COUNT-FOREGROUND
               END-PERFORM

               IF COUNT-BACKGROUND > 0
                   COMPUTE MEAN-BACKGROUND = SUM-BACKGROUND / COUNT-BACKGROUND
               ELSE
                   MOVE 0 TO MEAN-BACKGROUND
               END-IF

               IF COUNT-FOREGROUND > 0
                   COMPUTE MEAN-FOREGROUND = SUM-FOREGROUND / COUNT-FOREGROUND
               ELSE
                   MOVE 0 TO MEAN-FOREGROUND
               END-IF

               COMPUTE BETWEEN-VARIANCE = 
                   WEIGHT-BACKGROUND * WEIGHT-FOREGROUND * 
                   (MEAN-BACKGROUND - MEAN-FOREGROUND) ** 2

               IF BETWEEN-VARIANCE > MAX-VARIANCE
                   MOVE BETWEEN-VARIANCE TO MAX-VARIANCE
                   MOVE THRESHOLD-VALUE TO MAX-THRESHOLD
               END-IF
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "OPTIMAL THRESHOLD VALUE: " MAX-THRESHOLD
           DISPLAY "MAXIMUM VARIANCE: " MAX-VARIANCE.

       END PROGRAM OTSU-METHOD.
```

## Key Components Explained:

### 1. **Data Structures**
- `HISTOGRAM` array to store pixel frequency counts (256 entries for 0-255 grayscale values)
- `PROBABILITY` array to store normalized probabilities
- Working storage variables for intermediate calculations

### 2. **Algorithm Steps**
1. **Initialize histogram** - Set all histogram entries to zero
2. **Read image data** - Count pixel occurrences and populate histogram
3. **Calculate threshold** - Test each possible threshold value (0-255)
4. **Compute between-class variance** - For each threshold, calculate the variance between background and foreground classes
5. **Select optimal threshold** - Choose the threshold that maximizes variance

### 3. **Mathematical Approach**
The algorithm uses Otsu's method formula:
- Calculates probability of each gray level
- Computes background and foreground weights
- Calculates class means
- Determines between-class variance
- Selects threshold that maximizes this variance

This implementation provides a complete COBOL solution for automatic threshold selection in image processing applications.