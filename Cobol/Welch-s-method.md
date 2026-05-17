# Welch's Method Algorithm in COBOL

Here's an implementation of Welch's method for power spectral density estimation in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WELCH-METHOD.
       AUTHOR. COBOL IMPLEMENTATION.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-DATA ASSIGN TO "INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-RESULTS ASSIGN TO "RESULTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-DATA.
       01 INPUT-RECORD.
          05 SAMPLE-DATA    PIC 9(8)V9(4).
          05 RECORD-NO      PIC 9(6).

       FD OUTPUT-RESULTS.
       01 OUTPUT-RECORD.
          05 FREQUENCY      PIC 9(8)V9(6).
          05 POWER-VALUE    PIC 9(8)V9(6).

       WORKING-STORAGE SECTION.
       01 WS-CONSTANTS.
          05 N-SAMPLES      PIC 9(6) VALUE 1024.
          05 N-SEGMENTS     PIC 9(6) VALUE 8.
          05 OVERLAP        PIC 9(6) VALUE 512.
          05 FFT-SIZE       PIC 9(6) VALUE 1024.
          05 WS-PI          PIC 9(8)V9(6) VALUE 3.14159265359.
       
       01 WS-VARIABLES.
          05 WS-INDEX       PIC 9(6).
          05 WS-SEGMENT-START PIC 9(6).
          05 WS-SEGMENT-END   PIC 9(6).
          05 WS-WINDOW-INDEX  PIC 9(6).
          05 WS-FFT-INDEX     PIC 9(6).
          05 WS-RESULT-INDEX  PIC 9(6).
          05 WS-TEMP        PIC 9(8)V9(6).
          05 WS-SUM         PIC 9(10)V9(6).
          05 WS-POWER-SUM   PIC 9(10)V9(6).
          05 WS-FFT-RESULT  PIC 9(8)V9(6).
          05 WS-FFT-POWER   PIC 9(8)V9(6).
          05 WS-NOISE-LEVEL PIC 9(8)V9(6).
       
       01 WS-DATA-ARRAY.
          05 DATA-VALUES    OCCURS 1024 TIMES.
             10 SAMPLE-VALUE PIC 9(8)V9(6).
       
       01 WS-WINDOW-ARRAY.
          05 WINDOW-VALUES  OCCURS 1024 TIMES.
             10 WINDOW-COEFF   PIC 9(8)V9(6).
       
       01 WS-FFT-ARRAY.
          05 FFT-REAL       OCCURS 1024 TIMES.
             10 FFT-REAL-VALUE PIC 9(8)V9(6).
          05 FFT-IMAG       OCCURS 1024 TIMES.
             10 FFT-IMAG-VALUE PIC 9(8)V9(6).
       
       01 WS-RESULTS-ARRAY.
          05 RESULT-FREQUENCY OCCURS 512 TIMES.
             10 FREQUENCY-VALUE PIC 9(8)V9(6).
          05 RESULT-POWER     OCCURS 512 TIMES.
             10 POWER-VALUE     PIC 9(8)V9(6).
       
       01 WS-FILE-STATUS.
          05 FILE-STATUS    PIC XX.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-WINDOW
           PERFORM LOAD-DATA
           PERFORM WELCH-ALGORITHM
           PERFORM WRITE-RESULTS
           STOP RUN.

       INITIALIZE-WINDOW.
           COMPUTE WS-SEGMENT-START = 1
           COMPUTE WS-SEGMENT-END = FFT-SIZE
           COMPUTE WS-WINDOW-INDEX = 1
           
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > FFT-SIZE
               COMPUTE WINDOW-COEFF(WS-INDEX) = 
                   0.5 * (1.0 - COS(2.0 * WS-PI * (WS-INDEX - 1) / (FFT-SIZE - 1)))
           END-PERFORM.

       LOAD-DATA.
           OPEN INPUT INPUT-DATA
           READ INPUT-DATA AT END GO TO LOAD-DATA-END
           MOVE SAMPLE-DATA TO SAMPLE-VALUE(1)
           ADD 1 TO RECORD-NO
           PERFORM LOAD-DATA-CONT
           CLOSE INPUT-DATA
           GO TO LOAD-DATA-END.

       LOAD-DATA-CONT.
           READ INPUT-DATA AT END GO TO LOAD-DATA-END
           MOVE SAMPLE-DATA TO SAMPLE-VALUE(RECORD-NO)
           ADD 1 TO RECORD-NO
           GO TO LOAD-DATA-CONT.

       LOAD-DATA-END.
           IF RECORD-NO < N-SAMPLES
               DISPLAY "ERROR: Insufficient data samples"
               STOP RUN
           END-IF.

       WELCH-ALGORITHM.
           COMPUTE WS-SEGMENT-START = 1
           COMPUTE WS-RESULT-INDEX = 1
           
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > N-SEGMENTS
               COMPUTE WS-SEGMENT-END = WS-SEGMENT-START + FFT-SIZE - 1
               PERFORM PROCESS-SEGMENT
               COMPUTE WS-SEGMENT-START = WS-SEGMENT-START + FFT-SIZE - OVERLAP
           END-PERFORM.

       PROCESS-SEGMENT.
           PERFORM ZERO-FFT-ARRAYS
           PERFORM APPLY-WINDOW
           PERFORM COMPUTE-FFT
           PERFORM COMPUTE-POWER-SPECTRUM
           PERFORM AVERAGE-SEGMENTS.

       ZERO-FFT-ARRAYS.
           PERFORM VARYING WS-FFT-INDEX FROM 1 BY 1 UNTIL WS-FFT-INDEX > FFT-SIZE
               MOVE 0.0 TO FFT-REAL-VALUE(WS-FFT-INDEX)
               MOVE 0.0 TO FFT-IMAG-VALUE(WS-FFT-INDEX)
           END-PERFORM.

       APPLY-WINDOW.
           PERFORM VARYING WS-WINDOW-INDEX FROM 1 BY 1 UNTIL WS-WINDOW-INDEX > FFT-SIZE
               COMPUTE SAMPLE-VALUE(WS-WINDOW-INDEX) = 
                   SAMPLE-VALUE(WS-WINDOW-INDEX) * WINDOW-COEFF(WS-WINDOW-INDEX)
           END-PERFORM.

       COMPUTE-FFT.
           * This would normally call an FFT subroutine
           * For demonstration, we'll simulate the FFT computation
           PERFORM VARYING WS-FFT-INDEX FROM 1 BY 1 UNTIL WS-FFT-INDEX > FFT-SIZE
               COMPUTE FFT-REAL-VALUE(WS-FFT-INDEX) = 
                   SAMPLE-VALUE(WS-FFT-INDEX) * COS(2.0 * WS-PI * WS-FFT-INDEX / FFT-SIZE)
               COMPUTE FFT-IMAG-VALUE(WS-FFT-INDEX) = 
                   SAMPLE-VALUE(WS-FFT-INDEX) * SIN(2.0 * WS-PI * WS-FFT-INDEX / FFT-SIZE)
           END-PERFORM.

       COMPUTE-POWER-SPECTRUM.
           PERFORM VARYING WS-FFT-INDEX FROM 1 BY 1 UNTIL WS-FFT-INDEX > FFT-SIZE
               COMPUTE FFT-POWER = 
                   (FFT-REAL-VALUE(WS-FFT-INDEX) ** 2) + 
                   (FFT-IMAG-VALUE(WS-FFT-INDEX) ** 2)
               COMPUTE RESULT-POWER(WS-FFT-INDEX) = 
                   RESULT-POWER(WS-FFT-INDEX) + FFT-POWER
           END-PERFORM.

       AVERAGE-SEGMENTS.
           PERFORM VARYING WS-FFT-INDEX FROM 1 BY 1 UNTIL WS-FFT-INDEX > FFT-SIZE
               COMPUTE RESULT-POWER(WS-FFT-INDEX) = 
                   RESULT-POWER(WS-FFT-INDEX) / N-SEGMENTS
           END-PERFORM.

       WRITE-RESULTS.
           OPEN OUTPUT OUTPUT-RESULTS
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > FFT-SIZE/2
               MOVE FREQUENCY-VALUE(WS-INDEX) TO FREQUENCY
               MOVE POWER-VALUE(WS-INDEX) TO POWER-VALUE
               WRITE OUTPUT-RECORD
           END-PERFORM
           CLOSE OUTPUT-RESULTS.
```

## Key Features of this Implementation:

1. **Data Structure**: Uses arrays to store input data, window coefficients, and FFT results
2. **Windowing**: Implements a Hamming window (similar to Welch's method)
3. **Segmentation**: Divides input data into overlapping segments
4. **FFT Computation**: Simulates FFT calculation for demonstration
5. **Averaging**: Averages power spectra from multiple segments
6. **Output**: Writes results to a results file

## Algorithm Steps:

1. Initialize window coefficients
2. Load input data samples
3. Process data in overlapping segments
4. Apply windowing to each segment
5. Compute FFT for each segment
6. Calculate power spectrum
7. Average power spectra from all segments
8. Output final power spectral density

This implementation demonstrates the core concepts of Welch's method for power spectral density estimation in COBOL.

