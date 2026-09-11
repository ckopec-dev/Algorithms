# Euclidean Algorithm in COBOL

Here's an example of implementing the Euclidean algorithm (to find GCD) in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUCLIDEAN-GCD.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM1            PIC 9(5) VALUE 48.
       01  NUM2            PIC 9(5) VALUE 18.
       01  TEMP            PIC 9(5).
       01  REMAINDER       PIC 9(5).
       01  RESULT          PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Finding GCD of " NUM1 " and " NUM2.

           MOVE NUM1 TO TEMP.
           MOVE NUM2 TO RESULT.

           PERFORM UNTIL RESULT = 0
               COMPUTE REMAINDER = TEMP MOD RESULT
               MOVE RESULT TO TEMP
               MOVE REMAINDER TO RESULT
           END-PERFORM.

           DISPLAY "GCD is: " TEMP.
           STOP RUN.
```

## Alternative Version with User Input

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EUCLIDEAN-GCD-INPUT.
       AUTHOR. COBOL EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INPUT-NUM1      PIC 9(5).
       01  INPUT-NUM2      PIC 9(5).
       01  TEMP            PIC 9(5).
       01  REMAINDER       PIC 9(5).
       01  RESULT          PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Enter first number: ".
           ACCEPT INPUT-NUM1.

           DISPLAY "Enter second number: ".
           ACCEPT INPUT-NUM2.

           MOVE INPUT-NUM1 TO TEMP.
           MOVE INPUT-NUM2 TO RESULT.

           PERFORM UNTIL RESULT = 0
               COMPUTE REMAINDER = TEMP MOD RESULT
               MOVE RESULT TO TEMP
               MOVE REMAINDER TO RESULT
           END-PERFORM.

           DISPLAY "GCD of " INPUT-NUM1 " and " INPUT-NUM2 " is: " TEMP.
           STOP RUN.
```

## How it works:

1. **Initialization**: Set up the two numbers (or accept user input)
2. **Loop**: Continue while the second number (RESULT) is not zero
3. **Calculation**: Compute remainder using MOD operator
4. **Update**: Replace the larger number with the smaller one, and smaller with remainder
5. **Result**: When RESULT becomes 0, TEMP contains the GCD

## Example Output:
```
Finding GCD of 48 and 18
GCD is: 6
```

This implements the classic Euclidean algorithm where GCD(a,b) = GCD(b, a mod b) until one number becomes zero.