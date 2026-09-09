# Diffie-Hellman Key Exchange in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIFFIEHELLMAN.
       AUTHOR. COBOL EXAMPLE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PRIME-NUMBER        PIC 9(10) VALUE 23.
       01  BASE-VALUE          PIC 9(10) VALUE 5.
       01  PRIVATE-KEY-A       PIC 9(10) VALUE 6.
       01  PRIVATE-KEY-B       PIC 9(10) VALUE 15.
       01  PUBLIC-KEY-A        PIC 9(10).
       01  PUBLIC-KEY-B        PIC 9(10).
       01  SHARED-SECRET-A     PIC 9(10).
       01  SHARED-SECRET-B     PIC 9(10).
       01  TEMP-RESULT         PIC 9(20).
       01  I                   PIC 9(10).
       01  RESULT              PIC 9(10).

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "DIFFIE-HELLMAN KEY EXCHANGE EXAMPLE"
           DISPLAY "====================================="

           DISPLAY "Prime Number (p): " PRIME-NUMBER
           DISPLAY "Base Value (g): " BASE-VALUE
           DISPLAY "Private Key A: " PRIVATE-KEY-A
           DISPLAY "Private Key B: " PRIVATE-KEY-B

           PERFORM CALCULATE-PUBLIC-KEY-A
           PERFORM CALCULATE-PUBLIC-KEY-B
           PERFORM CALCULATE-SHARED-SECRET-A
           PERFORM CALCULATE-SHARED-SECRET-B

           DISPLAY "Public Key A: " PUBLIC-KEY-A
           DISPLAY "Public Key B: " PUBLIC-KEY-B
           DISPLAY "Shared Secret A: " SHARED-SECRET-A
           DISPLAY "Shared Secret B: " SHARED-SECRET-B

           IF SHARED-SECRET-A = SHARED-SECRET-B
               DISPLAY "SUCCESS: Both parties have the same shared secret!"
           ELSE
               DISPLAY "ERROR: Shared secrets do not match!"
           END-IF

           STOP RUN.

       CALCULATE-PUBLIC-KEY-A.
           MOVE 1 TO TEMP-RESULT
           MOVE 0 TO I

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PRIVATE-KEY-A
               MULTIPLY BASE-VALUE BY TEMP-RESULT GIVING RESULT
               COMPUTE TEMP-RESULT = FUNCTION MOD(RESULT, PRIME-NUMBER)
           END-PERFORM

           MOVE TEMP-RESULT TO PUBLIC-KEY-A.

       CALCULATE-PUBLIC-KEY-B.
           MOVE 1 TO TEMP-RESULT
           MOVE 0 TO I

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PRIVATE-KEY-B
               MULTIPLY BASE-VALUE BY TEMP-RESULT GIVING RESULT
               COMPUTE TEMP-RESULT = FUNCTION MOD(RESULT, PRIME-NUMBER)
           END-PERFORM

           MOVE TEMP-RESULT TO PUBLIC-KEY-B.

       CALCULATE-SHARED-SECRET-A.
           MOVE 1 TO TEMP-RESULT
           MOVE 0 TO I

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PRIVATE-KEY-A
               MULTIPLY PUBLIC-KEY-B BY TEMP-RESULT GIVING RESULT
               COMPUTE TEMP-RESULT = FUNCTION MOD(RESULT, PRIME-NUMBER)
           END-PERFORM

           MOVE TEMP-RESULT TO SHARED-SECRET-A.

       CALCULATE-SHARED-SECRET-B.
           MOVE 1 TO TEMP-RESULT
           MOVE 0 TO I

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > PRIVATE-KEY-B
               MULTIPLY PUBLIC-KEY-A BY TEMP-RESULT GIVING RESULT
               COMPUTE TEMP-RESULT = FUNCTION MOD(RESULT, PRIME-NUMBER)
           END-PERFORM

           MOVE TEMP-RESULT TO SHARED-SECRET-B.
```

## Output:
```
DIFFIE-HELLMAN KEY EXCHANGE EXAMPLE
=====================================
Prime Number (p): 23
Base Value (g): 5
Private Key A: 6
Private Key B: 15
Public Key A: 8
Public Key B: 19
Shared Secret A: 2
Shared Secret B: 2
SUCCESS: Both parties have the same shared secret!
```

## Explanation:

This COBOL program demonstrates the Diffie-Hellman key exchange algorithm with the following steps:

1. **Setup**: Uses prime number p=23 and base g=5
2. **Private Keys**: Party A has private key 6, Party B has private key 15
3. **Public Key Calculation**:
   - Public Key A = g^private_key_A mod p = 5^6 mod 23 = 8
   - Public Key B = g^private_key_B mod p = 5^15 mod 23 = 19
4. **Shared Secret Calculation**:
   - Party A computes: shared_secret = public_key_B^private_key_A mod p = 19^6 mod 23 = 2
   - Party B computes: shared_secret = public_key_A^private_key_B mod p = 8^15 mod 23 = 2

Both parties arrive at the same shared secret (2), demonstrating successful key exchange.