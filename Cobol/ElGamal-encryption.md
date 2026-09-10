# ElGamal Encryption in COBOL

Below is an example implementation of the ElGamal encryption algorithm in COBOL:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ELGAMAL-ENCRYPT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KEY-FILE ASSIGN TO "keys.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD KEY-FILE.
       01 KEY-RECORD.
          05 P-SIZE        PIC 9(4).
          05 P-VALUE       PIC 9(20).
          05 G-VALUE       PIC 9(20).
          05 PUBLIC-KEY    PIC 9(20).

       WORKING-STORAGE SECTION.
       01 WS-INPUT-VALUES.
          05 M-VALUE       PIC 9(20).
          05 P-VALUE-WS    PIC 9(20).
          05 G-VALUE-WS    PIC 9(20).
          05 PRIVATE-KEY   PIC 9(20).
          05 PUBLIC-KEY-WS PIC 9(20).

       01 WS-MATH-VALUES.
          05 RANDOM-VALUE  PIC 9(20).
          05 K-VALUE       PIC 9(20).
          05 C1-VALUE      PIC 9(20).
          05 C2-VALUE      PIC 9(20).
          05 TEMP-RESULT   PIC 9(30).
          05 MOD-RESULT    PIC 9(20).

       01 WS-OUTPUT.
          05 ENCRYPTED-MESSAGE.
             10 C1-ENCRYPTED PIC 9(20).
             10 C2-ENCRYPTED PIC 9(20).

       01 WS-TEMPORARY.
          05 I-INDEX       PIC 9(4) VALUE 1.
          05 J-INDEX       PIC 9(4) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "=== ElGamal Encryption ===".
           
           CALL "GET-INPUT" USING M-VALUE, P-VALUE-WS, 
                                 G-VALUE-WS, PRIVATE-KEY.
           
           CALL "GENERATE-RANDOM" USING RANDOM-VALUE.
           
           COMPUTE K-VALUE = RANDOM-VALUE MOD (P-VALUE-WS - 1).
           IF K-VALUE = 0 THEN
               COMPUTE K-VALUE = 1
           END-IF.
           
           * Calculate C1 = G^k mod P
           CALL "MODULAR-EXPONENTIATION" 
               USING G-VALUE-WS, K-VALUE, P-VALUE-WS, C1-VALUE.
           
           * Calculate C2 = M * (Public Key)^k mod P
           COMPUTE TEMP-RESULT = PRIVATE-KEY * K-VALUE.
           CALL "MODULAR-EXPONENTIATION" 
               USING PUBLIC-KEY-WS, K-VALUE, P-VALUE-WS, TEMP-RESULT.
           
           COMPUTE C2-VALUE = (M-VALUE * TEMP-RESULT) MOD P-VALUE-WS.
           
           MOVE C1-VALUE TO C1-ENCRYPTED.
           MOVE C2-VALUE TO C2-ENCRYPTED.
           
           DISPLAY "Original Message: " M-VALUE.
           DISPLAY "Encrypted C1: " C1-ENCRYPTED.
           DISPLAY "Encrypted C2: " C2-ENCRYPTED.
           
           STOP RUN.

       MODULAR-EXPONENTIATION.
           *> This is a simplified modular exponentiation function
           *> In practice, this would be more complex to handle large numbers
           *> This is a basic implementation for demonstration
           01 WS-BASE      PIC 9(20).
           01 WS-EXPONENT  PIC 9(20).
           01 WS-MODULUS   PIC 9(20).
           01 WS-RESULT    PIC 9(20) VALUE 1.
           01 WS-TEMP      PIC 9(20).
           01 WS-I         PIC 9(20) VALUE 1.

           MOVE FUNCTION TRIM(BASE) TO WS-BASE.
           MOVE FUNCTION TRIM(EXPONENT) TO WS-EXPONENT.
           MOVE FUNCTION TRIM(MODULUS) TO WS-MODULUS.
           
           PERFORM VARYING WS-I BY 1 UNTIL WS-I > WS-EXPONENT
               COMPUTE WS-TEMP = (WS-RESULT * WS-BASE) MOD WS-MODULUS
               MOVE WS-TEMP TO WS-RESULT
           END-PERFORM.
           
           MOVE WS-RESULT TO RESULT.

       GET-INPUT.
           *> This would typically read from a file or input stream
           *> For demonstration, we'll use hardcoded values
           MOVE 12345 TO M-VALUE.
           MOVE 17 TO P-VALUE-WS.
           MOVE 3 TO G-VALUE-WS.
           MOVE 7 TO PRIVATE-KEY.
           MOVE 10 TO PUBLIC-KEY-WS.

       GENERATE-RANDOM.
           *> Simple random number generator for demonstration
           COMPUTE RANDOM-VALUE = FUNCTION MOD(54321, 98765) + 1.

       END PROGRAM ELGAMAL-ENCRYPT.
```

## Key Components Explained:

### 1. **Key Structure**
- Prime modulus `p`
- Generator `g` 
- Private key `x`
- Public key `y = g^x mod p`

### 2. **Encryption Process**
- **C1 = g^k mod p** (where k is random)
- **C2 = m * y^k mod p** (where m is the message)

### 3. **Mathematical Operations**
- Modular exponentiation
- Modular multiplication
- Random number generation

## Important Notes:

⚠️ **This is a simplified demonstration** - Real-world ElGamal implementation would require:
- Proper large integer arithmetic
- Secure random number generation
- Input validation
- Error handling
- File I/O for key management

The algorithm demonstrates the core mathematical principles but uses basic COBOL operations that may not handle very large numbers effectively.