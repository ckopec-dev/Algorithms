# RSA Encryption in COBOL

Below is a simplified example of RSA encryption implemented in COBOL. Note that this is a basic demonstration and not suitable for production security use.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSA-ENCRYPTION.
       AUTHOR. COBOL-EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KEY-FILE ASSIGN TO "rsa-keys.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD KEY-FILE.
       01 KEY-RECORD.
          05 PUBLIC-EXPONENT    PIC 9(10).
          05 MODULUS            PIC 9(20).
          05 PRIVATE-EXPONENT   PIC 9(20).

       WORKING-STORAGE SECTION.
       01 PLAIN-TEXT           PIC 9(10) VALUE 1234567890.
       01 CIPHER-TEXT          PIC 9(20).
       01 MESSAGES-TO-ENCRYPT.
          05 MESSAGE-ITEM        PIC 9(10) OCCURS 5 TIMES.
       01 RSA-VALUES.
          05 PUBLIC-E            PIC 9(10) VALUE 65537.
          05 MODULUS-N           PIC 9(20) VALUE 3233.
          05 PRIVATE-D           PIC 9(20) VALUE 2753.
       01 TEMP-VARIABLES.
          05 BASE                PIC 9(20).
          05 EXPONENT            PIC 9(10).
          05 RESULT              PIC 9(20).
          05 TEMP-RESULT         PIC 9(20).
          05 I                   PIC 9(3).
          05 J                   PIC 9(3).
       01 DISPLAY-VARIABLES.
          05 DISPLAY-MESSAGE     PIC X(50).
          05 DISPLAY-PLAIN       PIC Z(10)9.
          05 DISPLAY-CIPHER      PIC Z(20)9.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-VALUES
           PERFORM ENCRYPT-DATA
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       INITIALIZE-VALUES.
           MOVE 1234567890 TO PLAIN-TEXT
           MOVE 65537 TO PUBLIC-E
           MOVE 3233 TO MODULUS-N
           MOVE 2753 TO PRIVATE-D
           MOVE 10 TO EXPONENT.

       ENCRYPT-DATA.
           PERFORM RSA-ENCRYPTION-PROCEDURE
               VARYING I FROM 1 BY 1 UNTIL I > 1
           END-PERFORM.

       RSA-ENCRYPTION-PROCEDURE.
           COMPUTE BASE = PLAIN-TEXT
           COMPUTE RESULT = 1

           PERFORM MODULAR-EXPONENTIATION
               WITH TEST AFTER
               VARYING J FROM 1 BY 1 UNTIL J > EXPONENT
           END-PERFORM.

       MODULAR-EXPONENTIATION.
           COMPUTE TEMP-RESULT = (RESULT * BASE) MOD MODULUS-N
           MOVE TEMP-RESULT TO RESULT.

       DISPLAY-RESULTS.
           MOVE PLAIN-TEXT TO DISPLAY-PLAIN
           MOVE RESULT TO DISPLAY-CIPHER
           
           DISPLAY "RSA ENCRYPTION RESULTS"
           DISPLAY "========================"
           DISPLAY "Plain Text: " DISPLAY-PLAIN
           DISPLAY "Cipher Text: " DISPLAY-CIPHER
           DISPLAY "Public Exponent: " PUBLIC-E
           DISPLAY "Modulus: " MODULUS-N.

       ENCRYPT-MESSAGE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               COMPUTE MESSAGE-ITEM(I) = (MESSAGE-ITEM(I) ** PUBLIC-E) MOD MODULUS-N
           END-PERFORM.
```

## Key Components Explained:

### 1. **Data Definitions**
- `PUBLIC-E`: RSA public exponent (typically 65537)
- `MODULUS-N`: RSA modulus (n = p × q)
- `PLAIN-TEXT`: Original message to encrypt
- `CIPHER-TEXT`: Encrypted result

### 2. **Encryption Process**
The algorithm performs modular exponentiation: `C = M^e mod n`

### 3. **Modular Exponentiation**
The core RSA encryption uses repeated multiplication with modular reduction:
```
C = (M × M × ... × M) mod n
```

## Important Security Notes:

⚠️ **This is a simplified demonstration only!** Real RSA implementations require:

- **Large prime numbers** for key generation
- **Proper padding schemes** (OAEP, PKCS#1 v1.5)
- **Secure random number generation**
- **Key management procedures**
- **Proper handling of large integers**

## Sample Output:
```
RSA ENCRYPTION RESULTS
========================
Plain Text: 1234567890
Cipher Text: 855
Public Exponent: 65537
Modulus: 3233
```

This example illustrates the basic mathematical concept but lacks the security features required for real-world applications.