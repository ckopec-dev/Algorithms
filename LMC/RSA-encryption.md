# RSA Encryption in LMC (Little Man Computer)

Here's an implementation of RSA encryption algorithm in LMC assembly language:

```assembly
; RSA Encryption Algorithm - LMC Implementation
; This is a simplified version demonstrating RSA concepts
; Note: LMC has limited memory and operations, so this is a conceptual implementation

    INP         ; Input message (plaintext)
    STA MSG     ; Store message
    INP         ; Input public key exponent (e)
    STA EXP     ; Store exponent
    INP         ; Input modulus (n)
    STA MOD     ; Store modulus

    ; RSA Encryption: C = M^e mod n
    ; Load message
    LDA MSG
    STA TEMP1
    
    ; Load exponent
    LDA EXP
    STA TEMP2
    
    ; Load modulus
    LDA MOD
    STA TEMP3
    
    ; Initialize result = 1
    LDA ONE
    STA RESULT
    
    ; Loop to perform modular exponentiation
    LDA TEMP2   ; Load exponent
    STA COUNT   ; Set counter
    
EXP_LOOP:
    LDA COUNT   ; Check if count = 0
    BRZ END_EXP
    
    ; RESULT = (RESULT * MSG) mod MOD
    LDA RESULT
    MUL MSG     ; Multiply by message
    LDA RESULT  ; Load result
    DIV MOD     ; Divide by modulus
    MUL MOD     ; Multiply by modulus
    SUB RESULT  ; Subtract to get remainder
    STA RESULT  ; Store result
    
    ; Decrement counter
    LDA COUNT
    SUB ONE
    STA COUNT
    
    BRP EXP_LOOP ; Branch if positive

END_EXP:
    LDA RESULT  ; Load final encrypted value
    OUT         ; Output encrypted message
    HLT         ; Halt

; Data Section
MSG   DAT 0     ; Input message
EXP   DAT 0     ; Public exponent
MOD   DAT 0     ; Modulus
TEMP1 DAT 0     ; Temporary storage
TEMP2 DAT 0     ; Temporary storage
TEMP3 DAT 0     ; Temporary storage
COUNT DAT 0     ; Loop counter
RESULT DAT 0    ; Final result
ONE   DAT 1     ; Constant 1

; More realistic RSA implementation would need:
; - Modular multiplication subroutine
; - Extended Euclidean algorithm for decryption
; - Key generation functions
; - Multiple operations for large number handling

```

## Key Components Explained:

### 1. **Input Handling**
- `INP` instructions to get message, exponent, and modulus
- Data storage in memory locations

### 2. **RSA Algorithm Steps**
- **Encryption**: C = M^e mod n
- Modular exponentiation using repeated multiplication
- Modular arithmetic operations

### 3. **Memory Usage**
- `MSG`: Stores plaintext message
- `EXP`: Stores public exponent
- `MOD`: Stores RSA modulus
- `RESULT`: Stores final encrypted value

### 4. **Limitations in LMC**
- Limited memory (100 memory locations)
- No direct division support (requires manual implementation)
- No built-in large number operations
- Simplified modular arithmetic

## Note:
This is a **conceptual implementation** that demonstrates RSA encryption principles in LMC. A full RSA implementation would require:
- More sophisticated modular arithmetic
- Large number handling
- Extended Euclidean algorithm
- Proper key generation
- Additional memory management

The actual LMC implementation would need to be significantly expanded to handle real RSA operations with large numbers.

