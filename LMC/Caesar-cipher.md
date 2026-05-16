# Caesar Cipher in LMC (Little Man Computer)

Here's a complete implementation of a Caesar cipher algorithm in LMC:

```assembly
        INP         ; Get the character to encrypt
        STA CHAR    ; Store the character
        INP         ; Get the shift value (0-25)
        STA SHIFT   ; Store the shift value
        
        LDA CHAR    ; Load the character
        SUB ASCII_A ; Subtract 'A' to get 0-25 range
        BRZ IS_A    ; If it's 'A', handle specially
        BRZ IS_Z    ; If it's 'Z', handle specially
        
        LDA SHIFT   ; Load shift value
        ADD CHAR    ; Add to character (this will be 0-25 + shift)
        STA TEMP    ; Store result
        
        LDA TEMP    ; Load result
        MOD 26      ; Modulo 26 to wrap around
        STA RESULT  ; Store wrapped result
        
        LDA RESULT  ; Load wrapped result
        ADD ASCII_A ; Add 'A' back to get ASCII value
        OUT         ; Output encrypted character
        
        LDA ZERO    ; Load 0 to terminate
        HLT         ; Halt
        
IS_A    LDA SHIFT   ; Load shift value
        ADD ASCII_A ; Add 'A' to get new character
        OUT         ; Output encrypted character
        LDA ZERO    ; Load 0 to terminate
        HLT         ; Halt
        
IS_Z    LDA SHIFT   ; Load shift value
        SUB 26      ; Subtract 26 to wrap around
        ADD ASCII_A ; Add 'A' to get new character
        OUT         ; Output encrypted character
        LDA ZERO    ; Load 0 to terminate
        HLT         ; Halt

CHAR    DAT 0     ; Character to encrypt
SHIFT   DAT 0     ; Shift value (0-25)
TEMP    DAT 0     ; Temporary storage
RESULT  DAT 0     ; Result storage
ASCII_A DAT 65    ; ASCII value of 'A'
ZERO    DAT 0     ; Zero for termination
MOD     DAT 26    ; Modulo value for wrapping
```

## How it works:

1. **Input**: Takes a character and shift value as input
2. **Conversion**: Converts character to 0-25 range by subtracting 'A'
3. **Encryption**: Adds the shift value to the character
4. **Wrapping**: Uses modulo 26 to wrap around the alphabet
5. **Output**: Converts back to ASCII and outputs the encrypted character

## Example usage:
- Input character: 'A' (ASCII 65)
- Input shift: 3
- Output: 'D' (ASCII 68)

## Note:
This is a simplified version that demonstrates the core concept. A full implementation would need to handle:
- Multiple characters
- Non-alphabetic characters
- Decryption functionality
- Proper modulo operations for negative results

The LMC implementation uses a simplified approach with basic arithmetic operations and conditional branching to achieve the Caesar cipher functionality.

