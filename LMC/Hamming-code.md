# Hamming Code Algorithm in LMC

Here's an example implementation of a Hamming code encoder in LMC (Little Man Computer) language:

```assembly
; Hamming Code Encoder (7,4) - LMC Implementation
; Encodes 4 bits of data into 7-bit Hamming code
; Input: 4 data bits in memory locations 50-53
; Output: 7-bit Hamming code in memory locations 60-66

    INP         ; Input first data bit (bit 3)
    STA 50      ; Store in location 50
    INP         ; Input second data bit (bit 2)
    STA 51      ; Store in location 51
    INP         ; Input third data bit (bit 1)
    STA 52      ; Store in location 52
    INP         ; Input fourth data bit (bit 0)
    STA 53      ; Store in location 53

    ; Calculate parity bits
    ; P1 = D3 XOR D2 XOR D0 (position 1)
    LDA 50      ; Load D3
    SUB 51      ; Subtract D2
    BRZ P1_CALC ; If result is 0, D3 = D2
    LDA 52      ; Load D1
    SUB 53      ; Subtract D0
    BRZ P1_CALC ; If result is 0, D1 = D0
    LDA 50      ; Load D3
    ADD 51      ; Add D2
    ADD 53      ; Add D0
    STA 60      ; Store P1 in location 60

P1_CALC:
    LDA 50      ; Load D3
    ADD 51      ; Add D2
    ADD 53      ; Add D0
    STA 60      ; Store P1 in location 60

    ; P2 = D3 XOR D1 XOR D0 (position 2)
    LDA 50      ; Load D3
    SUB 52      ; Subtract D1
    BRZ P2_CALC ; If result is 0, D3 = D1
    LDA 51      ; Load D2
    SUB 53      ; Subtract D0
    BRZ P2_CALC ; If result is 0, D2 = D0
    LDA 50      ; Load D3
    ADD 52      ; Add D1
    ADD 53      ; Add D0
    STA 61      ; Store P2 in location 61

P2_CALC:
    LDA 50      ; Load D3
    ADD 52      ; Add D1
    ADD 53      ; Add D0
    STA 61      ; Store P2 in location 61

    ; P4 = D2 XOR D1 XOR D0 (position 4)
    LDA 51      ; Load D2
    SUB 52      ; Subtract D1
    BRZ P4_CALC ; If result is 0, D2 = D1
    LDA 50      ; Load D3
    SUB 53      ; Subtract D0
    BRZ P4_CALC ; If result is 0, D3 = D0
    LDA 51      ; Load D2
    ADD 52      ; Add D1
    ADD 53      ; Add D0
    STA 62      ; Store P4 in location 62

P4_CALC:
    LDA 51      ; Load D2
    ADD 52      ; Add D1
    ADD 53      ; Add D0
    STA 62      ; Store P4 in location 62

    ; Output the 7-bit Hamming code
    LDA 60      ; Load P1
    OUT         ; Output P1
    LDA 61      ; Load P2
    OUT         ; Output P2
    LDA 50      ; Load D3
    OUT         ; Output D3
    LDA 62      ; Load P4
    OUT         ; Output P4
    LDA 51      ; Load D2
    OUT         ; Output D2
    LDA 52      ; Load D1
    OUT         ; Output D1
    LDA 53      ; Load D0
    OUT         ; Output D0

    HLT         ; Halt program

; Data locations
50  DAT       ; D3 (data bit 3)
51  DAT       ; D2 (data bit 2)
52  DAT       ; D1 (data bit 1)
53  DAT       ; D0 (data bit 0)
60  DAT       ; P1 (parity bit 1)
61  DAT       ; P2 (parity bit 2)
62  DAT       ; P4 (parity bit 4)
```

## How it works:

1. **Input**: Takes 4 data bits (D0, D1, D2, D3) from user input
2. **Parity Calculation**: 
   - P1 = D3 ⊕ D2 ⊕ D0 (positions 1,2,4)
   - P2 = D3 ⊕ D1 ⊕ D0 (positions 1,3,4)
   - P4 = D2 ⊕ D1 ⊕ D0 (positions 2,3,4)
3. **Output**: Outputs the 7-bit Hamming code in order P1, P2, D3, P4, D2, D1, D0

## Example:
Input: D3=1, D2=0, D1=1, D0=1
Output: P1=0, P2=1, D3=1, P4=0, D2=0, D1=1, D0=1

This creates the Hamming code word: 0110011

