# Cuckoo Hashing in LMC (Little Man Computer)

Here's an implementation of a simple Cuckoo hashing algorithm in LMC assembly language:

```assembly
; Cuckoo Hashing Implementation in LMC
; Hash table size: 7 slots (0-6)
; Uses two hash functions: h1(x) = x mod 7, h2(x) = (x div 10) mod 7

        INP             ; Read key to insert
        STA KEY         ; Store key

; Compute first hash function h1(x) = x mod 7
        LDA KEY         ; Load key
        LDA 100         ; Load 100 (divisor for mod operation)
        DIV             ; Divide key by 100
        LDA 101         ; Load 7 (modulus)
        DIV             ; Divide remainder by 7
        STA H1          ; Store first hash value

; Compute second hash function h2(x) = (x div 10) mod 7
        LDA KEY         ; Load key
        LDA 10          ; Load 10 (divisor for division)
        DIV             ; Divide key by 10
        LDA 101         ; Load 7 (modulus)
        DIV             ; Divide remainder by 7
        STA H2          ; Store second hash value

; Check if slot H1 is empty
        LDA H1          ; Load first hash
        LDA TABLE       ; Load table value at H1
        LDA 0           ; Load 0 (empty slot marker)
        BRZ INSERT1     ; If empty, insert here

; Check if slot H2 is empty
        LDA H2          ; Load second hash
        LDA TABLE       ; Load table value at H2
        LDA 0           ; Load 0 (empty slot marker)
        BRZ INSERT2     ; If empty, insert here

; Both slots occupied - perform cuckoo operation
        LDA H1          ; Load first hash
        LDA TABLE       ; Load value at H1
        STA OLDVAL      ; Store old value
        LDA H1          ; Load first hash
        LDA KEY         ; Load new key
        STA TABLE       ; Replace old value with new key

; Cuckoo kick-out process
        LDA OLDVAL      ; Load kicked-out value
        LDA H2          ; Load second hash
        LDA TABLE       ; Load value at H2
        LDA 0           ; Load 0 (empty slot marker)
        BRZ INSERT3     ; If empty, insert kicked value

; Continue cuckoo process (simplified)
        LDA H2          ; Load second hash
        LDA OLDVAL      ; Load kicked-out value
        STA TABLE       ; Store in slot H2

; Insert new key in first available slot
INSERT1 LDA H1          ; Load first hash
        LDA KEY         ; Load key
        STA TABLE       ; Store key in table

INSERT2 LDA H2          ; Load second hash
        LDA KEY         ; Load key
        STA TABLE       ; Store key in table

INSERT3 LDA H1          ; Load first hash
        LDA OLDVAL      ; Load kicked-out value
        STA TABLE       ; Store in table

        LDA 999         ; End program
        OUT             ; Output result
        HLT             ; Halt

; Data section
KEY     DAT 0           ; Input key storage
H1      DAT 0           ; First hash result
H2      DAT 0           ; Second hash result
OLDVAL  DAT 0           ; Old value during cuckoo operation
TABLE   DAT 0           ; Hash table (7 slots)
        DAT 0
        DAT 0
        DAT 0
        DAT 0
        DAT 0
        DAT 0
        DAT 0
        DAT 0           ; End of table
        DAT 100         ; Divisor for mod operation
        DAT 10          ; Divisor for second hash
        DAT 7           ; Modulus for hash functions
```

## How it works:

1. **Input**: Takes a key value from user input
2. **Hash Functions**: 
   - h1(x) = x mod 7 (first hash)
   - h2(x) = (x ÷ 10) mod 7 (second hash)
3. **Insertion Process**:
   - Check if slot h1(key) is empty
   - If empty, insert key there
   - If occupied, check slot h2(key)
   - If that's empty, insert there
   - If both occupied, use cuckoo kick-out mechanism
4. **Cuckoo Operation**: 
   - Remove existing value from one slot
   - Insert it in another slot using alternative hash
   - Continue until empty slot found

## Example Usage:

Input: 25
- h1(25) = 25 mod 7 = 4
- h2(25) = (25 ÷ 10) mod 7 = 2 mod 7 = 2
- If slot 4 is empty, insert 25 there
- If slot 2 is empty, insert 25 there
- If both occupied, kick out existing values and rehash

This is a simplified version that demonstrates the core concepts of Cuckoo hashing in LMC's limited instruction set.

