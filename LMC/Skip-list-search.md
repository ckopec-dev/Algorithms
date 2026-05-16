# Skip List Search Algorithm in LMC

```assembly
        ORG 100
        LDA HEAD     ; Load head pointer
        STA CURRENT  ; Store in current pointer
        LDA LEVEL    ; Load maximum level
        STA LVL      ; Store in level counter

SEARCH_LOOP
        LDA LVL      ; Load current level
        BRZ FOUND    ; If level is 0, we found the position
        LDA CURRENT  ; Load current pointer
        LDA LEVEL    ; Load level field
        STA NEXT_PTR ; Store next pointer
        LDA NEXT_PTR ; Load next pointer
        LDA KEY      ; Load search key
        LDA CURRENT  ; Load current key
        SUB KEY      ; Compare keys
        BRZ FOUND    ; If equal, found it
        BRN SKIP     ; If current key > search key, skip to next level
        LDA NEXT_PTR ; Load next pointer
        STA CURRENT  ; Update current pointer
        BR SEARCH_LOOP ; Continue searching

SKIP
        LDA LVL      ; Load current level
        SUB ONE      ; Decrement level
        STA LVL      ; Store back
        BR SEARCH_LOOP ; Continue at lower level

FOUND
        LDA CURRENT  ; Load found node
        LDA KEY      ; Load search key
        LDA CURRENT  ; Load current key
        SUB KEY      ; Compare keys
        BRZ SUCCESS  ; If equal, success
        BRN NOT_FOUND ; If not found, go to not found

SUCCESS
        LDA CURRENT  ; Load found node
        LDA VALUE    ; Load value
        HLT          ; Halt

NOT_FOUND
        LDA ZERO     ; Load zero
        HLT          ; Halt

        DAT 0        ; HEAD pointer
        DAT 0        ; CURRENT pointer
        DAT 0        ; LEVEL field
        DAT 0        ; NEXT pointer
        DAT 0        ; KEY field
        DAT 0        ; VALUE field
        DAT 0        ; LVL counter
        DAT 1        ; ONE constant
        DAT 0        ; ZERO constant
```

## Explanation

This LMC implementation demonstrates a skip list search algorithm with the following components:

**Key Features:**
- **HEAD**: Points to the head of the skip list
- **CURRENT**: Tracks the current node during search
- **LEVEL**: Maximum level of the skip list
- **NEXT_PTR**: Points to the next node at current level
- **KEY**: The search key value
- **VALUE**: The value associated with the key

**Algorithm Flow:**
1. Start at the highest level of the skip list
2. Compare search key with current node key
3. If current key > search key, move to next node at same level
4. If current key < search key, move to next level and continue search
5. If keys match, return the value
6. If we reach level 0 and still haven't found the key, return not found

**LMC Instructions Used:**
- `LDA` (Load Accumulator)
- `STA` (Store Accumulator)
- `SUB` (Subtract)
- `BRZ` (Branch if Zero)
- `BRN` (Branch if Negative)
- `BR` (Branch)
- `HLT` (Halt)

This implementation provides a basic framework for skip list search that can be extended with additional functionality like insertion and deletion operations.

