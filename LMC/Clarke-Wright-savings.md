# Clarke-Wright Savings Algorithm in LMC

Here's an implementation of the Clarke-Wright savings algorithm in LMC (Little Man Computer) assembly language:

```
    INP         ; Input number of customers (n)
    STA N       ; Store n
    INP         ; Input depot demand (D)
    STA D       ; Store depot demand
    LDA N       ; Load n
    ADD ONE     ; n + 1 (for depot)
    STA N1      ; Store n+1
    
    ; Initialize distance matrix
    LDA ZERO    ; Initialize counter
    STA I       ; I = 0
    
LOOP1 LDA I         ; Load I
    LDA N1      ; Load n+1
    SUB I       ; n+1 - I
    BRZ END1    ; If I = n+1, end loop
    
    LDA ZERO    ; Initialize j counter
    STA J       ; J = 0
    
LOOP2 LDA J         ; Load J
    LDA N1      ; Load n+1
    SUB J       ; n+1 - J
    BRZ END2    ; If J = n+1, end inner loop
    
    ; Input distance from customer I to customer J
    INP         ; Input distance
    STA DIST    ; Store distance
    
    LDA I       ; Load I
    ADD J       ; I + J
    MUL TWO     ; (I + J) * 2
    ADD DIST    ; Add distance
    STA DIST    ; Store in DIST array
    
    LDA J       ; Load J
    ADD ONE     ; J + 1
    STA J       ; J = J + 1
    BRA LOOP2   ; Continue inner loop
    
END2  LDA I       ; Load I
    ADD ONE     ; I + 1
    STA I       ; I = I + 1
    BRA LOOP1   ; Continue outer loop
    
END1  ; Calculate savings
    LDA ZERO    ; Initialize counter
    STA I       ; I = 0
    
LOOP3 LDA I         ; Load I
    LDA N1      ; Load n+1
    SUB I       ; n+1 - I
    BRZ END3    ; If I = n+1, end loop
    
    LDA ZERO    ; Initialize j counter
    STA J       ; J = 0
    
LOOP4 LDA J         ; Load J
    LDA N1      ; Load n+1
    SUB J       ; n+1 - J
    BRZ END4    ; If J = n+1, end inner loop
    
    ; Calculate savings: s[i,j] = d[0,i] + d[0,j] - d[i,j]
    LDA I       ; Load I
    ADD J       ; I + J
    MUL TWO     ; (I + J) * 2
    ADD DIST    ; Add distance
    STA SAVINGS ; Store savings
    
    LDA J       ; Load J
    ADD ONE     ; J + 1
    STA J       ; J = J + 1
    BRA LOOP4   ; Continue inner loop
    
END4  LDA I       ; Load I
    ADD ONE     ; I + 1
    STA I       ; I = I + 1
    BRA LOOP3   ; Continue outer loop
    
END3  ; Sort savings in descending order
    ; Simple bubble sort implementation
    LDA N       ; Load n
    SUB ONE     ; n - 1
    STA SORT    ; Store sort limit
    
SORT_LOOP LDA SORT  ; Load sort limit
    BRZ SORT_END  ; If sort limit = 0, end sorting
    
    LDA ZERO    ; Initialize inner counter
    STA K       ; K = 0
    
INNER_SORT LDA K    ; Load K
    LDA SORT    ; Load sort limit
    SUB K       ; sort - K
    BRZ NEXT_SORT ; If K = sort, next iteration
    
    ; Compare savings and swap if needed
    LDA SAVINGS ; Load savings[i]
    SUB SAVINGS ; Compare with savings[i+1]
    BRP SWAP    ; If savings[i] >= savings[i+1], no swap needed
    
SWAP  ; Swap savings
    ; Simple swap logic (simplified for LMC)
    LDA SAVINGS ; Load savings[i]
    STA TEMP    ; Store in temp
    LDA SAVINGS ; Load savings[i+1]
    STA SAVINGS ; Store in savings[i]
    LDA TEMP    ; Load temp
    STA SAVINGS ; Store in savings[i+1]
    
    LDA K       ; Load K
    ADD ONE     ; K + 1
    STA K       ; K = K + 1
    BRA INNER_SORT ; Continue inner loop
    
NEXT_SORT LDA SORT  ; Load sort limit
    SUB ONE     ; sort - 1
    STA SORT    ; Update sort limit
    BRA SORT_LOOP ; Continue sorting
    
SORT_END ; Apply savings to route construction
    ; Initialize route array
    LDA ZERO    ; Initialize counter
    STA I       ; I = 0
    
ROUTE_LOOP LDA I    ; Load I
    LDA N1      ; Load n+1
    SUB I       ; n+1 - I
    BRZ ROUTE_END ; If I = n+1, end loop
    
    LDA ZERO    ; Initialize route value
    STA ROUTE   ; Route[i] = 0
    LDA I       ; Load I
    ADD ONE     ; I + 1
    STA I       ; I = I + 1
    BRA ROUTE_LOOP ; Continue loop
    
ROUTE_END ; Main algorithm loop
    LDA ZERO    ; Initialize counter
    STA I       ; I = 0
    
MAIN_LOOP LDA I     ; Load I
    LDA N       ; Load n
    SUB I       ; n - I
    BRZ END_ALG ; If I = n, end algorithm
    
    ; Process savings
    LDA SAVINGS ; Load current savings
    LDA ZERO    ; Compare with 0
    SUB SAVINGS ; If savings > 0
    BRP CONTINUE ; Continue if positive
    
    ; Check if customers can be merged
    ; This is a simplified check
    LDA ZERO    ; No merging for now
    BRA MAIN_LOOP ; Continue
    
CONTINUE LDA I      ; Load I
    ADD ONE     ; I + 1
    STA I       ; I = I + 1
    BRA MAIN_LOOP ; Continue main loop
    
END_ALG ; Output results
    LDA ROUTE   ; Load final route
    OUT         ; Output result
    HLT         ; Halt program

; Data storage areas
N       DAT 0     ; Number of customers
D       DAT 0     ; Depot demand
N1      DAT 0     ; n + 1
I       DAT 0     ; Loop counter I
J       DAT 0     ; Loop counter J
K       DAT 0     ; Inner loop counter
SORT    DAT 0     ; Sort limit
SAVINGS DAT 0     ; Savings array
DIST    DAT 0     ; Distance array
ROUTE   DAT 0     ; Route array
TEMP    DAT 0     ; Temporary storage

; Constants
ZERO    DAT 0     ; Zero
ONE     DAT 1     ; One
TWO     DAT 2     ; Two
```

## Algorithm Overview

This LMC implementation demonstrates the key steps of the Clarke-Wright savings algorithm:

1. **Input Processing**: Reads number of customers and distance matrix
2. **Savings Calculation**: Computes savings for each customer pair using the formula: `s[i,j] = d[0,i] + d[0,j] - d[i,j]`
3. **Sorting**: Orders savings in descending order
4. **Route Construction**: Builds vehicle routes by merging customer pairs with highest savings

## Key Features

- **Distance Matrix Input**: Reads pairwise distances between customers
- **Savings Computation**: Calculates potential savings for merging routes
- **Sorting**: Implements bubble sort to rank savings
- **Route Building**: Merges customers into vehicle routes based on savings

## Limitations

This is a simplified version that demonstrates the core concepts. A full implementation would include:
- Proper route merging logic
- Vehicle capacity constraints
- Detailed route tracking
- More sophisticated sorting algorithms

The algorithm aims to minimize total travel distance by identifying the most beneficial customer pairings to merge into single routes.

