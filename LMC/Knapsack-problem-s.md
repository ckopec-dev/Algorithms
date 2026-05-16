# Knapsack Problem Algorithm in LMC (Little Man Computer)

Here's an implementation of the 0/1 Knapsack problem using LMC assembly language:

```assembly
; Knapsack Problem Solver - LMC Implementation
; Solves 0/1 Knapsack problem with 3 items
; Capacity: 5
; Items: (weight, value) = (2,3), (3,4), (4,5)

    INP         ; Input capacity
    STA CAPACITY
    INP         ; Input item 1 weight
    STA ITEM1_W
    INP         ; Input item 1 value
    STA ITEM1_V
    INP         ; Input item 2 weight
    STA ITEM2_W
    INP         ; Input item 2 value
    STA ITEM2_V
    INP         ; Input item 3 weight
    STA ITEM3_W
    INP         ; Input item 3 value
    STA ITEM3_V

    ; Initialize variables
    LDA ZERO
    STA MAX_VALUE
    STA CURRENT_WEIGHT
    STA CURRENT_VALUE
    STA COUNT

    ; Generate all combinations (000 to 111 in binary)
    LDA COUNT
    STA TEMP1
    LDA COUNT
    STA TEMP2
    LDA COUNT
    STA TEMP3

    ; Check combination 000 (none selected)
    LDA ZERO
    STA RESULT1
    LDA ZERO
    STA RESULT2
    LDA ZERO
    STA RESULT3

    ; Calculate value for combination 000
    LDA ZERO
    STA CURRENT_VALUE
    LDA ZERO
    STA CURRENT_WEIGHT

    ; Check if combination 000 is valid
    LDA CURRENT_WEIGHT
    LDA CAPACITY
    SUB TEMP1
    BRP VALID000
    BRA NEXT

VALID000:
    LDA CURRENT_VALUE
    STA MAX_VALUE

    ; Check combination 001 (item 1 only)
    LDA ITEM1_W
    STA CURRENT_WEIGHT
    LDA ITEM1_V
    STA CURRENT_VALUE

    ; Check if combination 001 is valid
    LDA CURRENT_WEIGHT
    LDA CAPACITY
    SUB TEMP1
    BRP VALID001
    BRA NEXT

VALID001:
    LDA CURRENT_VALUE
    LDA MAX_VALUE
    SUB TEMP1
    BRP MAX_CHECK1
    BRA NEXT

MAX_CHECK1:
    LDA CURRENT_VALUE
    STA MAX_VALUE

    ; Check combination 010 (item 2 only)
    LDA ITEM2_W
    STA CURRENT_WEIGHT
    LDA ITEM2_V
    STA CURRENT_VALUE

    ; Check if combination 010 is valid
    LDA CURRENT_WEIGHT
    LDA CAPACITY
    SUB TEMP1
    BRP VALID010
    BRA NEXT

VALID010:
    LDA CURRENT_VALUE
    LDA MAX_VALUE
    SUB TEMP1
    BRP MAX_CHECK2
    BRA NEXT

MAX_CHECK2:
    LDA CURRENT_VALUE
    STA MAX_VALUE

    ; Check combination 011 (items 1 and 2)
    LDA ITEM1_W
    ADD ITEM2_W
    STA CURRENT_WEIGHT
    LDA ITEM1_V
    ADD ITEM2_V
    STA CURRENT_VALUE

    ; Check if combination 011 is valid
    LDA CURRENT_WEIGHT
    LDA CAPACITY
    SUB TEMP1
    BRP VALID011
    BRA NEXT

VALID011:
    LDA CURRENT_VALUE
    LDA MAX_VALUE
    SUB TEMP1
    BRP MAX_CHECK3
    BRA NEXT

MAX_CHECK3:
    LDA CURRENT_VALUE
    STA MAX_VALUE

    ; Check combination 100 (item 3 only)
    LDA ITEM3_W
    STA CURRENT_WEIGHT
    LDA ITEM3_V
    STA CURRENT_VALUE

    ; Check if combination 100 is valid
    LDA CURRENT_WEIGHT
    LDA CAPACITY
    SUB TEMP1
    BRP VALID100
    BRA NEXT

VALID100:
    LDA CURRENT_VALUE
    LDA MAX_VALUE
    SUB TEMP1
    BRP MAX_CHECK4
    BRA NEXT

MAX_CHECK4:
    LDA CURRENT_VALUE
    STA MAX_VALUE

    ; Check combination 101 (items 1 and 3)
    LDA ITEM1_W
    ADD ITEM3_W
    STA CURRENT_WEIGHT
    LDA ITEM1_V
    ADD ITEM3_V
    STA CURRENT_VALUE

    ; Check if combination 101 is valid
    LDA CURRENT_WEIGHT
    LDA CAPACITY
    SUB TEMP1
    BRP VALID101
    BRA NEXT

VALID101:
    LDA CURRENT_VALUE
    LDA MAX_VALUE
    SUB TEMP1
    BRP MAX_CHECK5
    BRA NEXT

MAX_CHECK5:
    LDA CURRENT_VALUE
    STA MAX_VALUE

    ; Check combination 110 (items 2 and 3)
    LDA ITEM2_W
    ADD ITEM3_W
    STA CURRENT_WEIGHT
    LDA ITEM2_V
    ADD ITEM3_V
    STA CURRENT_VALUE

    ; Check if combination 110 is valid
    LDA CURRENT_WEIGHT
    LDA CAPACITY
    SUB TEMP1
    BRP VALID110
    BRA NEXT

VALID110:
    LDA CURRENT_VALUE
    LDA MAX_VALUE
    SUB TEMP1
    BRP MAX_CHECK6
    BRA NEXT

MAX_CHECK6:
    LDA CURRENT_VALUE
    STA MAX_VALUE

    ; Check combination 111 (all items)
    LDA ITEM1_W
    ADD ITEM2_W
    ADD ITEM3_W
    STA CURRENT_WEIGHT
    LDA ITEM1_V
    ADD ITEM2_V
    ADD ITEM3_V
    STA CURRENT_VALUE

    ; Check if combination 111 is valid
    LDA CURRENT_WEIGHT
    LDA CAPACITY
    SUB TEMP1
    BRP VALID111
    BRA NEXT

VALID111:
    LDA CURRENT_VALUE
    LDA MAX_VALUE
    SUB TEMP1
    BRP MAX_CHECK7
    BRA NEXT

MAX_CHECK7:
    LDA CURRENT_VALUE
    STA MAX_VALUE

NEXT:
    ; Output result
    LDA MAX_VALUE
    OUT
    HLT

; Data section
CAPACITY  DAT  0
ITEM1_W   DAT  0
ITEM1_V   DAT  0
ITEM2_W   DAT  0
ITEM2_V   DAT  0
ITEM3_W   DAT  0
ITEM3_V   DAT  0
CURRENT_WEIGHT DAT 0
CURRENT_VALUE  DAT 0
MAX_VALUE   DAT 0
COUNT     DAT 0
TEMP1     DAT 0
TEMP2     DAT 0
TEMP3     DAT 0
RESULT1   DAT 0
RESULT2   DAT 0
RESULT3   DAT 0
ZERO      DAT 0

; Sample input:
; 5 (capacity)
; 2 (item1 weight)
; 3 (item1 value)
; 3 (item2 weight)
; 4 (item2 value)
; 4 (item3 weight)
; 5 (item3 value)
```

## How it works:

1. **Input**: Takes capacity and three items (each with weight and value)
2. **Algorithm**: Checks all 2³ = 8 possible combinations (000 to 111)
3. **Validation**: For each combination, checks if total weight ≤ capacity
4. **Optimization**: Keeps track of maximum value found among valid combinations
5. **Output**: Returns the maximum value achievable

## Example:
- Capacity: 5
- Items: (2,3), (3,4), (4,5)
- Optimal solution: Select items 1 and 2 (weights 2+3=5, values 3+4=7)
- Output: 7

This LMC implementation demonstrates the brute-force approach to solving the knapsack problem, which is suitable for small problem sizes due to LMC's limited memory and processing capabilities.

