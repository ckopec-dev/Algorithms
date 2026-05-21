# Vogel's Approximation Method in LMC

Here's an implementation of Vogel's Approximation Method for solving transportation problems in LMC (Little Man Computer) assembly language:

```assembly
; Vogel's Approximation Method Implementation
; Transportation Problem Solver

    INP         ; Input number of rows (suppliers)
    STA ROWS
    INP         ; Input number of columns (demanders)
    STA COLS
    LDA ZERO
    STA TOTAL_COST
    STA ROW_SUM
    STA COL_SUM

; Initialize cost matrix
    LDA ROWS
    STA ROW_COUNT
INIT_ROWS
    LDA COLS
    STA COL_COUNT
INIT_COLS
    INP         ; Input cost c[i][j]
    STA COSTS,X
    LDA COL_COUNT
    SUB ONE
    STA COL_COUNT
    BRZ INIT_ROWS
    LDA COLS
    ADD COL_COUNT
    STA X
    BRZ INIT_COLS

; Initialize supply and demand arrays
    LDA ROWS
    STA ROW_COUNT
INIT_SUPPLY
    INP         ; Input supply s[i]
    STA SUPPLY,X
    LDA ROW_COUNT
    SUB ONE
    STA ROW_COUNT
    BRZ INIT_SUPPLY

    LDA COLS
    STA COL_COUNT
INIT_DEMAND
    INP         ; Input demand d[j]
    STA DEMAND,X
    LDA COL_COUNT
    SUB ONE
    STA COL_COUNT
    BRZ INIT_DEMAND

; Main VAM algorithm
    LDA ROWS
    STA ROW_COUNT
    LDA COLS
    STA COL_COUNT

VAM_LOOP
    LDA ROW_COUNT
    BRZ VAM_END
    LDA COL_COUNT
    BRZ VAM_END

    ; Calculate penalties for each row
    LDA ROWS
    STA ROW_COUNT
CALC_ROW_PENALTIES
    LDA ZERO
    STA MIN1
    LDA ZERO
    STA MIN2
    LDA ZERO
    STA MIN_POS
    LDA ZERO
    STA COL_COUNT

ROW_MIN_LOOP
    LDA COL_COUNT
    BRZ ROW_MIN_END
    LDA COSTS,X
    STA TEMP
    LDA MIN1
    BRZ SET_MIN1
    LDA TEMP
    SUB MIN1
    BRZ NEXT_COL
    BRZ SET_MIN1
    LDA TEMP
    SUB MIN2
    BRZ SET_MIN2
    LDA MIN1
    SUB MIN2
    BRZ SET_MIN2
    LDA TEMP
    SUB MIN1
    BRZ SET_MIN1
    LDA TEMP
    SUB MIN2
    BRZ SET_MIN2
    LDA MIN1
    SUB MIN2
    BRZ SET_MIN2

SET_MIN1
    LDA TEMP
    STA MIN1
    LDA COL_COUNT
    STA MIN_POS
    BRZ NEXT_COL

SET_MIN2
    LDA MIN1
    STA MIN2
    LDA COL_COUNT
    STA MIN_POS
    BRZ NEXT_COL

NEXT_COL
    LDA COL_COUNT
    ADD ONE
    STA COL_COUNT
    BRZ ROW_MIN_LOOP
ROW_MIN_END

    ; Calculate row penalty
    LDA MIN1
    LDA MIN2
    SUB MIN1
    STA PENALTY,X

    LDA ROW_COUNT
    SUB ONE
    STA ROW_COUNT
    BRZ CALC_ROW_PENALTIES

    ; Calculate penalties for each column
    LDA COLS
    STA COL_COUNT
CALC_COL_PENALTIES
    LDA ZERO
    STA MIN1
    LDA ZERO
    STA MIN2
    LDA ZERO
    STA MIN_POS
    LDA ZERO
    STA ROW_COUNT

COL_MIN_LOOP
    LDA ROW_COUNT
    BRZ COL_MIN_END
    LDA COSTS,X
    STA TEMP
    LDA MIN1
    BRZ SET_MIN1_COL
    LDA TEMP
    SUB MIN1
    BRZ NEXT_ROW
    BRZ SET_MIN1_COL
    LDA TEMP
    SUB MIN2
    BRZ SET_MIN2_COL
    LDA MIN1
    SUB MIN2
    BRZ SET_MIN2_COL
    LDA TEMP
    SUB MIN1
    BRZ SET_MIN1_COL
    LDA TEMP
    SUB MIN2
    BRZ SET_MIN2_COL
    LDA MIN1
    SUB MIN2
    BRZ SET_MIN2_COL

SET_MIN1_COL
    LDA TEMP
    STA MIN1
    LDA ROW_COUNT
    STA MIN_POS
    BRZ NEXT_ROW

SET_MIN2_COL
    LDA MIN1
    STA MIN2
    LDA ROW_COUNT
    STA MIN_POS
    BRZ NEXT_ROW

NEXT_ROW
    LDA ROW_COUNT
    ADD ONE
    STA ROW_COUNT
    BRZ COL_MIN_LOOP
COL_MIN_END

    ; Calculate column penalty
    LDA MIN1
    LDA MIN2
    SUB MIN1
    STA PENALTY,X

    LDA COL_COUNT
    SUB ONE
    STA COL_COUNT
    BRZ CALC_COL_PENALTIES

    ; Find maximum penalty
    LDA ZERO
    STA MAX_PENALTY
    LDA ZERO
    STA MAX_POS
    LDA ROWS
    STA ROW_COUNT

MAX_PENALTY_LOOP
    LDA PENALTY,X
    LDA MAX_PENALTY
    SUB MAX_PENALTY
    BRZ MAX_PENALTY_NEXT
    LDA PENALTY,X
    STA MAX_PENALTY
    LDA X
    STA MAX_POS

MAX_PENALTY_NEXT
    LDA ROW_COUNT
    SUB ONE
    STA ROW_COUNT
    BRZ MAX_PENALTY_LOOP

    ; Allocate minimum cost cell
    LDA MAX_POS
    STA ALLOC_POS
    LDA SUPPLY,X
    LDA DEMAND,X
    SUB SUPPLY,X
    BRZ SUPPLY_LESS
    LDA SUPPLY,X
    STA ALLOC_AMOUNT
    BRZ ALLOCATE

SUPPLY_LESS
    LDA DEMAND,X
    STA ALLOC_AMOUNT

ALLOCATE
    LDA ALLOC_AMOUNT
    LDA COSTS,X
    MUL ALLOC_AMOUNT
    ADD TOTAL_COST
    STA TOTAL_COST

    ; Update supply and demand
    LDA SUPPLY,X
    LDA ALLOC_AMOUNT
    SUB SUPPLY,X
    STA SUPPLY,X

    LDA DEMAND,X
    LDA ALLOC_AMOUNT
    SUB DEMAND,X
    STA DEMAND,X

    ; Mark allocated cell
    LDA ONE
    STA ALLOCATED,X

    ; Check if allocation is complete
    LDA ROWS
    STA ROW_COUNT
    LDA COLS
    STA COL_COUNT

    LDA ROW_COUNT
    BRZ VAM_END
    LDA COL_COUNT
    BRZ VAM_END

    LDA ZERO
    STA TEMP_COUNT
    LDA ROWS
    STA ROW_COUNT
CHECK_ROWS
    LDA SUPPLY,X
    BRZ ROW_EMPTY
    LDA ZERO
    STA TEMP_COUNT
    BRZ CHECK_ROWS

ROW_EMPTY
    LDA ROW_COUNT
    SUB ONE
    STA ROW_COUNT
    BRZ CHECK_ROWS

    LDA ZERO
    STA TEMP_COUNT
    LDA COLS
    STA COL_COUNT
CHECK_COLS
    LDA DEMAND,X
    BRZ COL_EMPTY
    LDA ZERO
    STA TEMP_COUNT
    BRZ CHECK_COLS

COL_EMPTY
    LDA COL_COUNT
    SUB ONE
    STA COL_COUNT
    BRZ CHECK_COLS

    LDA ROW_COUNT
    SUB ONE
    STA ROW_COUNT
    BRZ VAM_LOOP

VAM_END
    LDA TOTAL_COST
    OUT         ; Output total cost
    HLT         ; Halt

; Data areas
ROWS     DAT 0
COLS     DAT 0
TOTAL_COST DAT 0
ROW_COUNT DAT 0
COL_COUNT DAT 0
MAX_PENALTY DAT 0
MAX_POS DAT 0
ALLOC_POS DAT 0
ALLOC_AMOUNT DAT 0
TEMP_COUNT DAT 0

; Arrays
COSTS    DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
SUPPLY   DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
DEMAND   DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
PENALTY  DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
ALLOCATED DAT 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

; Constants
ZERO     DAT 0
ONE      DAT 1
TEMP     DAT 0
X        DAT 0
MIN1     DAT 0
MIN2     DAT 0
MIN_POS  DAT 0
```

## Example Usage

To run this algorithm with a sample problem:

**Input:**
```
3    ; 3 suppliers
4    ; 4 demanders
10   15   20   25   ; Costs for supplier 1
12   18   22   15   ; Costs for supplier 2
15   12   18   20   ; Costs for supplier 3
30   40   50   60   ; Supplies
25   35   45   55   ; Demands
```

**Output:**
```
Total cost: [calculated value]
```

## Algorithm Steps

1. **Initialize**: Input supply, demand, and cost matrix
2. **Calculate penalties**: For each row and column, find the difference between the two smallest costs
3. **Find maximum penalty**: Identify the row or column with the highest penalty
4. **Allocate**: Assign the maximum possible amount to the cell with the minimum cost in that row/column
5. **Update**: Adjust supply/demand and mark the cell as allocated
6. **Repeat**: Continue until all allocations are made

This implementation demonstrates the core logic of Vogel's Approximation Method in LMC assembly language, showing how to handle matrix operations and iterative optimization.

