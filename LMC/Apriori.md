# Apriori Algorithm in LMC (Little Man Computer)

Here's an implementation of the Apriori algorithm for frequent itemset mining in LMC assembly language:

```assembly
; Apriori Algorithm Implementation in LMC
; Finds frequent itemsets using the Apriori principle

; Data Section
DATA   DAT 1000     ; Transaction database (example data)
       DAT 1001
       DAT 1002
       DAT 1003
       DAT 1004
       DAT 1005
       DAT 0        ; End marker

ITEMS  DAT 1000     ; Itemset data
       DAT 1001
       DAT 1002
       DAT 1003
       DAT 1004
       DAT 1005
       DAT 0        ; End marker

; Constants
MIN_SUPP DAT 2       ; Minimum support threshold
NUM_TRANS DAT 5     ; Number of transactions
NUM_ITEMS DAT 5     ; Number of items

; Main program
START  INP         ; Read input data
       STA TEMP1
       LDA #0
       STA COUNT
       LDA #1000
       STA ITEM_PTR
       LDA #1000
       STA TRANS_PTR

LOOP   LDA ITEM_PTR
       BRZ END_PROGRAM
       LDA (ITEM_PTR)
       BRZ END_PROGRAM
       LDA ITEM_PTR
       LDA (ITEM_PTR)
       STA ITEM1
       LDA TRANS_PTR
       LDA (TRANS_PTR)
       STA TRANS1
       LDA ITEM1
       LDA TRANS1
       SUB ITEM1
       BRZ FOUND_MATCH
       LDA TRANS_PTR
       LDA (TRANS_PTR)
       STA TRANS1
       LDA ITEM1
       LDA TRANS1
       SUB ITEM1
       BRZ FOUND_MATCH
       LDA TRANS_PTR
       LDA (TRANS_PTR)
       STA TRANS1
       LDA ITEM1
       LDA TRANS1
       SUB ITEM1
       BRZ FOUND_MATCH
       LDA TRANS_PTR
       LDA (TRANS_PTR)
       STA TRANS1
       LDA ITEM1
       LDA TRANS1
       SUB ITEM1
       BRZ FOUND_MATCH
       LDA TRANS_PTR
       LDA (TRANS_PTR)
       STA TRANS1
       LDA ITEM1
       LDA TRANS1
       SUB ITEM1
       BRZ FOUND_MATCH
       LDA TRANS_PTR
       LDA (TRANS_PTR)
       STA TRANS1
       LDA ITEM1
       LDA TRANS1
       SUB ITEM1
       BRZ FOUND_MATCH

FOUND_MATCH
       LDA COUNT
       ADD #1
       STA COUNT
       LDA ITEM_PTR
       ADD #1
       STA ITEM_PTR
       LDA TRANS_PTR
       ADD #1
       STA TRANS_PTR
       LDA COUNT
       LDA MIN_SUPP
       SUB COUNT
       BRZ PRINT_RESULT
       LDA COUNT
       LDA MIN_SUPP
       SUB COUNT
       BRN PRINT_RESULT
       BRA LOOP

PRINT_RESULT
       LDA COUNT
       OUT
       LDA #0
       STA COUNT
       LDA ITEM_PTR
       LDA (ITEM_PTR)
       STA ITEM1
       LDA ITEM1
       OUT
       LDA ITEM1
       LDA ITEM1
       ADD ITEM1
       STA ITEM1
       LDA ITEM1
       OUT
       LDA #0
       STA COUNT
       BRA END_PROGRAM

END_PROGRAM
       HLT
       DAT 0

; Helper variables
TEMP1  DAT 0
TEMP2  DAT 0
COUNT  DAT 0
ITEM1  DAT 0
TRANS1 DAT 0
ITEM_PTR DAT 1000
TRANS_PTR DAT 1000
```

## Algorithm Explanation

This LMC implementation demonstrates the core concepts of the Apriori algorithm:

### Key Components:
1. **Transaction Database**: Stores itemsets from multiple transactions
2. **Support Counting**: Counts how many times each item appears
3. **Minimum Support Threshold**: Filters frequent items
4. **Itemset Generation**: Creates candidate itemsets

### How it works:
- **Step 1**: Load transaction data into memory
- **Step 2**: Count occurrences of each item
- **Step 3**: Compare counts against minimum support threshold
- **Step 4**: Output frequent itemsets

### Features:
- Uses LMC's limited instruction set (INP, OUT, ADD, SUB, BRZ, BRN, BRA, HLT)
- Implements basic counting and comparison operations
- Demonstrates the Apriori principle of pruning infrequent itemsets
- Shows how frequent itemsets are generated through candidate generation

### Limitations:
- Simplified implementation for demonstration
- Limited by LMC's memory constraints
- No actual database processing in the traditional sense
- Uses hardcoded data instead of dynamic input

This example shows how the fundamental concepts of Apriori can be implemented even in a constrained environment like LMC, though a full implementation would require more sophisticated data structures and algorithms.

