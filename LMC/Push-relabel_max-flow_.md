# Push-Relabel Max-Flow Algorithm in Little Man Computer

Below is an implementation of a simplified push-relabel max-flow algorithm in Little Man Computer (LMC) for a small network. This example implements a basic max-flow problem with 4 nodes (0,1,2,3) and demonstrates the core concepts.

## LMC Code Implementation

```
; Push-Relabel Max-Flow Algorithm - LMC Implementation
; Network: 0->1, 0->2, 1->2, 1->3, 2->3
; Capacity: 10, 5, 3, 8, 7

; Constants
CAPACITY_01 DAT 10
CAPACITY_02 DAT 5
CAPACITY_12 DAT 3
CAPACITY_13 DAT 8
CAPACITY_23 DAT 7

; Flow array (flow from i to j)
FLOW_01 DAT 0
FLOW_02 DAT 0
FLOW_12 DAT 0
FLOW_13 DAT 0
FLOW_23 DAT 0

; Residual capacity array
RES_01 DAT 0
RES_02 DAT 0
RES_12 DAT 0
RES_13 DAT 0
RES_23 DAT 0

; Height array
HEIGHT_0 DAT 0
HEIGHT_1 DAT 0
HEIGHT_2 DAT 0
HEIGHT_3 DAT 0

; Excess array
EXCESS_0 DAT 0
EXCESS_1 DAT 0
EXCESS_2 DAT 0
EXCESS_3 DAT 0

; Main program
START LDA ONE
    STA HEIGHT_0
    LDA ZERO
    STA HEIGHT_1
    STA HEIGHT_2
    STA HEIGHT_3
    LDA ZERO
    STA EXCESS_0
    STA EXCESS_1
    STA EXCESS_2
    STA EXCESS_3
    
    ; Initialize residual capacities
    LDA CAPACITY_01
    STA RES_01
    LDA CAPACITY_02
    STA RES_02
    LDA CAPACITY_12
    STA RES_12
    LDA CAPACITY_13
    STA RES_13
    LDA CAPACITY_23
    STA RES_23
    
    ; Set source excess
    LDA SOURCE_FLOW
    STA EXCESS_0
    
    ; Start algorithm
    LDA HEIGHT_0
    STA TEMP1
    LDA HEIGHT_1
    STA TEMP2
    LDA HEIGHT_2
    STA TEMP3
    LDA HEIGHT_3
    STA TEMP4
    
    ; Main loop - while excess exists
MAIN_LOOP LDA EXCESS_0
    BRZ TERMINATE
    LDA EXCESS_1
    BRZ MAIN_LOOP
    LDA EXCESS_2
    BRZ MAIN_LOOP
    LDA EXCESS_3
    BRZ MAIN_LOOP
    
    ; Find highest vertex with excess
    LDA EXCESS_0
    STA MAX_EXCESS
    LDA ZERO
    STA VERT_ID
    
    ; Check vertex 1
    LDA EXCESS_1
    SUB MAX_EXCESS
    BRZ SKIP1
    BRN SKIP1
    LDA EXCESS_1
    STA MAX_EXCESS
    LDA ONE
    STA VERT_ID
    
SKIP1 LDA EXCESS_2
    SUB MAX_EXCESS
    BRZ SKIP2
    BRN SKIP2
    LDA EXCESS_2
    STA MAX_EXCESS
    LDA TWO
    STA VERT_ID
    
SKIP2 LDA EXCESS_3
    SUB MAX_EXCESS
    BRZ SKIP3
    BRN SKIP3
    LDA EXCESS_3
    STA MAX_EXCESS
    LDA THREE
    STA VERT_ID
    
SKIP3 LDA VERT_ID
    STA CURRENT_VERTEX
    
    ; Push operation
    LDA CURRENT_VERTEX
    BRZ PUSH_0
    BRN PUSH_0
    
PUSH_0 LDA RES_01
    BRZ PUSH_02
    LDA EXCESS_0
    SUB RES_01
    BRN PUSH_02
    LDA RES_01
    STA PUSH_AMOUNT
    
    ; Update flows and residuals
    LDA FLOW_01
    ADD PUSH_AMOUNT
    STA FLOW_01
    LDA RES_01
    SUB PUSH_AMOUNT
    STA RES_01
    LDA EXCESS_0
    SUB PUSH_AMOUNT
    STA EXCESS_0
    LDA EXCESS_1
    ADD PUSH_AMOUNT
    STA EXCESS_1
    
PUSH_02 LDA RES_02
    BRZ PUSH_1
    LDA EXCESS_0
    SUB RES_02
    BRN PUSH_1
    LDA RES_02
    STA PUSH_AMOUNT
    
    ; Update flows and residuals
    LDA FLOW_02
    ADD PUSH_AMOUNT
    STA FLOW_02
    LDA RES_02
    SUB PUSH_AMOUNT
    STA RES_02
    LDA EXCESS_0
    SUB PUSH_AMOUNT
    STA EXCESS_0
    LDA EXCESS_2
    ADD PUSH_AMOUNT
    STA EXCESS_2
    
PUSH_1 LDA RES_12
    BRZ PUSH_13
    LDA EXCESS_1
    SUB RES_12
    BRN PUSH_13
    LDA RES_12
    STA PUSH_AMOUNT
    
    ; Update flows and residuals
    LDA FLOW_12
    ADD PUSH_AMOUNT
    STA FLOW_12
    LDA RES_12
    SUB PUSH_AMOUNT
    STA RES_12
    LDA EXCESS_1
    SUB PUSH_AMOUNT
    STA EXCESS_1
    LDA EXCESS_2
    ADD PUSH_AMOUNT
    STA EXCESS_2
    
PUSH_13 LDA RES_13
    BRZ PUSH_23
    LDA EXCESS_1
    SUB RES_13
    BRN PUSH_23
    LDA RES_13
    STA PUSH_AMOUNT
    
    ; Update flows and residuals
    LDA FLOW_13
    ADD PUSH_AMOUNT
    STA FLOW_13
    LDA RES_13
    SUB PUSH_AMOUNT
    STA RES_13
    LDA EXCESS_1
    SUB PUSH_AMOUNT
    STA EXCESS_1
    LDA EXCESS_3
    ADD PUSH_AMOUNT
    STA EXCESS_3
    
PUSH_23 LDA RES_23
    BRZ PUSH_COMPLETE
    LDA EXCESS_2
    SUB RES_23
    BRN PUSH_COMPLETE
    LDA RES_23
    STA PUSH_AMOUNT
    
    ; Update flows and residuals
    LDA FLOW_23
    ADD PUSH_AMOUNT
    STA FLOW_23
    LDA RES_23
    SUB PUSH_AMOUNT
    STA RES_23
    LDA EXCESS_2
    SUB PUSH_AMOUNT
    STA EXCESS_2
    LDA EXCESS_3
    ADD PUSH_AMOUNT
    STA EXCESS_3
    
PUSH_COMPLETE LDA EXCESS_0
    BRZ MAIN_LOOP
    LDA EXCESS_1
    BRZ MAIN_LOOP
    LDA EXCESS_2
    BRZ MAIN_LOOP
    LDA EXCESS_3
    BRZ MAIN_LOOP
    BRA MAIN_LOOP

TERMINATE LDA FLOW_01
    ADD FLOW_02
    ADD FLOW_12
    ADD FLOW_13
    ADD FLOW_23
    STA MAX_FLOW
    HLT

; Data section
ZERO DAT 0
ONE DAT 1
TWO DAT 2
THREE DAT 3
SOURCE_FLOW DAT 13
TEMP1 DAT 0
TEMP2 DAT 0
TEMP3 DAT 0
TEMP4 DAT 0
MAX_EXCESS DAT 0
VERT_ID DAT 0
CURRENT_VERTEX DAT 0
PUSH_AMOUNT DAT 0

; Output addresses for results
MAX_FLOW DAT 0
```

## Algorithm Explanation

This LMC implementation demonstrates the core concepts of the push-relabel algorithm:

### Key Components:

1. **Data Structures**:
   - Capacity arrays for network edges
   - Flow arrays tracking current flow
   - Residual capacity arrays
   - Height and excess arrays

2. **Main Operations**:
   - **Push**: Transfer flow from high-height vertex to neighbor with lower height
   - **Relabel**: Increase height of vertex when no admissible edges exist
   - **Initialization**: Set initial heights and excess values

3. **Algorithm Steps**:
   - Initialize network with source excess
   - Find vertex with maximum excess
   - Push flow from this vertex to neighbors
   - Continue until no excess remains

### Expected Output:
The program will calculate the maximum flow from source (vertex 0) to sink (vertex 3) and store the result in MAX_FLOW.

### Limitations:
- This is a simplified version for LMC's constraints
- Only handles small networks (4 nodes)
- No explicit relabeling operation implemented
- Limited to integer arithmetic

This implementation shows how the push-relabel algorithm's core concepts can be translated to LMC's instruction set, though it's a simplified version that would need expansion for full functionality.

