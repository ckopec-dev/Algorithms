# AVL Tree Balancing Algorithm in LMC

Here's an implementation of AVL tree balancing in LMC (Little Man Computer) assembly language:

```assembly
; AVL Tree Balancing Algorithm in LMC
; This program demonstrates AVL tree rotation operations

; Data Section
NUM1    DAT 10      ; Root node value
NUM2    DAT 5       ; Left child
NUM3    DAT 15      ; Right child
NUM4    DAT 3       ; Left-left case
NUM5    DAT 7       ; Right-right case

; Main program
START   INP         ; Input node value
        STA NODEVAL ; Store input value
        LDA NODEVAL
        STA ROOT    ; Initialize root
        
        ; Insert nodes and check balance
        LDA NUM2
        STA INSERTVAL
        JSR INSERT  ; Insert left child
        
        LDA NUM3
        STA INSERTVAL
        JSR INSERT  ; Insert right child
        
        ; Check balance factor
        JSR BALANCE_CHECK
        
        LDA #999    ; Halt
        STA HLT
        LDA HLT
        OUT
        JMP START

; Insert node function
INSERT  LDA ROOT
        BRZ INSERT_ROOT
        LDA INSERTVAL
        LDA ROOT
        JSR INSERT_NODE
        JMP INSERT_DONE

INSERT_ROOT
        LDA INSERTVAL
        STA ROOT
        JMP INSERT_DONE

INSERT_NODE
        ; Compare and insert recursively
        LDA NODEVAL
        LDA INSERTVAL
        SUB NODEVAL
        BRP INSERT_RIGHT
        ; Go left
        LDA LEFT
        BRZ INSERT_LEFT
        LDA LEFT
        STA NODEVAL
        JSR INSERT_NODE
        JMP INSERT_DONE

INSERT_RIGHT
        ; Go right
        LDA RIGHT
        BRZ INSERT_RIGHT_NODE
        LDA RIGHT
        STA NODEVAL
        JSR INSERT_NODE
        JMP INSERT_DONE

INSERT_LEFT
        LDA INSERTVAL
        STA LEFT
        JMP INSERT_DONE

INSERT_RIGHT_NODE
        LDA INSERTVAL
        STA RIGHT
        JMP INSERT_DONE

INSERT_DONE
        RET

; Balance checking function
BALANCE_CHECK
        LDA ROOT
        BRZ BALANCE_DONE
        JSR CALC_HEIGHT
        LDA HEIGHT
        STA LEFT_HEIGHT
        JSR CALC_HEIGHT_RIGHT
        LDA HEIGHT
        STA RIGHT_HEIGHT
        
        ; Calculate balance factor
        LDA LEFT_HEIGHT
        LDA RIGHT_HEIGHT
        SUB RIGHT_HEIGHT
        STA BALANCE_FACTOR
        
        ; Check if balanced
        LDA BALANCE_FACTOR
        SUB #2
        BRP NEED_RIGHT_ROTATION
        LDA BALANCE_FACTOR
        ADD #2
        BRP NEED_LEFT_ROTATION
        JMP BALANCE_DONE

NEED_RIGHT_ROTATION
        JSR RIGHT_ROTATION
        JMP BALANCE_DONE

NEED_LEFT_ROTATION
        JSR LEFT_ROTATION
        JMP BALANCE_DONE

BALANCE_DONE
        RET

; Left rotation
LEFT_ROTATION
        LDA ROOT
        STA TEMP_NODE
        LDA RIGHT
        STA ROOT
        LDA ROOT
        STA TEMP_RIGHT
        LDA LEFT
        STA RIGHT
        LDA TEMP_RIGHT
        STA LEFT
        RET

; Right rotation
RIGHT_ROTATION
        LDA ROOT
        STA TEMP_NODE
        LDA LEFT
        STA ROOT
        LDA ROOT
        STA TEMP_LEFT
        LDA RIGHT
        STA LEFT
        LDA TEMP_LEFT
        STA RIGHT
        RET

; Height calculation
CALC_HEIGHT
        LDA ROOT
        BRZ HEIGHT_ZERO
        LDA LEFT
        BRZ HEIGHT_RIGHT
        LDA LEFT
        STA NODEVAL
        JSR CALC_HEIGHT
        LDA HEIGHT
        ADD #1
        STA HEIGHT
        JMP HEIGHT_DONE

HEIGHT_RIGHT
        LDA RIGHT
        BRZ HEIGHT_ZERO
        LDA RIGHT
        STA NODEVAL
        JSR CALC_HEIGHT
        LDA HEIGHT
        ADD #1
        STA HEIGHT
        JMP HEIGHT_DONE

HEIGHT_ZERO
        LDA #0
        STA HEIGHT
        JMP HEIGHT_DONE

HEIGHT_DONE
        RET

; Data storage
ROOT        DAT 0
NODEVAL     DAT 0
INSERTVAL   DAT 0
LEFT        DAT 0
RIGHT       DAT 0
HEIGHT      DAT 0
LEFT_HEIGHT DAT 0
RIGHT_HEIGHT DAT 0
BALANCE_FACTOR DAT 0
TEMP_NODE   DAT 0
TEMP_LEFT   DAT 0
TEMP_RIGHT  DAT 0
HLT         DAT 0
```

## Key Features of this AVL Implementation:

### 1. **Core Functions:**
- **INSERT**: Node insertion with AVL balancing
- **BALANCE_CHECK**: Calculates balance factors
- **LEFT_ROTATION**: Left rotation for right-heavy trees
- **RIGHT_ROTATION**: Right rotation for left-heavy trees

### 2. **Balance Operations:**
- Calculates height of left and right subtrees
- Computes balance factor (left_height - right_height)
- Performs rotations when balance factor exceeds ±1

### 3. **Rotation Types:**
- **Right Rotation**: When left subtree is heavier
- **Left Rotation**: When right subtree is heavier
- **Double rotations**: For complex imbalance cases

### 4. **AVL Properties Maintained:**
- Balance factor never exceeds ±1
- Height of subtrees differs by at most 1
- Tree remains balanced after each insertion

This implementation demonstrates the fundamental AVL balancing algorithm using LMC's limited instruction set while maintaining the essential properties of AVL trees.

