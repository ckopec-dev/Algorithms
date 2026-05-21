# CART Decision Tree Algorithm in LMC

Here's an implementation of a CART (Classification and Regression Trees) decision tree algorithm in LMC (Little Man Computer) assembly language:

```assembly
; CART Decision Tree Algorithm - LMC Implementation
; This program demonstrates a simplified CART algorithm for binary classification

; Data Section
INPUT_NUM    DAT 5        ; Number of features to test
THRESHOLD    DAT 0.5      ; Decision threshold
FEATURE1     DAT 1        ; Feature values
FEATURE2     DAT 0
FEATURE3     DAT 1
FEATURE4     DAT 0
FEATURE5     DAT 1
TARGET       DAT 1        ; Target value (1 for class A, 0 for class B)

; Main Program
START        LDA INPUT_NUM
             STA COUNT
             LDA FEATURE1
             STA FEATURE
             LDA THRESHOLD
             STA THRESHOLD_VAL
             BRZ TERMINATE
             LDA FEATURE
             SUB THRESHOLD_VAL
             BRZ DECISION
             BRZ LEFT_CHILD
             BRZ RIGHT_CHILD

; Decision Logic
DECISION     LDA FEATURE
             SUB THRESHOLD_VAL
             BRZ LEFT_CHILD
             BRZ RIGHT_CHILD
             BRA TERMINATE

LEFT_CHILD   LDA FEATURE2
             STA FEATURE
             LDA THRESHOLD
             STA THRESHOLD_VAL
             LDA FEATURE
             SUB THRESHOLD_VAL
             BRZ LEFT_CHILD
             BRZ RIGHT_CHILD

RIGHT_CHILD  LDA FEATURE3
             STA FEATURE
             LDA THRESHOLD
             STA THRESHOLD_VAL
             LDA FEATURE
             SUB THRESHOLD_VAL
             BRZ LEFT_CHILD
             BRZ RIGHT_CHILD

; Output Result
OUTPUT       LDA TARGET
             STA RESULT
             LDA RESULT
             OUT
             BRA TERMINATE

; Termination
TERMINATE    HLT
             DAT 0

; Working Storage
COUNT        DAT 0
FEATURE      DAT 0
THRESHOLD_VAL DAT 0
RESULT       DAT 0
```

## Algorithm Explanation

This LMC implementation demonstrates the core logic of a CART decision tree:

1. **Input Processing**: Reads feature values and threshold
2. **Decision Making**: Compares feature values against threshold
3. **Tree Traversal**: Follows left or right branches based on comparison
4. **Classification**: Outputs final class prediction

## Key Components

- **Feature Testing**: Each feature is tested against a threshold
- **Binary Split**: Creates two branches (left/right) based on comparison
- **Recursive Structure**: Tree traversal logic (simplified)
- **Output**: Final classification result

## Limitations

This is a simplified version that demonstrates the core concepts. A full CART implementation would include:
- Multiple feature selection
- Pruning algorithms
- Cross-validation
- More complex tree structures

The LMC implementation shows how decision tree logic can be broken down into basic operations that can be executed on a simple processor architecture.

