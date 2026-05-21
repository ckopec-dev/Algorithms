# Backpropagation Algorithm in LMC (Little Man Computer)

Here's an example implementation of a simple backpropagation algorithm using LMC instructions. This demonstrates the core concepts of neural network training in the LMC architecture.

```assembly
; Backpropagation Algorithm - LMC Implementation
; Simple 2-layer neural network with 2 inputs, 1 output

    INP         ; Input 1 (x1)
    STA INPUT1
    INP         ; Input 2 (x2)
    STA INPUT2
    INP         ; Target output (t)
    STA TARGET

; Initialize weights and biases
    LDA W11     ; Load initial weight w11
    STA WEIGHT11
    LDA W12     ; Load initial weight w12
    STA WEIGHT12
    LDA B1      ; Load bias b1
    STA BIAS1

; Forward Pass
    LDA INPUT1  ; Load x1
    MUL WEIGHT11 ; x1 * w11
    STA TEMP1
    LDA INPUT2  ; Load x2
    MUL WEIGHT12 ; x2 * w12
    ADD TEMP1   ; (x1*w11) + (x2*w12)
    ADD BIAS1   ; + b1
    STA NET1    ; Store net input to hidden unit

; Apply activation function (sigmoid)
    LDA NET1
    STA TEMP2
    LDA ONE     ; Load 1.0
    ADD TEMP2   ; 1 + net1
    STA TEMP3
    LDA ONE     ; Load 1.0
    DIV TEMP3   ; 1/(1+net1) = sigmoid(net1)
    STA OUTPUT  ; Store activation

; Calculate error
    LDA TARGET  ; Load target
    SUB OUTPUT  ; target - output
    STA ERROR   ; Store error

; Backward Pass - Calculate gradients
    LDA OUTPUT  ; Load output
    LDA ONE     ; Load 1.0
    SUB OUTPUT  ; 1 - output
    MUL OUTPUT  ; output * (1-output) = derivative
    STA DERIV   ; Store derivative

    LDA ERROR   ; Load error
    MUL DERIV   ; error * derivative
    STA GRADIENT ; Store gradient

; Update weights (simplified version)
    LDA WEIGHT11
    LDA GRADIENT
    MUL LEARNING_RATE ; gradient * learning_rate
    SUB WEIGHT11      ; w11 - learning_rate * gradient
    STA WEIGHT11      ; Store updated weight

    LDA WEIGHT12
    LDA GRADIENT
    MUL LEARNING_RATE ; gradient * learning_rate
    SUB WEIGHT12      ; w12 - learning_rate * gradient
    STA WEIGHT12      ; Store updated weight

    LDA BIAS1
    LDA GRADIENT
    MUL LEARNING_RATE ; gradient * learning_rate
    SUB BIAS1         ; b1 - learning_rate * gradient
    STA BIAS1         ; Store updated bias

    LDA WEIGHT11
    OUT
    LDA WEIGHT12
    OUT
    LDA BIAS1
    OUT
    HLT

; Data Section
INPUT1   DAT 0
INPUT2   DAT 0
TARGET   DAT 0
OUTPUT   DAT 0
ERROR    DAT 0
NET1     DAT 0
DERIV    DAT 0
GRADIENT DAT 0
TEMP1    DAT 0
TEMP2    DAT 0
TEMP3    DAT 0

; Constants
W11      DAT 0.5    ; Initial weight w11
W12      DAT 0.3    ; Initial weight w12
B1       DAT 0.1    ; Initial bias b1
LEARNING_RATE DAT 0.1 ; Learning rate
ONE      DAT 1

; End of program
```

## Key Components Explained:

### 1. **Forward Pass**
- Loads inputs and weights
- Computes net input to hidden unit
- Applies sigmoid activation function

### 2. **Error Calculation**
- Computes difference between target and output
- Calculates derivative of activation function

### 3. **Backward Pass**
- Computes gradients using chain rule
- Updates weights and biases using gradient descent

### 4. **Weight Updates**
- Applies learning rate to gradient
- Updates weights and biases

## LMC Instructions Used:
- `INP` - Input data
- `STA` - Store accumulator
- `LDA` - Load accumulator
- `ADD` - Addition
- `SUB` - Subtraction
- `MUL` - Multiplication
- `DIV` - Division
- `OUT` - Output result
- `HLT` - Halt program

This implementation demonstrates the fundamental principles of backpropagation within the constraints of LMC's simple instruction set architecture.

