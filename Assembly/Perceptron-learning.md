# Perceptron Learning Algorithm in Assembly

Below is an example implementation of the Perceptron learning algorithm in x86-64 Assembly language. This implementation demonstrates the core learning process for a single-layer perceptron.

```assembly
.section .data
    # Training data - 4 samples with 2 features each
    training_data:
        .double 1.0, 1.0    # Sample 1: x1=1.0, x2=1.0, y=1.0
        .double 1.0, -1.0   # Sample 2: x1=1.0, x2=-1.0, y=0.0
        .double -1.0, 1.0   # Sample 3: x1=-1.0, x2=1.0, y=0.0
        .double -1.0, -1.0  # Sample 4: x1=-1.0, x2=-1.0, y=0.0
    
    # Expected outputs
    expected_outputs:
        .double 1.0
        .double 0.0
        .double 0.0
        .double 0.0
    
    # Initial weights (3 weights including bias)
    weights: 
        .double 0.0, 0.0, 0.0
    
    # Learning rate
    learning_rate: .double 0.1
    
    # Number of training samples
    num_samples: .double 4.0
    
    # Maximum iterations
    max_iterations: .double 100.0
    
    # Print format strings
    newline: .ascii "\n"
    newline_len = . - newline
    
    result_msg: .ascii "Final weights: "
    result_msg_len = . - result_msg

.section .text
    .global _start

_start:
    # Initialize registers
    movsd   learning_rate(%rip), %xmm0      # Load learning rate
    movsd   weights(%rip), %xmm1            # Load weights (w0, w1, w2)
    movsd   num_samples(%rip), %xmm2        # Load number of samples
    movsd   max_iterations(%rip), %xmm3     # Load max iterations
    
    # Initialize iteration counter
    xor     %rax, %rax                      # iteration = 0
    
outer_loop:
    # Check if max iterations reached
    cmpsd   %xmm3, %xmm2, %xmm4
    jae     done_learning
    
    # Initialize error sum
    xorpd   %xmm5, %xmm5                    # error_sum = 0.0
    
    # Initialize sample counter
    xor     %rcx, %rcx                      # sample = 0
    
inner_loop:
    # Check if all samples processed
    cmp     %rcx, %rax                      # Compare sample with num_samples
    jge     update_weights
    
    # Calculate dot product: w0*x0 + w1*x1 + w2*x2
    # Load current sample (3 doubles: x0, x1, x2)
    movsd   training_data(%rip, %rcx, 8), %xmm6     # Load x0
    movsd   training_data+8(%rip, %rcx, 8), %xmm7   # Load x1
    movsd   training_data+16(%rip, %rcx, 8), %xmm8  # Load x2
    
    # Calculate prediction: w0*x0 + w1*x1 + w2*x2
    mulsd   %xmm6, %xmm1                    # w0 * x0
    movsd   %xmm1, %xmm9                    # Save partial result
    
    mulsd   %xmm7, %xmm1                    # w1 * x1
    addsd   %xmm9, %xmm1                    # w0*x0 + w1*x1
    
    mulsd   %xmm8, %xmm1                    # w2 * x2
    addsd   %xmm1, %xmm9                    # w0*x0 + w1*x1 + w2*x2
    
    # Apply activation function (step function)
    # Simple comparison: if prediction >= 0.5, output = 1, else 0
    movsd   $0.5, %xmm10
    cmpsd   %xmm10, %xmm9, %xmm11
    movsd   $1.0, %xmm12
    movsd   $0.0, %xmm13
    cmovae  %xmm12, %xmm11                  # If >= 0.5, output = 1.0
    cmovb   %xmm13, %xmm11                  # If < 0.5, output = 0.0
    
    # Get expected output
    movsd   expected_outputs(%rip, %rcx, 8), %xmm14
    
    # Calculate error: expected - prediction
    subsd   %xmm11, %xmm14                  # error = expected - output
    
    # Update weights: w = w + learning_rate * error * input
    movsd   %xmm14, %xmm15                  # Load error
    
    # Update w0
    mulsd   %xmm15, %xmm6                   # error * x0
    mulsd   learning_rate(%rip), %xmm6      # learning_rate * error * x0
    addsd   %xmm6, weights(%rip)            # w0 = w0 + update
    
    # Update w1
    mulsd   %xmm15, %xmm7                   # error * x1
    mulsd   learning_rate(%rip), %xmm7      # learning_rate * error * x1
    addsd   %xmm7, weights+8(%rip)          # w1 = w1 + update
    
    # Update w2 (bias)
    mulsd   %xmm15, %xmm8                   # error * x2 (bias = 1.0)
    mulsd   learning_rate(%rip), %xmm8      # learning_rate * error * x2
    addsd   %xmm8, weights+16(%rip)         # w2 = w2 + update
    
    # Increment sample counter
    inc     %rcx
    jmp     inner_loop

update_weights:
    # Increment iteration counter
    inc     %rax
    
    # Continue learning
    jmp     outer_loop

done_learning:
    # Print final weights
    mov     $1, %rax                        # sys_write
    mov     $1, %rdi                        # stdout
    mov     $result_msg, %rsi               # message
    mov     $result_msg_len, %rdx           # length
    syscall
    
    # Print weights (simplified - in practice would need more complex output)
    # This is a simplified version - full implementation would require
    # more complex output formatting
    
    # Exit program
    mov     $60, %rax                       # sys_exit
    mov     $0, %rdi                        # exit status
    syscall
```

## Key Components Explained:

### 1. **Data Section**
- `training_data`: Input samples with 2 features each
- `expected_outputs`: Target values for each sample
- `weights`: Initial weights (including bias term)
- `learning_rate`: Step size for weight updates

### 2. **Algorithm Flow**
1. **Outer Loop**: Iterates through maximum training iterations
2. **Inner Loop**: Processes each training sample
3. **Forward Pass**: Computes weighted sum and applies activation function
4. **Error Calculation**: Compares prediction with expected output
5. **Weight Update**: Adjusts weights using gradient descent

### 3. **Key Operations**
- **Dot Product**: Computes weighted sum of inputs
- **Activation Function**: Simple step function
- **Weight Update**: Uses gradient descent rule
- **Iteration Control**: Manages training loop termination

### 4. **Important Notes**
- This is a simplified implementation focusing on the core algorithm
- Real-world implementation would require more sophisticated output formatting
- Error handling and edge cases are omitted for clarity
- The algorithm assumes binary classification with step activation function

This implementation demonstrates how the fundamental perceptron learning rule (`w = w + η(y - ŷ)x`) is executed in assembly language, showing the low-level operations that make machine learning algorithms work.

