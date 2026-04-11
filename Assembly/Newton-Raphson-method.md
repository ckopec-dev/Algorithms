# Newton-Raphson Method in Assembly Language

Here's an example implementation of the Newton-Raphson method in x86-64 Assembly language to find the square root of a number.

```assembly
.section .data
    # Input value for which we want to find square root
    input_value: .double 25.0
    tolerance:   .double 1.0e-10
    max_iterations: .long 100
    
    # String constants for output
    result_msg: .ascii "Square root of 25.0 = "
    result_len = . - result_msg

.section .text
    .global _start

# Function to compute f(x) = x^2 - input_value
# Returns double precision result in xmm0
compute_function:
    movsd   input_value(%rip), %xmm1    # Load input value
    mulsd   %xmm0, %xmm0                # x^2
    subsd   %xmm1, %xmm0                # x^2 - input_value
    ret

# Function to compute f'(x) = 2*x
# Returns double precision result in xmm0
compute_derivative:
    addsd   %xmm0, %xmm0                # 2*x
    ret

# Newton-Raphson method implementation
newton_raphson:
    push    %rbp
    mov     %rsp, %rbp
    sub     $32, %rsp                   # Allocate stack space
    
    # Initialize x0 = input_value / 2.0
    movsd   input_value(%rip), %xmm0
    movsd   %xmm0, -8(%rbp)             # Store x0
    movsd   input_value(%rip), %xmm0
    divsd   $2.0, %xmm0
    movsd   %xmm0, -16(%rbp)            # Store initial guess
    
    # Initialize iteration counter
    movl    $0, -20(%rbp)
    
loop_start:
    # Check if we've exceeded max iterations
    movl    -20(%rbp), %eax
    cmpl    max_iterations(%rip), %eax
    jge     done
    
    # Load current x value
    movsd   -16(%rbp), %xmm0
    
    # Compute f(x) and f'(x)
    call    compute_function
    movsd   %xmm0, -24(%rbp)            # Store f(x)
    
    movsd   -16(%rbp), %xmm0
    call    compute_derivative
    movsd   %xmm0, -32(%rbp)            # Store f'(x)
    
    # Check if f'(x) is zero (avoid division by zero)
    movsd   -32(%rbp), %xmm0
    cmpsd   $0.0, %xmm0, %xmm0
    movmskpd %xmm0, %eax
    test    %eax, %eax
    jnz     done                        # If f'(x) = 0, we're done
    
    # Compute x_new = x - f(x)/f'(x)
    movsd   -24(%rbp), %xmm0            # Load f(x)
    movsd   -32(%rbp), %xmm1            # Load f'(x)
    divsd   %xmm1, %xmm0                # f(x)/f'(x)
    movsd   -16(%rbp), %xmm1            # Load x
    subsd   %xmm0, %xmm1                # x - f(x)/f'(x)
    movsd   %xmm1, -16(%rbp)            # Store new x
    
    # Check convergence: |x_new - x_old| < tolerance
    movsd   -16(%rbp), %xmm0            # New x
    movsd   -8(%rbp), %xmm1             # Old x
    subsd   %xmm1, %xmm0                # x_new - x_old
    movsd   %xmm0, %xmm1
    andpd   $0x7FFFFFFFFFFFFFFF, %xmm1  # Absolute value
    movsd   tolerance(%rip), %xmm0
    cmpsd   %xmm0, %xmm1, %xmm0
    movmskpd %xmm0, %eax
    test    %eax, %eax
    jnz     done                        # If converged, we're done
    
    # Update x_old
    movsd   -16(%rbp), %xmm0
    movsd   %xmm0, -8(%rbp)
    
    # Increment iteration counter
    incl    -20(%rbp)
    jmp     loop_start
    
done:
    # Return final result in xmm0
    movsd   -16(%rbp), %xmm0
    
    add     $32, %rsp
    pop     %rbp
    ret

# Main program
_start:
    # Call Newton-Raphson method
    call    newton_raphson
    
    # Exit program
    mov     $60, %rax                   # sys_exit
    mov     $0, %rdi                    # exit status
    syscall
```

## Key Components Explained:

### 1. **Data Section**
- `input_value`: The number we want to find the square root of
- `tolerance`: Convergence threshold
- `max_iterations`: Maximum iterations to prevent infinite loops

### 2. **Function Implementations**
- `compute_function`: Computes f(x) = x² - input_value
- `compute_derivative`: Computes f'(x) = 2x

### 3. **Main Algorithm Logic**
- Initialize x₀ = input_value / 2
- Iteratively apply Newton-Raphson formula: x_{n+1} = x_n - f(x_n)/f'(x_n)
- Check convergence condition
- Handle edge cases (division by zero, max iterations)

### 4. **Key Assembly Features Used**
- **SSE2 instructions**: `movsd`, `mulsd`, `divsd`, `subsd` for double precision arithmetic
- **Stack management**: Proper function call/return with stack frame setup
- **Conditional jumps**: `jge`, `jnz`, `jmp` for control flow
- **Bit manipulation**: `andpd` for absolute value calculation

## How It Works:
1. Start with an initial guess (input/2)
2. Apply Newton-Raphson iteration formula
3. Continue until convergence or maximum iterations reached
4. Return the computed square root

This implementation demonstrates how to perform numerical methods in assembly language, handling floating-point operations and iterative algorithms at the machine level.

