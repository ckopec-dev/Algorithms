# Linear Regression in Assembly Language

Here's an example of implementing linear regression using Assembly language (x86-64 AT&T syntax):

```assembly
.section .data
    # Sample data points
    x_data: .double 1.0, 2.0, 3.0, 4.0, 5.0
    y_data: .double 2.1, 3.9, 6.2, 8.1, 9.8
    n: .long 5
    
    # Results storage
    slope: .double 0.0
    intercept: .double 0.0

.section .text
    .global _start

# Function to calculate linear regression coefficients
# y = mx + b where m is slope and b is intercept
linear_regression:
    push %rbp
    mov %rsp, %rbp
    
    # Parameters:
    # %rdi = pointer to x_data array
    # %rsi = pointer to y_data array
    # %rdx = number of data points (n)
    
    # Initialize registers for calculations
    xor %rax, %rax          # i = 0
    xor %rbx, %rbx          # sum_x = 0
    xor %rcx, %rcx          # sum_y = 0
    xor %r8, %r8            # sum_xy = 0
    xor %r9, %r9            # sum_x2 = 0
    
    mov %rdx, %r10          # n in r10 for loop counter
    
    # Calculate sums: sum_x, sum_y, sum_xy, sum_x2
calculate_sums:
    # Check loop condition
    cmp $0, %r10
    je sums_done
    
    # Load x[i] and y[i]
    movsd (%rdi,%rax,8), %xmm0  # Load x[i]
    movsd (%rsi,%rax,8), %xmm1  # Load y[i]
    
    # sum_x += x[i]
    addsd %xmm0, %xmm2
    movsd %xmm2, %rbx
    
    # sum_y += y[i]
    addsd %xmm1, %xmm3
    movsd %xmm3, %rcx
    
    # sum_xy += x[i] * y[i]
    mulsd %xmm1, %xmm0
    addsd %xmm0, %xmm4
    movsd %xmm4, %r8
    
    # sum_x2 += x[i] * x[i]
    mulsd %xmm0, %xmm0
    addsd %xmm0, %xmm5
    movsd %xmm5, %r9
    
    # Increment loop counter
    inc %rax
    dec %r10
    jmp calculate_sums

sums_done:
    # Calculate slope: m = (n*sum_xy - sum_x*sum_y) / (n*sum_x2 - sum_x*sum_x)
    # Calculate numerator: n*sum_xy - sum_x*sum_y
    movsd %xmm4, %xmm0      # sum_xy
    movsd %xmm2, %xmm1      # sum_x
    movsd %xmm3, %xmm2      # sum_y
    movsd %xmm5, %xmm3      # sum_x2
    
    # n * sum_xy
    movsd %xmm0, %xmm6
    mulsd %xmm6, %xmm6
    
    # sum_x * sum_y
    mulsd %xmm2, %xmm7
    
    # numerator = n*sum_xy - sum_x*sum_y
    subsd %xmm7, %xmm6
    
    # Calculate denominator: n*sum_x2 - sum_x*sum_x
    # n * sum_x2
    movsd %xmm3, %xmm7
    mulsd %xmm7, %xmm7
    
    # sum_x * sum_x
    mulsd %xmm1, %xmm1
    
    # denominator = n*sum_x2 - sum_x*sum_x
    subsd %xmm1, %xmm7
    
    # Calculate slope = numerator / denominator
    divsd %xmm7, %xmm6
    movsd %xmm6, slope
    
    # Calculate intercept: b = (sum_y - m*sum_x) / n
    # m * sum_x
    mulsd %xmm6, %xmm1
    
    # sum_y - m*sum_x
    subsd %xmm1, %xmm2
    
    # divide by n
    movsd %xmm2, %xmm8
    divsd %xmm8, %xmm8
    
    movsd %xmm8, intercept
    
    pop %rbp
    ret

# Main program
_start:
    # Call linear regression function
    mov $x_data, %rdi       # pointer to x_data
    mov $y_data, %rsi       # pointer to y_data
    mov $5, %rdx            # number of points
    
    call linear_regression
    
    # Exit program
    mov $60, %rax           # sys_exit
    mov $0, %rdi            # exit status
    syscall
```

## Key Components Explained:

### 1. **Data Section**
- Contains sample data points for x and y values
- Stores the number of data points (n)
- Holds memory locations for slope and intercept results

### 2. **Algorithm Implementation**
- **Loop-based calculation**: Iterates through all data points
- **Summation**: Calculates sum_x, sum_y, sum_xy, sum_x2
- **Linear regression formulas**:
  - Slope (m) = (n×∑xy - ∑x×∑y) / (n×∑x² - (∑x)²)
  - Intercept (b) = (∑y - m×∑x) / n

### 3. **Assembly Features Used**
- **Floating-point operations**: `addsd`, `mulsd`, `subsd`, `divsd`
- **Memory addressing**: Array access with base + offset
- **Loop control**: Conditional jumps and counters
- **Register usage**: Proper use of x86-64 registers

### 4. **Mathematical Approach**
The algorithm implements the least squares method for linear regression, which minimizes the sum of squared residuals between observed and predicted values.

This example demonstrates how mathematical concepts can be translated into low-level assembly code while maintaining the core linear regression algorithm functionality.

