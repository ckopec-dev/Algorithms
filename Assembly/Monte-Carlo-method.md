# Monte Carlo Method in Assembly Language

Here's an example of a Monte Carlo simulation to estimate π using Assembly language (x86-64 AT&T syntax):

```assembly
.section .data
    .equ MAX_ITERATIONS, 1000000
    .equ SEED, 12345
    
    # Buffers for random numbers
    .align 8
    random_buffer:
        .quad 0
        .quad 0
        .quad 0
        .quad 0
    
    # Result storage
    pi_estimate:
        .double 0.0
    
    # Constants
    .align 8
    one_double:
        .double 1.0
    four_double:
        .double 4.0

.section .text
    .global _start

_start:
    # Initialize counter and hits
    movq $0, %rax          # counter (iterations)
    movq $0, %rbx          # hits inside circle
    movl $SEED, %ecx       # seed for random
    
    # Main Monte Carlo loop
loop_start:
    # Check if we've reached max iterations
    cmpq $MAX_ITERATIONS, %rax
    jge loop_end
    
    # Generate two random numbers between 0 and 1
    call generate_random_pair
    
    # Store random numbers in registers
    movsd %xmm0, %xmm2     # x coordinate
    movsd %xmm1, %xmm3     # y coordinate
    
    # Calculate x^2 + y^2
    movsd %xmm2, %xmm0     # copy x
    mulsd %xmm0, %xmm0     # x^2
    movsd %xmm3, %xmm1     # copy y
    mulsd %xmm1, %xmm1     # y^2
    addsd %xmm1, %xmm0     # x^2 + y^2
    
    # Compare with 1.0 (inside unit circle)
    movsd one_double(%rip), %xmm1
    cmpsd $0, %xmm1, %xmm0 # compare x^2 + y^2 with 1.0
    jbe hit_inside         # if <= 1, point is inside circle
    
    # Point is outside circle
    jmp continue_loop
    
hit_inside:
    # Increment hit counter
    incq %rbx
    
continue_loop:
    # Increment iteration counter
    incq %rax
    jmp loop_start

loop_end:
    # Calculate π estimate: π ≈ 4 * (hits / total)
    movq %rbx, %rax        # hits to rax
    movq %rax, %rbx        # copy to rbx for division
    
    # Convert to double for division
    cvtsi2sd %rax, %xmm0   # hits as double
    movsd four_double(%rip), %xmm1
    mulsd %xmm1, %xmm0     # 4 * hits
    
    # Divide by total iterations
    movq $MAX_ITERATIONS, %rax
    cvtsi2sd %rax, %xmm1   # total as double
    divsd %xmm1, %xmm0     # (4 * hits) / total
    
    # Store result
    movsd %xmm0, pi_estimate(%rip)
    
    # Exit program
    movq $60, %rax         # sys_exit
    movq $0, %rdi          # exit status
    syscall

# Function to generate two random numbers between 0 and 1
# Uses simple linear congruential generator
generate_random_pair:
    # Linear congruential generator: X(n+1) = (a * X(n) + c) mod m
    # Using: a = 1664525, c = 1013904223, m = 2^32
    movl %ecx, %eax        # seed to eax
    movl $1664525, %ebx    # multiplier
    imull %ebx, %eax       # eax = seed * 1664525
    addl $1013904223, %eax # add increment
    movl %eax, %ecx        # update seed
    
    # Convert to double between 0 and 1
    # First random number
    movl %eax, %ebx
    shrl $16, %ebx         # get upper 16 bits
    movl %ebx, %eax
    cvtsi2sd %eax, %xmm0   # convert to double
    
    # Second random number
    movl %ecx, %ebx
    shrl $16, %ebx         # get upper 16 bits
    movl %ebx, %eax
    cvtsi2sd %eax, %xmm1   # convert to double
    
    # Normalize to [0,1)
    movsd one_double(%rip), %xmm2
    divsd %xmm2, %xmm0     # normalize first number
    divsd %xmm2, %xmm1     # normalize second number
    
    ret
```

## How This Monte Carlo Simulation Works:

1. **Algorithm Overview**: 
   - Generate random points (x,y) in the unit square [0,1] × [0,1]
   - Count how many points fall inside the unit circle (x² + y² ≤ 1)
   - Estimate π using: π ≈ 4 × (points inside circle / total points)

2. **Key Components**:
   - Random number generation using LCG (Linear Congruential Generator)
   - Point testing using the circle equation
   - Accumulation of hits and final π calculation
   - Assembly instructions for arithmetic and control flow

3. **Assembly Features Used**:
   - Floating-point operations (`movsd`, `mulsd`, `divsd`)
   - Integer arithmetic and conversion (`cvtsi2sd`)
   - Conditional jumps (`jge`, `jbe`)
   - Memory addressing and data storage
   - System calls for program termination

This example demonstrates how to implement a classic Monte Carlo method in low-level assembly language, showing the fundamental principles of numerical simulation at the machine level.

