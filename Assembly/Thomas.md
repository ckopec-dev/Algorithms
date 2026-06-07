# Thomas Algorithm in Assembly Language

The Thomas algorithm is a simplified form of Gaussian elimination for tridiagonal systems. Here's an implementation in x86-64 Assembly:

```assembly
.section .text
.global thomas_algorithm

# void thomas_algorithm(double *a, double *b, double *c, double *d, double *x, int n)
# Parameters:
#   %rdi = a (lower diagonal)
#   %rsi = b (main diagonal) 
#   %rdx = c (upper diagonal)
#   %rcx = d (right-hand side)
#   %r8  = x (solution vector)
#   %r9  = n (size of system)

thomas_algorithm:
    push    %rbp
    mov     %rsp, %rbp
    push    %rbx
    push    %r12
    push    %r13
    push    %r14
    push    %r15

    # Initialize loop counters and pointers
    mov     $1, %rax           # i = 1
    mov     %r9, %r10          # n = n
    dec     %r10               # n-1

    # Forward elimination phase
forward_elimination:
    # Check if we've reached the last row
    cmp     %r10, %rax
    jge     backward_substitution

    # Calculate m_i = a[i] / b[i-1]
    mov     %rax, %r11         # i
    dec     %r11               # i-1
    movsd   (%rdi,%r11,8), %xmm0  # a[i-1]
    movsd   (%rsi,%r11,8), %xmm1  # b[i-1]
    divsd   %xmm1, %xmm0       # m_i = a[i-1] / b[i-1]

    # Store m_i in temporary location (we'll overwrite a)
    movsd   %xmm0, (%rdi,%r11,8)  # Store m_i in a[i-1]

    # Update b[i] = b[i] - m_i * c[i-1]
    movsd   (%rsi,%r11,8), %xmm2  # b[i-1]
    movsd   (%rdx,%r11,8), %xmm3  # c[i-1]
    mulsd   %xmm3, %xmm0       # m_i * c[i-1]
    subsd   %xmm0, %xmm2       # b[i] = b[i] - m_i * c[i-1]
    movsd   %xmm2, (%rsi,%r11,8)  # Store updated b[i]

    # Update d[i] = d[i] - m_i * d[i-1]
    movsd   (%rcx,%r11,8), %xmm4  # d[i-1]
    mulsd   %xmm0, %xmm4       # m_i * d[i-1]
    subsd   %xmm4, %xmm2       # d[i] = d[i] - m_i * d[i-1]
    movsd   %xmm2, (%rcx,%r11,8)  # Store updated d[i]

    inc     %rax
    jmp     forward_elimination

backward_substitution:
    # Backward substitution phase
    mov     %r9, %rax          # i = n
    dec     %rax               # i = n-1
    dec     %rax               # i = n-2

backward_loop:
    cmp     $0, %rax
    jl      done

    # x[i] = (d[i] - c[i] * x[i+1]) / b[i]
    movsd   (%rcx,%rax,8), %xmm0  # d[i]
    movsd   (%rdx,%rax,8), %xmm1  # c[i]
    movsd   (%r8,%r11,8), %xmm2  # x[i+1] (need to adjust)
    
    # Calculate c[i] * x[i+1]
    mulsd   %xmm2, %xmm1
    subsd   %xmm1, %xmm0       # d[i] - c[i] * x[i+1]
    
    # Divide by b[i]
    movsd   (%rsi,%rax,8), %xmm2  # b[i]
    divsd   %xmm2, %xmm0
    
    movsd   %xmm0, (%r8,%rax,8)  # Store x[i]

    dec     %rax
    jmp     backward_loop

done:
    # Handle last element
    movsd   (%rcx,%rax,8), %xmm0  # d[0]
    movsd   (%rsi,%rax,8), %xmm1  # b[0]
    divsd   %xmm1, %xmm0
    movsd   %xmm0, (%r8,%rax,8)   # Store x[0]

    # Restore registers
    pop     %r15
    pop     %r14
    pop     %r13
    pop     %r12
    pop     %rbx
    pop     %rbp
    ret
```

## Example Usage

```assembly
# Example of calling the Thomas algorithm
.section .data
    # Tridiagonal system:
    # 2*x[0] + 1*x[1] = 3
    # 1*x[0] + 3*x[1] + 1*x[2] = 4
    # 1*x[1] + 2*x[2] = 5
    
    a: .quad 0, 1, 1    # Lower diagonal (a[0] = 0, a[1] = 1, a[2] = 1)
    b: .quad 2, 3, 2    # Main diagonal
    c: .quad 1, 1, 0    # Upper diagonal (c[2] = 0)
    d: .quad 3, 4, 5    # Right-hand side
    x: .quad 0, 0, 0    # Solution vector

.section .text
    call    thomas_algorithm

    # After call, x contains the solution
```

## Key Features

1. **Memory Management**: Uses registers efficiently to minimize memory access
2. **Forward Elimination**: Computes modified coefficients
3. **Backward Substitution**: Solves for unknowns starting from the last equation
4. **Register Usage**: 
   - `%rax`, `%rbx`, `%r12`-%r15 for general purpose
   - `%xmm` registers for double-precision floating-point operations
5. **Loop Control**: Proper loop termination and counter management

## Time Complexity
- **Forward Elimination**: O(n)
- **Backward Substitution**: O(n)
- **Overall**: O(n) - much faster than general Gaussian elimination O(n³)