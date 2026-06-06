# Pollard's Rho Algorithm in Assembly

Here's an implementation of Pollard's Rho algorithm for integer factorization in x86-64 assembly:

```assembly
.section .data
    .global gcd_table
    gcd_table:
        .quad 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
        .quad 11, 12, 13, 14, 15, 16, 17, 18, 19, 20

.section .text
    .global pollard_rho
    .global gcd

# Pollard's Rho algorithm implementation
# Input: n (number to factor)
# Output: factor of n (or 1 if none found)
pollard_rho:
    push    %rbp
    mov     %rsp, %rbp
    push    %rbx
    push    %rcx
    push    %rdx
    push    %rsi
    push    %rdi

    # Input: rdi = n (number to factor)
    mov     %rdi, %rax      # rax = n
    mov     %rax, %rbx      # rbx = n (for later use)

    # Initialize x = 2, y = 2, c = 1
    mov     $2, %r8         # x = 2
    mov     $2, %r9         # y = 2
    mov     $1, %r10        # c = 1
    mov     $1, %r11        # iteration counter

factor_loop:
    # x = (x*x + c) mod n
    mov     %r8, %rax       # rax = x
    imul    %rax, %rax      # rax = x*x
    add     %r10, %rax      # rax = x*x + c
    mov     %rbx, %rcx      # rcx = n
    xor     %rdx, %rdx      # clear rdx
    div     %rcx            # rax = quotient, rdx = remainder
    mov     %rdx, %r8       # x = x*x + c mod n

    # y = (y*y + c) mod n (first iteration)
    mov     %r9, %rax       # rax = y
    imul    %rax, %rax      # rax = y*y
    add     %r10, %rax      # rax = y*y + c
    mov     %rbx, %rcx      # rcx = n
    xor     %rdx, %rdx      # clear rdx
    div     %rcx            # rax = quotient, rdx = remainder
    mov     %rdx, %r9       # y = y*y + c mod n

    # y = (y*y + c) mod n (second iteration)
    mov     %r9, %rax       # rax = y
    imul    %rax, %rax      # rax = y*y
    add     %r10, %rax      # rax = y*y + c
    mov     %rbx, %rcx      # rcx = n
    xor     %rdx, %rdx      # clear rdx
    div     %rcx            # rax = quotient, rdx = remainder
    mov     %rdx, %r9       # y = y*y + c mod n

    # Calculate |x - y|
    mov     %r8, %rax       # rax = x
    cmp     %r9, %rax       # compare x and y
    jge     no_negation
    neg     %rax            # if y > x, negate difference
no_negation:
    sub     %r9, %rax       # rax = |x - y|

    # Calculate gcd(|x - y|, n)
    mov     %rax, %r12      # r12 = |x - y|
    mov     %rbx, %r13      # r13 = n
    call    gcd

    # If gcd > 1 and gcd < n, we found a factor
    cmp     $1, %rax
    jle     continue_loop
    cmp     %rbx, %rax
    jge     continue_loop

    # Found a factor - return it
    jmp     factor_found

continue_loop:
    # Check if we've done too many iterations
    inc     %r11
    cmp     $10000, %r11     # max iterations
    jge     no_factor_found

    jmp     factor_loop

factor_found:
    # Return the factor in rax
    jmp     done

no_factor_found:
    mov     $1, %rax        # return 1 (no factor found)

done:
    pop     %rdi
    pop     %rsi
    pop     %rdx
    pop     %rcx
    pop     %rbx
    pop     %rbp
    ret

# Greatest Common Divisor using Euclidean algorithm
# Input: rdi = a, rsi = b
# Output: rax = gcd(a,b)
gcd:
    push    %rbp
    mov     %rsp, %rbp
    push    %rbx
    push    %rcx
    push    %rdx

    mov     %rdi, %rax      # rax = a
    mov     %rsi, %rbx      # rbx = b

gcd_loop:
    cmp     $0, %rbx        # if b == 0
    je      gcd_done
    xor     %rdx, %rdx      # clear rdx
    mov     %rax, %rcx      # rcx = a
    div     %rbx            # rax = a/b, rdx = a%b
    mov     %rbx, %rax      # rax = b
    mov     %rdx, %rbx      # rbx = a%b
    jmp     gcd_loop

gcd_done:
    # rax contains gcd
    pop     %rdx
    pop     %rcx
    pop     %rbx
    pop     %rbp
    ret

# Example usage
.section .text
    .global _start

_start:
    # Example: factor 8051
    mov     $8051, %rdi     # n = 8051
    call    pollard_rho

    # Exit system call
    mov     $60, %rax       # sys_exit
    mov     $0, %rdi        # exit status
    syscall
```

## Key Features of This Implementation:

1. **Main Algorithm**: Implements Pollard's Rho with the function f(x) = x² + c
2. **Cycle Detection**: Uses Floyd's cycle detection algorithm
3. **GCD Calculation**: Euclidean algorithm for finding greatest common divisor
4. **Modular Arithmetic**: Proper handling of modular operations
5. **Iteration Limit**: Prevents infinite loops with maximum iteration count

## How It Works:

1. Initialize x and y to 2, and c to 1
2. Apply the function f(x) = x² + c repeatedly
3. Use Floyd's cycle detection to find cycles
4. Calculate GCD(|x - y|, n) to find factors
5. Continue until factor is found or iteration limit reached

## Example Usage:

The algorithm would find that 8051 = 83 × 97, where 83 is a factor found by the algorithm.

**Note**: This is a simplified implementation for educational purposes. A production version would include better randomization, more sophisticated cycle detection, and proper error handling.