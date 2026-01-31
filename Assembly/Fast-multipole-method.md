# Assembly Algorithm Example: Sum of Array Elements

Here's an example of an assembly algorithm that calculates the sum of elements in an array:

## x86-64 Assembly Implementation

```assembly
.section .data
    array:      .long 10, 20, 30, 40, 50    # Array of 5 integers
    array_size: .long 5                     # Size of array

.section .text
    .global _start

_sum_array:
    push    %rbp                # Save base pointer
    mov     %rsp, %rbp          # Set up base pointer
    
    mov     %rdi, %rcx          # Load array address into rcx
    mov     %rsi, %rax          # Load array size into rax
    xor     %r8, %r8            # Clear r8 (sum accumulator)
    
    # Loop through array elements
    mov     $0, %r9             # Initialize counter
    jmp     check_loop
    
loop_body:
    mov     (%rcx,%r9,4), %r10  # Load array[i] into r10
    add     %r10, %r8           # Add to sum
    inc     %r9                 # Increment counter
    
check_loop:
    cmp     %rax, %r9           # Compare counter with array size
    jl      loop_body           # Jump if less than size
    
    mov     %r8, %rax           # Move sum to return register
    pop     %rbp                # Restore base pointer
    ret                         # Return

# Main program
_start:
    lea     array(%rip), %rdi   # Load array address
    mov     array_size(%rip), %rsi # Load array size
    call    _sum_array          # Call sum function
    
    # Exit program
    mov     $60, %rax           # sys_exit
    mov     $0, %rdi            # exit status
    syscall
```

## Algorithm Explanation

This assembly program demonstrates:
1. **Array traversal** using indexed addressing
2. **Loop control** with conditional jumps
3. **Register usage** for efficient computation
4. **Function calling convention** with parameter passing
5. **Memory management** and stack operations

## Key Assembly Concepts Shown

- **Data section**: Defines array and size variables
- **Text section**: Contains executable instructions
- **Register operations**: Using `%rax`, `%rcx`, `%r8`, etc.
- **Memory addressing**: `(%rcx,%r9,4)` for array indexing
- **Control flow**: `jmp`, `jl`, `cmp` instructions
- **Function calls**: `call` and `ret` instructions

## Expected Output
The program calculates the sum: 10 + 20 + 30 + 40 + 50 = 150

This example shows how assembly language provides direct control over hardware while implementing high-level algorithmic concepts.

