# Assembly Algorithm Example: Sum of Array Elements

Here's an example of an Assembly algorithm that calculates the sum of elements in an array:

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
    
loop_start:
    cmp     $0, %rax            # Compare size with 0
    je      loop_end            # Jump if size is 0
    
    add     (%rcx), %r8         # Add current array element to sum
    add     $4, %rcx            # Move to next element (4 bytes)
    dec     %rax                # Decrement counter
    jmp     loop_start          # Jump back to loop start
    
loop_end:
    mov     %r8, %rax           # Move result to return register
    pop     %rbp                # Restore base pointer
    ret                         # Return

# Main program
_start:
    mov     $array, %rdi        # Load array address
    mov     $array_size, %rsi   # Load array size
    call    _sum_array          # Call sum function
    
    # Exit program
    mov     $60, %rax           # sys_exit
    mov     $0, %rdi            # exit status
    syscall
```

## Algorithm Explanation

This assembly program demonstrates:
1. **Array traversal** using a loop
2. **Memory access** to read array elements
3. **Arithmetic operations** to accumulate sum
4. **Control flow** with conditional jumps
5. **Function calling convention** with parameters and return values

## Key Assembly Concepts Shown

- **Registers**: `%rax`, `%rbx`, `%rcx`, `%rdx`, `%rsi`, `%rdi`, `%r8`
- **Memory addressing**: `(%rcx)` for dereferencing
- **Control flow**: `cmp`, `je`, `jmp`, `dec`
- **Function prologue/epilogue**: `push`/`pop` `%rbp`
- **System calls**: `syscall` for program termination

## Expected Output
The program calculates the sum: 10 + 20 + 30 + 40 + 50 = 150

