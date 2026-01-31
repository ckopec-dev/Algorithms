# Assembly Algorithm Example: Bubble Sort

Here's an example of a bubble sort algorithm implemented in x86-64 Assembly language:

```assembly
.section .data
    array: .long 64, 34, 25, 12, 22, 11, 90
    array_size: .long 7

.section .text
    .global _start

bubble_sort:
    push %rbp
    mov %rsp, %rbp
    
    # Parameters:
    # %rdi = array address
    # %rsi = array size
    
    mov %rsi, %rcx          # Loop counter (outer loop)
    mov $0, %r8             # i = 0
    
outer_loop:
    cmp $0, %rcx
    je end_sort
    
    mov $0, %r9             # j = 0
    mov %rcx, %r10          # temp = size
    dec %r10                # temp = size - 1
    
inner_loop:
    cmp %r9, %r10
    jge inner_end
    
    # Compare array[j] and array[j+1]
    mov (%rdi,%r9,4), %eax      # Load array[j]
    mov 4(%rdi,%r9,4), %ebx     # Load array[j+1]
    
    cmp %ebx, %eax
    jle no_swap
    
    # Swap elements
    mov %ebx, (%rdi,%r9,4)      # array[j] = array[j+1]
    mov %eax, 4(%rdi,%r9,4)     # array[j+1] = array[j]
    
no_swap:
    inc %r9                     # j++
    jmp inner_loop
    
inner_end:
    dec %rcx                    # i--
    jmp outer_loop
    
end_sort:
    pop %rbp
    ret

# Main program
_start:
    # Call bubble_sort with array and size
    lea array(%rip), %rdi       # Load array address
    mov array_size(%rip), %rsi  # Load array size
    
    call bubble_sort
    
    # Exit program
    mov $60, %rax               # sys_exit
    mov $0, %rdi                # exit status
    syscall
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Swapping**: Uses memory operations to exchange element values
5. **Loop Control**: Uses registers to manage loop counters and conditions

## Key Assembly Concepts Demonstrated:

- **Register usage**: `%rax`, `%rbx`, `%rcx`, `%rdi`, `%rsi`, etc.
- **Memory addressing**: `(%rdi,%r9,4)` for array access with scaling
- **Conditional jumps**: `cmp`, `jle`, `jge`, `je`
- **Function calls**: `call`, `ret`, stack management
- **System calls**: `syscall` for program termination

The algorithm sorts the array `[64, 34, 25, 12, 22, 11, 90]` into ascending order `[11, 12, 22, 25, 34, 64, 90]`.

