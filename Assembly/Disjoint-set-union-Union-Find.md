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
    # %rdi = array pointer
    # %rsi = array size
    
    mov %rsi, %rcx          # Loop counter (outer loop)
    mov %rsi, %r8           # Temporary counter for inner loop
    
outer_loop:
    cmp $1, %rcx            # If size <= 1, done
    jle end_sort
    
    mov $0, %r9             # Inner loop counter
    
inner_loop:
    cmp %r9, %r8            # If inner counter >= outer counter, break
    jge end_inner
    
    # Compare adjacent elements
    mov (%rdi,%r9,4), %eax  # Load array[i]
    mov 4(%rdi,%r9,4), %ebx # Load array[i+1]
    
    cmp %ebx, %eax          # Compare array[i] and array[i+1]
    jle no_swap             # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    mov %ebx, (%rdi,%r9,4)  # array[i] = array[i+1]
    mov %eax, 4(%rdi,%r9,4) # array[i+1] = array[i]
    
no_swap:
    inc %r9                 # i++
    dec %r8                 # decrement inner counter
    jmp inner_loop
    
end_inner:
    dec %rcx                # decrement outer counter
    mov %rcx, %r8           # reset inner counter
    jmp outer_loop
    
end_sort:
    pop %rbp
    ret

# Main program
_start:
    # Call bubble sort
    lea array(%rip), %rdi   # Load array address
    mov array_size(%rip), %rsi # Load array size
    call bubble_sort
    
    # Exit program
    mov $60, %rax           # sys_exit
    mov $0, %rdi            # exit status
    syscall
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Swapping**: Direct memory manipulation to exchange values
5. **Termination**: Algorithm stops when no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Register usage**: `%rax`, `%rbx`, `%rcx`, `%rdi`, `%rsi`, `%r9`, `%r8`
- **Memory addressing**: `(%rdi,%r9,4)` for array access with scaling
- **Control flow**: `cmp`, `jle`, `jmp` instructions for loops and conditions
- **Function calls**: Stack management with `push`/`pop` and `call`/`ret`
- **System calls**: `syscall` for program termination

The algorithm has O(n²) time complexity, which is typical for bubble sort.

