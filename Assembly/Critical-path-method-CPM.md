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
    dec %rcx                # Decrement by 1 (n-1 passes)
    
outer_loop:
    test %rcx, %rcx         # Check if counter is zero
    jz sort_done            # If zero, sorting is complete
    
    mov %rsi, %r8           # Inner loop counter = array_size
    dec %r8                 # Decrement by 1 (n-1 comparisons)
    
inner_loop:
    test %r8, %r8           # Check if inner counter is zero
    jz inner_loop_done      # If zero, go to next outer iteration
    
    # Compare adjacent elements
    mov (%rdi,%r8,4), %eax  # Load array[i]
    mov -4(%rdi,%r8,4), %ebx # Load array[i-1]
    
    cmp %eax, %ebx          # Compare array[i-1] and array[i]
    jle no_swap             # If array[i-1] <= array[i], no swap needed
    
    # Swap elements
    mov %eax, -4(%rdi,%r8,4) # array[i-1] = array[i]
    mov %ebx, (%rdi,%r8,4)   # array[i] = array[i-1]
    
no_swap:
    dec %r8                 # Decrement inner counter
    jmp inner_loop          # Continue inner loop
    
inner_loop_done:
    dec %rcx                # Decrement outer counter
    jmp outer_loop          # Continue outer loop
    
sort_done:
    pop %rbp
    ret

# Main program
_start:
    # Call bubble sort
    mov $array, %rdi        # Load array address
    mov array_size, %rsi    # Load array size
    call bubble_sort
    
    # Exit program
    mov $60, %rax           # sys_exit
    mov $0, %rdi            # exit status
    syscall
```

## Algorithm Explanation

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array (n-1 passes)
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Swapping**: Uses memory operations to exchange element values
5. **Termination**: Loops until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Register usage**: `%rax`, `%rbx`, `%rcx`, `%rdi`, `%rsi`, `%r8` for various purposes
- **Memory addressing**: Using base+offset addressing (`(%rdi,%r8,4)`)
- **Control flow**: `jmp`, `jz`, `jle` conditional jumps
- **Function calls**: `call` and `ret` instructions
- **System calls**: `syscall` for program termination

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

