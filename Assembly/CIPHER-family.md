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
    dec %rcx                # Decrement by 1 since we compare pairs
    
outer_loop:
    test %rcx, %rcx         # Check if counter is zero
    jz sorted               # If zero, we're done
    
    mov %rcx, %r8           # Save outer loop counter
    mov %rsi, %rcx          # Reset inner loop counter
    
inner_loop:
    dec %rcx                # Decrement inner counter
    jz inner_done           # If zero, exit inner loop
    
    # Compare adjacent elements
    mov (%rdi,%rcx,4), %eax # Load array[i]
    mov -4(%rdi,%rcx,4), %ebx # Load array[i-1]
    
    cmp %eax, %ebx          # Compare array[i-1] and array[i]
    jle no_swap             # If array[i-1] <= array[i], no swap needed
    
    # Swap elements
    mov %eax, -4(%rdi,%rcx,4) # array[i-1] = array[i]
    mov %ebx, (%rdi,%rcx,4)   # array[i] = array[i-1]
    
no_swap:
    jmp inner_loop          # Continue inner loop
    
inner_done:
    dec %r8                 # Decrement outer loop counter
    mov %r8, %rcx           # Restore outer loop counter
    jmp outer_loop          # Continue outer loop
    
sorted:
    pop %rbp
    ret

# Main program
_start:
    # Call bubble_sort with array and size
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
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Swapping**: Uses memory operations to exchange element values
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Register usage** for parameters and counters
- **Memory addressing** with base + index + scale
- **Conditional jumps** for loop control
- **Function calling conventions**
- **System calls** for program termination

The algorithm has O(n²) time complexity, which is typical for bubble sort.

