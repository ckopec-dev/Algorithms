# Assembly Algorithm Example: Bubble Sort

Here's an example of a bubble sort algorithm implemented in x86 Assembly language:

```assembly
.section .data
    array:      .long 64, 34, 25, 12, 22, 11, 90
    array_size: .long 7

.section .text
    .global _start

bubble_sort:
    push %ebp
    mov %esp, %ebp
    
    movl array_size(%ebp), %ecx    # Load array size into ecx (outer loop counter)
    movl $0, %edx                  # Initialize outer loop counter
    
outer_loop:
    cmp $0, %ecx                   # Check if we've completed all passes
    je sort_done
    
    movl $0, %esi                  # Initialize inner loop counter
    
inner_loop:
    cmp %esi, %ecx                 # Compare inner counter with array size
    je inner_loop_done
    
    # Compare adjacent elements
    movl array(,%esi,4), %eax      # Load array[i] into eax
    movl array(,%esi,4), %ebx      # Load array[i] into ebx (for comparison)
    movl array(,%esi,4), %ebx      # Load array[i+1] into ebx
    
    # Compare array[i] and array[i+1]
    cmp %ebx, %eax                 # Compare elements
    jle no_swap                    # Jump if array[i] <= array[i+1]
    
    # Swap elements
    movl %ebx, array(,%esi,4)      # array[i] = array[i+1]
    movl %eax, array(4,%esi,4)     # array[i+1] = array[i]
    
no_swap:
    inc %esi                       # Increment inner counter
    jmp inner_loop
    
inner_loop_done:
    dec %ecx                       # Decrement outer counter
    jmp outer_loop
    
sort_done:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80                      # system call
```

## Algorithm Explanation

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Conditional Jump**: `jle` (jump if less or equal) to decide when to swap
5. **Swapping**: Direct memory manipulation to exchange element values

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%ebp`
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: Conditional jumps and loop structures
- **Function calling**: Stack management and procedure calls
- **System calls**: Exit routine using `int $0x80`

The algorithm has O(n²) time complexity, which is typical for bubble sort implementation.

