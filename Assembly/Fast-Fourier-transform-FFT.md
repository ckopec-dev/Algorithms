# Assembly Algorithm Example: Bubble Sort

Here's an example of a bubble sort algorithm implemented in x86 Assembly language:

```assembly
.section .data
    array: .long 64, 34, 25, 12, 22, 11, 90
    array_size: .long 7

.section .text
    .global _start

bubble_sort:
    push %ebp
    mov %esp, %ebp
    
    movl array_size(%ebp), %ecx        # Load array size into ECX (outer loop counter)
    movl $0, %esi                      # Initialize outer loop index
    
outer_loop:
    cmp $0, %ecx                       # Check if we've completed all passes
    jle sort_done
    
    movl $0, %edi                      # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                     # Compare inner index with array size
    jge outer_loop_end
    
    # Calculate addresses for adjacent elements
    movl array(,%edi,4), %eax         # Load current element
    movl array(,%edi,4), %ebx         # Load current element (duplicate for comparison)
    movl array(,%edi,4), %ebx         # Load next element
    addl $4, %edi                     # Move to next element
    movl array(,%edi,4), %ebx         # Load next element
    
    # Compare adjacent elements
    cmp %ebx, %eax                    # Compare current with next
    jle no_swap                       # If current <= next, no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)         # Store next element at current position
    movl %eax, array(,%edi,4)         # Store current element at next position
    
no_swap:
    addl $1, %edi                     # Increment inner loop index
    jmp inner_loop                    # Continue inner loop
    
outer_loop_end:
    subl $1, %ecx                     # Decrement outer loop counter
    jmp outer_loop                    # Continue outer loop
    
sort_done:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                     # sys_exit
    movl $0, %ebx                     # exit status
    int $0x80                         # system call
```

## Algorithm Explanation

This bubble sort implementation:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Swapping**: Uses temporary registers to exchange values
4. **Optimization**: Reduces the number of comparisons in each pass

## Key Assembly Concepts Shown

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Array indexing with scaling factor (4 bytes per integer)
- **Control flow**: Conditional jumps (`jle`, `jge`, `jmp`)
- **Function calls**: Stack management with `%ebp` and `%esp`
- **System calls**: Exit routine using `int $0x80`

The algorithm sorts an array of 7 integers in ascending order using the classic bubble sort approach.

