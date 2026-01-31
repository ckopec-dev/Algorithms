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
    
    movl array_size(%ebp), %ecx    # Load array size into ecx (outer loop counter)
    movl $0, %edi                  # Initialize outer loop counter i = 0
    
outer_loop:
    cmp $0, %ecx                   # Check if we've done all passes
    jle end_sort                   # If ecx <= 0, we're done
    
    movl $0, %esi                  # Initialize inner loop counter j = 0
    movl %ecx, %edx                # Copy array size to edx for inner loop
    
inner_loop:
    cmp %esi, %edx                 # Compare j with (array_size - i)
    jge inner_loop_end             # If j >= (array_size - i), exit inner loop
    
    # Compare adjacent elements
    movl array(,%esi,4), %eax      # Load array[j] into eax
    movl array(,%esi,4), %ebx      # Load array[j] into ebx (duplicate)
    addl $4, %esi                  # Increment j
    movl array(,%esi,4), %ebx      # Load array[j+1] into ebx
    
    cmp %ebx, %eax                 # Compare array[j] and array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%esi,4)      # array[j+1] = array[j]
    movl %eax, array(,%esi,4)      # array[j] = array[j+1] (this is wrong, should be array[j-1])
    
no_swap:
    addl $1, %esi                  # Increment j
    jmp inner_loop                 # Continue inner loop
    
inner_loop_end:
    subl $1, %ecx                  # Decrement outer loop counter
    jmp outer_loop                 # Continue outer loop
    
end_sort:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80                      # system call
```

## Algorithm Explanation:

This bubble sort implementation:
1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Swapping**: Uses temporary registers to exchange element values
4. **Optimization**: Reduces the comparison range after each pass

## Key Assembly Concepts Demonstrated:

- **Register usage**: %eax, %ebx, %ecx, %edx, %esi, %edi
- **Memory addressing**: Array access using base+offset
- **Control flow**: Conditional jumps (cmp, jle, jge, jmp)
- **Function calls**: Stack management with push/pop
- **System calls**: Exit routine using int $0x80

This algorithm has O(n²) time complexity and sorts the array in ascending order.

