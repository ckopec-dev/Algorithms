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
    
    movl array_size(%ebp), %ecx    # Load array size into ECX (outer loop counter)
    movl $0, %esi                  # Initialize outer loop index
    
outer_loop:
    cmp $0, %ecx                   # Check if array size is 0
    je end_sort                    # If 0, exit
    
    movl $0, %edi                  # Initialize inner loop index
    movl %ecx, %edx                # Copy array size to EDX for inner loop
    
inner_loop:
    cmp %edi, %edx                 # Compare indices
    jge inner_loop_end             # If inner index >= outer size, exit inner loop
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX (for comparison)
    movl array(,%edi,4), %ebx      # Load array[i+1] into EBX
    
    # Compare array[i] and array[i+1]
    cmp %ebx, %eax                 # Compare elements
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    incl %edi                      # Increment inner loop index
    jmp inner_loop                 # Continue inner loop
    
inner_loop_end:
    decl %ecx                      # Decrement outer loop counter
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
1. **Outer loop**: Controls the number of passes through the array
2. **Inner loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine if swapping is required
4. **Swapping**: Exchanges elements when the left element is greater than the right element

## Key Assembly Concepts Demonstrated:

- **Register usage**: EAX, EBX, ECX, EDX, ESI, EDI for various operations
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: Conditional jumps (je, jg, jle) for loop control
- **Function calls**: Stack management for procedure calls
- **System calls**: Exit routine using int $0x80

The algorithm has O(n²) time complexity and sorts the array in ascending order.

