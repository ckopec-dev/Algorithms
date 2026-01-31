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
    
    dec %ecx                       # Decrement outer counter
    movl $0, %edi                  # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner index with outer counter
    jge inner_end                  # If inner >= outer, exit inner loop
    
    # Calculate array indices
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX (for comparison)
    movl array(,%edi,4), %edx      # Load array[i] into EDX (for comparison)
    
    # Compare adjacent elements
    addl $4, %edi                  # Increment inner index
    movl array(,%edi,4), %ebx      # Load array[i+1] into EBX
    
    cmp %ebx, %eax                 # Compare array[i] with array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # array[i+1] = array[i]
    movl %eax, array(,%edi,4)      # array[i] = array[i+1]
    
no_swap:
    jmp inner_loop                 # Continue inner loop
    
inner_end:
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

## Algorithm Explanation

This bubble sort implementation demonstrates:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Element Swapping**: Uses registers to temporarily store values during swapping
4. **Index Calculation**: Uses `array(,%edi,4)` to access array elements (4 bytes per integer)

## Key Assembly Concepts Shown

- **Register usage**: EAX, EBX, ECX, EDX, ESI, EDI for various operations
- **Memory addressing**: Array access using indexed addressing mode
- **Control flow**: Conditional jumps (je, jg, jle) for loop and comparison logic
- **Function calling**: Stack management for procedure calls
- **System calls**: Exit system call for program termination

The algorithm sorts an array of integers in ascending order using the bubble sort technique, where adjacent elements are repeatedly compared and swapped if they're in the wrong order.

