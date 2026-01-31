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
    
    movl array_size(%esp), %ecx    # Load array size into ECX (outer loop counter)
    movl $0, %esi                  # Initialize outer loop counter
    
outer_loop:
    cmp $0, %ecx                   # Check if we've completed all passes
    je end_sort
    
    movl $0, %edi                  # Initialize inner loop counter
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner counter with array size
    je inner_loop_end
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX (for comparison)
    addl $4, %ebx                  # Load array[i+1] into EBX
    
    cmp %ebx, %eax                 # Compare array[i] with array[i+1]
    jle no_swap                    # Jump if array[i] <= array[i+1]
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    inc %edi                       # Increment inner counter
    jmp inner_loop
    
inner_loop_end:
    dec %ecx                       # Decrement outer counter
    jmp outer_loop
    
end_sort:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80
```

## Algorithm Explanation

This bubble sort implementation:
1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine if swapping is required
4. **Swapping**: Exchanges elements when the left element is greater than the right element

## Key Assembly Concepts Demonstrated

- **Register usage**: EAX, EBX, ECX, EDI, ESI, EBP
- **Memory addressing**: Array elements accessed via base+offset
- **Control flow**: Conditional jumps (je, jle) and loops
- **Function calls**: Stack management with push/pop
- **System calls**: Exit routine using int $0x80

The algorithm has O(n²) time complexity and sorts the array in ascending order.

