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
    cmp $0, %ecx                   # Check if we've done all passes
    jle end_sort                   # If ECX <= 0, we're done
    
    movl $0, %edi                  # Initialize inner loop index
    movl array_size(%ebp), %edx    # Load array size into EDX
    
outer_loop_inner:
    cmp %edi, %edx                 # Compare inner index with array size
    jge end_inner_loop             # If index >= size, exit inner loop
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i]
    movl array(,%edi,4), %ebx      # Load array[i] (copy)
    addl $4, %edi                  # Increment index
    movl array(,%edi,4), %ebx      # Load array[i+1]
    
    cmp %ebx, %eax                 # Compare array[i] and array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # array[i+1] = array[i]
    movl %eax, array(,%edi,4)      # array[i] = array[i+1]
    
no_swap:
    addl $1, %edi                  # Increment inner loop index
    jmp outer_loop_inner           # Continue inner loop
    
end_inner_loop:
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

## Algorithm Explanation

This bubble sort implementation:
1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine if swapping is required
4. **Swapping**: Exchanges elements when the condition is met
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Register usage**: EAX, EBX, ECX, EDX, ESI, EDI for different purposes
- **Memory addressing**: Using base register with offset for array access
- **Control flow**: Conditional jumps (cmp, jle, jge) for loop control
- **Function calling**: Stack management with push/pop operations
- **System calls**: Using int $0x80 for program termination

The algorithm sorts an array of integers in ascending order using the bubble sort technique, which repeatedly steps through the list, compares adjacent elements, and swaps them if they're in the wrong order.

