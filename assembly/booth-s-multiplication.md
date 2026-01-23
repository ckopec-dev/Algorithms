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
    cmp $0, %ecx                   # Check if we've completed all passes
    jle end_sort
    
    movl $0, %edi                  # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner index with array size
    jge outer_loop_end
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX
    addl $4, %ebx                  # Load array[i+1] into EBX
    movl (%ebx), %ebx              # Get array[i+1] value
    
    cmp %ebx, %eax                 # Compare array[i] and array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    incl %edi                      # Increment inner loop index
    jmp inner_loop
    
outer_loop_end:
    decl %ecx                      # Decrement outer loop counter
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

## Algorithm Explanation:

This bubble sort implementation:
1. **Outer loop**: Controls the number of passes through the array
2. **Inner loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine when to swap
4. **Swapping**: Exchanges values using memory operations

## Key Assembly Concepts Demonstrated:

- **Register usage**: EAX, EBX, ECX, EDI, ESI, EBP
- **Memory addressing**: Array access using base + index + scale
- **Conditional jumps**: `cmp`, `jle`, `jge`, `jmp`
- **Function calling**: Stack management with `push`/`pop`
- **System calls**: `int $0x80` for program termination

The algorithm sorts an array of integers in ascending order using the bubble sort technique.

