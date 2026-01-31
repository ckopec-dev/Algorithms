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
    movl array(,%edi,4), %ebx      # Load array[i] into EBX (for comparison)
    movl array(,%edi,4), %edx      # Load array[i] into EDX (for comparison)
    
    # Load array[i+1] into EDX
    movl array(,%edi,4), %edx
    addl $4, %edx                  # Point to array[i+1]
    movl (%edx), %edx              # Load array[i+1]
    
    # Compare and swap if needed
    cmp %edx, %eax                 # Compare array[i] with array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %edx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    inc %edi                       # Increment inner loop index
    jmp inner_loop
    
outer_loop_end:
    dec %ecx                       # Decrement outer loop counter
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

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses conditional jumps to determine if swapping is needed
4. **Swapping**: Direct memory operations to exchange element values

## Key Assembly Concepts Demonstrated

- **Register usage**: EAX, EBX, ECX, EDX, ESI, EDI for various operations
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: Conditional jumps (JLE, JMP) for loop and decision making
- **Function calling**: Stack management for procedure calls
- **System calls**: Exit syscall for program termination

The algorithm has O(n²) time complexity, which is typical for bubble sort, but implemented in low-level assembly instructions.

