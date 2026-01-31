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
    
    movl array_size(%esp), %ecx    # Load array size into ecx (outer loop counter)
    movl $0, %esi                  # Initialize outer loop index i = 0
    
outer_loop:
    cmp $0, %ecx                   # Check if array size is 0
    jle end_sort                   # If size <= 0, exit
    
    dec %ecx                       # Decrement outer loop counter
    movl $0, %edi                  # Initialize inner loop index j = 0
    
inner_loop:
    movl array(,%edi,4), %eax     # Load array[j] into eax
    movl array(,%edi,4), %ebx     # Load array[j] into ebx (for comparison)
    
    cmp %ebx, %eax                 # Compare array[j] with array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl array(,%edi,4), %eax     # Load array[j] into eax
    movl array(4,%edi,4), %ebx    # Load array[j+1] into ebx
    movl %ebx, array(,%edi,4)     # Store array[j+1] at array[j]
    movl %eax, array(4,%edi,4)    # Store array[j] at array[j+1]
    
no_swap:
    inc %edi                       # Increment inner loop index
    cmp %ecx, %edi                 # Compare j with array_size-1
    jl inner_loop                  # If j < array_size-1, continue inner loop
    
    jmp outer_loop                 # Continue with outer loop

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

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses conditional jumps to determine if swapping is needed
4. **Swapping**: Direct memory operations to exchange array elements

## Key Assembly Concepts Demonstrated:

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`, `%ebp`
- **Memory addressing**: Array elements accessed via base + index + scale
- **Control flow**: Conditional jumps (`jle`, `jl`, `jmp`)
- **Function calling**: Stack management and procedure calls
- **System calls**: Program termination using `int $0x80`

The algorithm has O(n²) time complexity, which is typical for bubble sort.

