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
    cmpl %ecx, %edi                # Compare i with array_size
    jge end_outer                  # If i >= array_size, exit outer loop
    
    movl $0, %esi                  # Initialize inner loop counter j = 0
    movl %ecx, %edx                # Copy array_size to edx
    decl %edx                      # Decrement by 1 (n-1)
    subl %edi, %edx                # Subtract i to get (n-1-i)
    
inner_loop:
    cmpl %esi, %edx                # Compare j with (n-1-i)
    jge end_inner                  # If j >= (n-1-i), exit inner loop
    
    # Compare adjacent elements
    movl array(,%esi,4), %eax      # Load array[j] into eax
    movl array(,%esi,4), %ebx      # Load array[j] into ebx
    movl array(,%esi,4), %ebx      # Load array[j] into ebx
    
    # Compare array[j] and array[j+1]
    movl array(,%esi,4), %ebx      # Load array[j+1] into ebx
    cmpl %ebx, %eax                # Compare array[j] and array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl array(,%esi,4), %ebx      # Load array[j] into ebx
    movl array(,%esi,4), %eax      # Load array[j+1] into eax
    
    # Store swapped values back
    movl %ebx, array(,%esi,4)      # Store array[j+1] at array[j]
    movl %eax, array(,%esi,4)      # Store array[j] at array[j+1]
    
no_swap:
    incl %esi                      # Increment j
    jmp inner_loop                 # Continue inner loop
    
end_inner:
    incl %edi                      # Increment i
    jmp outer_loop                 # Continue outer loop
    
end_outer:
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
4. **Swapping**: Exchanges values using memory operations

## Key Assembly Concepts Demonstrated:

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`, `%ebp`
- **Memory addressing**: Array elements accessed using base + index + scale
- **Control flow**: Conditional jumps (`jge`, `jle`) for loop control
- **Function calls**: Stack management for procedure calls
- **System calls**: Exit functionality using `int $0x80`

The algorithm has O(n²) time complexity, typical for bubble sort, and sorts the array in ascending order.

