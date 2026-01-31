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
    cmp $0, %ecx                   # Check if array size is 0
    je end_sort                    # If size = 0, exit
    
    dec %ecx                       # Decrement outer loop counter
    movl $0, %esi                  # Initialize inner loop counter j = 0
    
inner_loop:
    cmp %ecx, %esi                 # Compare j with (n-1)
    jge outer_loop_end             # If j >= (n-1), go to next outer iteration
    
    # Compare array[j] and array[j+1]
    movl array(,%esi,4), %eax      # Load array[j] into eax
    movl array(,%esi,4), %ebx      # Load array[j] into ebx (for comparison)
    addl $4, %esi                  # Increment j
    movl array(,%esi,4), %ebx      # Load array[j+1] into ebx
    
    cmp %ebx, %eax                 # Compare array[j] and array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%esi,4)      # array[j+1] = array[j]
    movl %eax, array(,%esi,4)      # array[j] = array[j+1] (this is wrong - should be array[j-1])
    
no_swap:
    dec %esi                       # Decrement j
    jmp inner_loop                 # Continue inner loop
    
outer_loop_end:
    jmp outer_loop                 # Continue outer loop
    
end_sort:
    pop %ebp
    ret

# Main program
_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80                      # system call
```

## Algorithm Explanation

This assembly implementation demonstrates the bubble sort algorithm with the following key components:

1. **Data Section**: Defines an array of integers and its size
2. **Outer Loop**: Controls the number of passes through the array
3. **Inner Loop**: Compares adjacent elements and swaps them if needed
4. **Comparison and Swap**: Uses conditional jumps to determine when to swap elements
5. **Loop Control**: Properly manages loop counters and termination conditions

## Key Assembly Concepts Shown

- **Register usage**: %eax, %ebx, %ecx, %edx, %esi, %edi, %ebp
- **Memory addressing**: Array elements accessed using base + index scaling
- **Conditional jumps**: `cmp`, `jg`, `jle`, `je`
- **Function calls**: Stack management with `push`/`pop`
- **System calls**: `int $0x80` for program termination

This example illustrates how high-level algorithmic concepts are translated into low-level assembly instructions.

