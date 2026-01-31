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
    movl $0, %esi                  # Initialize outer loop counter i = 0
    
outer_loop:
    cmpl $0, %ecx                  # Check if array size is 0
    jle end_sort                   # If size <= 0, exit
    
    movl $0, %edi                  # Initialize inner loop counter j = 0
    movl %ecx, %edx                # Copy array size to edx for inner loop
    
inner_loop:
    cmpl $1, %edx                  # Check if we need to compare (j < n-1)
    jle inner_loop_end             # If j >= n-1, exit inner loop
    
    # Compare array[j] and array[j+1]
    movl array(,%edi,4), %eax     # Load array[j] into eax
    movl array(,%edi,4), %ebx     # Load array[j] into ebx (for comparison)
    addl $4, %ebx                 # Point to array[j+1]
    movl (%ebx), %ebx             # Load array[j+1] into ebx
    
    cmpl %ebx, %eax               # Compare array[j] and array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)     # array[j] = array[j+1]
    movl %eax, array(,%edi,4), %ebx # array[j+1] = array[j]
    
no_swap:
    addl $1, %edi                  # j++
    subl $1, %edx                  # n--
    jmp inner_loop                 # Continue inner loop
    
inner_loop_end:
    subl $1, %ecx                  # n--
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
- **Memory addressing**: Array indexing with base+offset
- **Control flow**: Conditional jumps (jle, jmp)
- **Function calling**: Stack management with push/pop
- **System calls**: Exit function using int $0x80

The algorithm sorts an array of integers in ascending order using the bubble sort technique.

