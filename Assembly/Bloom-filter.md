# Assembly Algorithm Example: Bubble Sort

Here's an example of a bubble sort algorithm implemented in x86 Assembly language:

```assembly
.section .data
    array:      .long 64, 34, 25, 12, 22, 11, 90
    array_size: .long 7

.section .text
    .global _start

bubble_sort:
    push %ebp
    mov %esp, %ebp
    
    movl array_size(%ebp), %ecx    # Load array size into ecx (outer loop counter)
    movl $0, %esi                  # Initialize outer loop index i = 0
    
outer_loop:
    cmpl $0, %ecx                  # Check if array size is 0
    jle end_sort                   # If size <= 0, exit
    
    dec %ecx                       # Decrement counter (we need n-1 passes)
    movl $0, %edi                  # Initialize inner loop index j = 0
    
inner_loop:
    cmpl %ecx, %edi                # Compare j with (n-1-i)
    jge outer_loop_end             # If j >= (n-1-i), go to next outer iteration
    
    # Compare array[j] with array[j+1]
    movl array(,%edi,4), %eax      # Load array[j] into eax
    movl array(,%edi,4), %ebx      # Load array[j] into ebx (for comparison)
    movl array(,%edi,4), %edx      # Load array[j] into edx (for comparison)
    
    # Compare array[j] and array[j+1]
    movl array(,%edi,4), %edx      # Load array[j+1] into edx
    cmpl %edx, %eax                # Compare array[j] with array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %edx, array(,%edi,4)      # array[j] = array[j+1]
    movl %eax, array(,%edi,4)      # array[j+1] = array[j]
    
no_swap:
    inc %edi                       # Increment j
    jmp inner_loop                 # Continue inner loop
    
outer_loop_end:
    inc %esi                       # Increment i
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

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses conditional jumps to determine if swapping is needed
4. **Swapping**: Direct memory operations to exchange array elements

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Array indexing with base + offset
- **Control flow**: Conditional jumps (`jle`, `jge`, `jmp`)
- **Function calls**: Stack management with `%ebp` and `%esp`
- **System calls**: Exit function using `int $0x80`

The algorithm has O(n²) time complexity, similar to the high-level bubble sort implementation.

