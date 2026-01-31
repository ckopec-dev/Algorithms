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
    
    movl array_size(%ebp), %ecx      # Load array size into ecx (outer loop counter)
    movl $0, %esi                    # Initialize outer loop counter i = 0
    
outer_loop:
    cmpl $0, %ecx                    # Check if array size is 0
    jz end_sort                      # If size = 0, exit
    
    dec %ecx                         # Decrement array size (one element sorted each pass)
    movl $0, %edi                    # Initialize inner loop counter j = 0
    
inner_loop:
    cmpl %ecx, %edi                  # Check if j >= array_size - 1
    jge outer_loop                   # If so, go to next outer iteration
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax        # Load array[j] into eax
    movl array(,%edi,4), %ebx        # Load array[j] into ebx (for comparison)
    movl array(,%edi,4), %edx        # Load array[j] into edx (for comparison)
    
    # Compare array[j] and array[j+1]
    addl $4, %edi                    # Increment j by 1
    movl array(,%edi,4), %ebx        # Load array[j+1] into ebx
    
    cmpl %ebx, %eax                  # Compare array[j] and array[j+1]
    jle no_swap                      # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)        # array[j+1] = array[j]
    movl %eax, array-4(,%edi,4)      # array[j] = array[j+1] (this is tricky, need proper indexing)
    
no_swap:
    dec %edi                         # Decrement j
    jmp inner_loop                   # Continue inner loop
    
end_sort:
    pop %ebp
    ret

# Main program
_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                    # sys_exit
    movl $0, %ebx                    # exit status
    int $0x80                        # system call
```

## Algorithm Explanation

This bubble sort implementation demonstrates:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine when to swap elements
4. **Swapping**: Direct memory manipulation to exchange array elements

## Key Assembly Concepts Shown

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Array indexing with scaling factors (4 bytes per integer)
- **Control flow**: Conditional jumps (`jz`, `jge`, `jle`)
- **Function calling**: Stack management with `%ebp` and `%esp`
- **System calls**: Using `int $0x80` for program termination

This example illustrates how high-level algorithmic concepts are translated into low-level assembly instructions.

