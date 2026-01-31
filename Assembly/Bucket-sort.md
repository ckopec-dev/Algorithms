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
    movl $0, %esi                  # Initialize outer loop index i = 0
    
outer_loop:
    cmp $0, %ecx                   # Check if array size is 0
    jle end_sort                   # If size <= 0, exit
    
    dec %ecx                       # Decrement outer counter
    movl $0, %edi                  # Initialize inner loop index j = 0
    
inner_loop:
    movl array(,%edi,4), %eax     # Load array[j] into eax
    movl array(,%edi,4), %ebx     # Load array[j] into ebx (for comparison)
    movl array(,%edi,4), %edx     # Load array[j] into edx (for comparison)
    
    cmp %edx, %eax                 # Compare array[j] with array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl array(,%edi,4), %eax     # Load array[j]
    movl array(4,%edi,4), %ebx    # Load array[j+1]
    movl %ebx, array(,%edi,4)     # array[j] = array[j+1]
    movl %eax, array(4,%edi,4)    # array[j+1] = array[j]
    
no_swap:
    inc %edi                       # Increment j
    cmp %ecx, %edi                 # Compare j with array_size-1
    jl inner_loop                  # Continue inner loop if j < array_size-1
    
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
1. **Outer loop**: Controls the number of passes through the array
2. **Inner loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Swapping**: Uses temporary registers to exchange element values
4. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: Conditional jumps (`cmp`, `jl`, `jle`, `jmp`)
- **Function calls**: Stack management with `push`/`pop`
- **System calls**: Program termination using `int $0x80`

The algorithm sorts the array in ascending order by repeatedly stepping through the list, comparing adjacent elements and swapping them if they're in the wrong order.

