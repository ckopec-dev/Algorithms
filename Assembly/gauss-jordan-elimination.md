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
    movl $0, %esi                  # Initialize outer loop counter i = 0
    
outer_loop:
    cmp $0, %ecx                   # Check if we've done all passes
    je end_sort
    
    movl $0, %edi                  # Initialize inner loop counter j = 0
    
inner_loop:
    cmp %edi, %ecx                 # Compare j with (array_size - i - 1)
    jge end_inner
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[j] into eax
    movl array(,%edi,4), %ebx      # Load array[j] into ebx (for comparison)
    movl array(,%edi,4), %edx      # Load array[j] into edx (for comparison)
    
    # Compare array[j] and array[j+1]
    addl $4, %edi                  # j++
    movl array(,%edi,4), %ebx      # Load array[j+1] into ebx
    
    cmp %ebx, %eax                 # Compare array[j] with array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # array[j+1] = array[j]
    movl %eax, array(,%edi,4)      # array[j] = array[j+1]
    
no_swap:
    addl $1, %edi                  # j++
    jmp inner_loop
    
end_inner:
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

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses conditional jumps to determine if swapping is needed
4. **Swapping**: Direct memory operations to exchange element values

## Key Assembly Concepts Demonstrated:

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: Conditional jumps (`je`, `jg`, `jmp`)
- **Function calls**: Stack management with `push`/`pop`
- **System calls**: `int $0x80` for program termination

The algorithm has O(n²) time complexity and sorts the array in ascending order.

