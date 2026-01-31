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
    
    movl array_size(%ebp), %ecx      # Load array size into ECX (outer loop counter)
    movl $0, %esi                    # Initialize outer loop index
    
outer_loop:
    cmp $0, %ecx                     # Check if we've completed all passes
    jle end_sort
    
    movl $0, %edi                    # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                   # Compare inner index with array size
    jge outer_loop_end
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax       # Load array[i]
    movl array(,%edi,4), %ebx       # Load array[i] (duplicate for comparison)
    movl array(,%edi,4), %ebx       # Load array[i+1] into EBX
    
    # Swap if array[i] > array[i+1]
    cmp %ebx, %eax
    jle no_swap
    
    # Swap elements
    movl %ebx, array(,%edi,4)       # array[i] = array[i+1]
    movl %eax, array(4,%edi,4)      # array[i+1] = array[i]
    
no_swap:
    inc %edi                         # Increment inner loop index
    jmp inner_loop
    
outer_loop_end:
    dec %ecx                         # Decrement outer loop counter
    jmp outer_loop
    
end_sort:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                    # sys_exit
    movl $0, %ebx                    # exit status
    int $0x80
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Swapping**: Uses `mov` instructions to exchange element values
5. **Loop Control**: Uses conditional jumps (`jle`, `jge`, `jmp`) to control flow

## Key Assembly Concepts Demonstrated:

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Array elements accessed via base + index + scale
- **Conditional jumps**: `jle`, `jge`, `jmp` for loop control
- **Function calls**: `push`, `pop`, `call`, `ret` for procedure calls
- **System calls**: `int $0x80` for program termination

This example shows how high-level algorithmic concepts are implemented using low-level assembly instructions and register operations.

