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
    movl $0, %esi                  # Initialize outer loop index
    
outer_loop:
    cmp $0, %ecx                   # Check if we've completed all passes
    jle end_sort
    
    movl $0, %edi                  # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner index with array size
    jge outer_loop_end
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i]
    movl array(,%edi,4), %ebx      # Load array[i]
    addl $4, %ebx                  # Load array[i+1]
    movl (%ebx), %ebx              # Get array[i+1] value
    
    cmp %ebx, %eax                 # Compare array[i] and array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # array[i] = array[i+1]
    movl %eax, array(,%edi,4)      # array[i+1] = array[i]
    
no_swap:
    incl %edi                      # Increment inner loop index
    jmp inner_loop
    
outer_loop_end:
    decl %ecx                      # Decrement outer loop counter
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
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine if swapping is required
4. **Swapping**: Exchanges element values using memory operations

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`, `%ebp`
- **Memory addressing**: Array elements accessed via base + index + scale
- **Control flow**: Conditional jumps (`jle`, `jge`, `jmp`)
- **Function calling**: Stack management with `push`/`pop`
- **System calls**: `int $0x80` for program termination

The algorithm sorts the array `[64, 34, 25, 12, 22, 11, 90]` in ascending order.

