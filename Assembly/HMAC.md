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
    
    movl array_size(%ebp), %ecx    # Load array size into ECX (outer loop counter)
    movl $0, %esi                  # Initialize outer loop counter
    
outer_loop:
    cmp $0, %ecx                   # Check if we've completed all passes
    jle end_sort
    
    movl $0, %edi                  # Initialize inner loop counter
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner counter with array size
    jge end_inner_loop
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i]
    movl array(,%edi,4), %ebx      # Load array[i] (duplicate)
    
    # Compare array[i] and array[i+1]
    addl $4, %edi                  # Increment to next element
    movl array(,%edi,4), %ebx      # Load array[i+1]
    
    # If array[i] > array[i+1], swap them
    cmp %ebx, %eax                 # Compare elements
    jle no_swap                    # Jump if no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array-4(,%edi,4)    # Store array[i] at array[i+1]
    
no_swap:
    jmp inner_loop                 # Continue inner loop
    
end_inner_loop:
    dec %ecx                       # Decrement outer loop counter
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
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine if swapping is required
4. **Swapping**: Exchanges elements in memory when the condition is met

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`, `%ebp`
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: Conditional jumps (`jle`, `jge`, `jmp`)
- **Function calls**: Stack management with `push`/`pop`
- **System calls**: Exit routine using `int $0x80`

The algorithm has O(n²) time complexity, which is typical for bubble sort.

