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
    cmp $0, %ecx                   # Check if array size is 0
    jle end_sort                   # If size <= 0, exit
    
    movl $0, %edi                  # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner index with array size
    jge end_inner                  # If inner >= array size, exit inner loop
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into eax
    movl array(,%edi,4), %ebx      # Load array[i] into ebx (duplicate)
    addl $4, %edi                  # Increment inner index
    movl array(,%edi,4), %ebx      # Load array[i+1] into ebx
    
    cmp %ebx, %eax                 # Compare array[i] with array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array-4(,%edi,4)    # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    jmp inner_loop                 # Continue inner loop
    
end_inner:
    dec %ecx                       # Decrement outer loop counter
    jmp outer_loop                 # Continue outer loop
    
end_sort:
    pop %ebp
    ret

# Main program execution
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
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Swapping**: Uses `mov` instructions to exchange element values
5. **Index Management**: Uses registers `%esi`, `%edi`, and `%ecx` for loop counters

## Key Assembly Concepts Demonstrated:

- **Memory Access**: Using base+offset addressing (`array(,%edi,4)`)
- **Control Flow**: `cmp`, `jle`, `jmp` instructions for conditional jumps
- **Register Usage**: Efficient use of CPU registers for variables
- **Function Calls**: Proper stack management with `push`/`pop`
- **System Calls**: Using `int $0x80` for program termination

The algorithm sorts an array of integers in ascending order using the classic bubble sort approach.

