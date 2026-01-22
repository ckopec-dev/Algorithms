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
    je end_sort
    
    movl $0, %edi                  # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner index with array size
    jge inner_loop_end
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into eax
    movl array(,%edi,4), %ebx      # Load array[i] into ebx
    addl $4, %ebx                  # Load array[i+1] into ebx
    movl (%ebx), %ebx              # Get value of array[i+1]
    
    cmp %ebx, %eax                 # Compare array[i] and array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    incl %edi                      # Increment inner loop index
    jmp inner_loop                 # Continue inner loop
    
inner_loop_end:
    decl %ecx                      # Decrement outer loop counter
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

This bubble sort implementation:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Swapping**: Uses `mov` instructions to exchange element values
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Shown

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`, `%ebp`
- **Memory addressing**: `array(,%edi,4)` for indexed array access
- **Control flow**: `cmp`, `je`, `jg`, `jmp` instructions
- **Function calls**: `push`, `pop`, `call`, `ret` for procedure calls
- **System calls**: `int $0x80` for program termination

This demonstrates fundamental assembly programming concepts while implementing a classic sorting algorithm.

