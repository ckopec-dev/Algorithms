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
    movl $0, %edx                  # Initialize outer loop counter
    
outer_loop:
    cmp $0, %ecx                   # Check if we've completed all passes
    je end_sort
    
    movl $0, %esi                  # Initialize inner loop counter
    
inner_loop:
    cmp %esi, %ecx                 # Compare inner counter with array size
    jge inner_loop_end
    
    # Compare adjacent elements
    movl array(,%esi,4), %eax      # Load array[i] into EAX
    movl array(,%esi,4), %ebx      # Load array[i] into EBX (copy)
    movl array(,%esi,4), %ebx      # Load array[i+1] into EBX
    
    # Compare and swap if needed
    cmpl %ebx, %eax                # Compare array[i] and array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%esi,4)      # Store array[i+1] in array[i]
    movl %eax, array(,%esi,4)      # Store array[i] in array[i+1]
    
no_swap:
    inc %esi                       # Increment inner loop counter
    jmp inner_loop                 # Continue inner loop
    
inner_loop_end:
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
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmpl` instruction to compare values
4. **Swapping**: Uses memory operations to exchange element positions
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%ebp`
- **Memory addressing**: `array(,%esi,4)` for indexed array access
- **Control flow**: `cmp`, `jle`, `jmp` instructions for conditional branching
- **Function calling**: Stack management with `push`/`pop` and `call`/`ret`
- **System calls**: `int $0x80` for program termination

This example shows how high-level algorithmic concepts are implemented using low-level assembly instructions and register operations.

