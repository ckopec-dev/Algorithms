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
    jz sort_done
    
    movl $0, %edi                  # Initialize inner loop counter
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner counter with array size
    jge inner_loop_end
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX
    movl array(,%edi,4), %edx      # Load array[i] into EDX
    
    # Compare array[i] and array[i+1]
    addl $4, %edi                  # Move to next element
    movl array(,%edi,4), %ebx      # Load array[i+1] into EBX
    
    cmp %ebx, %eax                 # Compare elements
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    addl $1, %edi                  # Increment inner counter
    jmp inner_loop
    
inner_loop_end:
    subl $1, %ecx                  # Decrement outer counter
    jmp outer_loop
    
sort_done:
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
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Swapping**: Moves values between memory locations when necessary
5. **Loop Control**: Uses conditional jumps (`jz`, `jge`, `jle`) to control loop execution

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`, `%ebp`
- **Memory addressing**: `array(,%edi,4)` for indexed array access
- **Control flow**: `jmp`, `jz`, `jge`, `jle` conditional jumps
- **Function calls**: `call` and `ret` instructions
- **System calls**: `int $0x80` for program termination

This example shows how high-level algorithmic concepts are implemented using low-level assembly instructions and addressing modes.

