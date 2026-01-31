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
    movl $0, %edi                  # Initialize outer loop counter i = 0
    
outer_loop:
    cmpl $0, %ecx                  # Check if array size is 0
    jle end_sort                   # If size <= 0, exit
    
    movl $0, %esi                  # Initialize inner loop counter j = 0
    movl %ecx, %edx                # Copy array size to edx for inner loop
    
inner_loop:
    cmpl $1, %edx                  # Check if we have at least 2 elements
    jle inner_loop_end             # If less than 2 elements, exit inner loop
    
    # Compare adjacent elements
    movl array(,%esi,4), %eax      # Load array[j] into eax
    movl array(,%esi,4), %ebx      # Load array[j] into ebx (duplicate)
    addl $4, %esi                  # Increment j
    movl array(,%esi,4), %ebx      # Load array[j+1] into ebx
    
    cmpl %ebx, %eax                # Compare array[j] with array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%esi,4)      # array[j+1] = array[j]
    movl %eax, array-4(,%esi,4)    # array[j] = array[j+1] (previous value)
    
no_swap:
    decl %edx                      # Decrement inner loop counter
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

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmpl` instruction to compare values
4. **Swapping**: Exchanges elements by storing values in memory locations
5. **Loop Control**: Uses counters and conditional jumps to control flow

## Key Assembly Concepts Demonstrated

- **Memory Access**: Using base+offset addressing (`array(,%esi,4)`)
- **Conditional Jumps**: `jle`, `jl`, `jmp` for loop control
- **Register Usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Function Calls**: `call` and `ret` for procedure calls
- **System Calls**: `int $0x80` for program termination

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

