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
    
    movl array_size(%ebp), %ecx      # Load array size into ecx (outer loop counter)
    movl $0, %esi                    # Initialize outer loop counter i = 0
    
outer_loop:
    cmpl $0, %ecx                    # Check if array size is 0
    jle end_sort                     # If size <= 0, exit
    
    movl $0, %edi                    # Initialize inner loop counter j = 0
    movl %ecx, %edx                  # Copy array size to edx for inner loop
    
inner_loop:
    cmpl $1, %edx                    # Check if we have at least 2 elements
    jle inner_loop_end               # If less than 2, exit inner loop
    
    # Compare array[j] and array[j+1]
    movl array(,%edi,4), %eax       # Load array[j] into eax
    movl array(,%edi,4), %ebx       # Load array[j] into ebx (for comparison)
    addl $4, %edi                   # Increment j by 1
    movl array(,%edi,4), %ebx       # Load array[j+1] into ebx
    
    cmpl %ebx, %eax                 # Compare array[j] and array[j+1]
    jle no_swap                      # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array-4(,%edi,4)     # Store array[j+1] at array[j]
    movl %eax, array(,%edi,4)       # Store array[j] at array[j+1]
    
no_swap:
    decl %edx                        # Decrement inner loop counter
    jmp inner_loop                   # Continue inner loop
    
inner_loop_end:
    decl %ecx                        # Decrement outer loop counter
    jmp outer_loop                   # Continue outer loop
    
end_sort:
    pop %ebp
    ret

# Main program
_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                    # sys_exit
    movl $0, %ebx                    # exit status
    int $0x80
```

## Algorithm Explanation

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmpl` instruction to compare values
4. **Swapping**: Direct memory manipulation to exchange element positions
5. **Termination**: Loops continue until no more swaps are needed

## Key Assembly Concepts Shown

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: `array(,%edi,4)` for indexed array access
- **Control flow**: `jmp`, `jle`, `jge` conditional jumps
- **Function calls**: `call` and `ret` instructions
- **System calls**: `int $0x80` for program termination

This demonstrates fundamental assembly programming concepts while implementing a classic sorting algorithm.

