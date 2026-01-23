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
    movl $0, %esi                  # Initialize outer loop counter i = 0
    
outer_loop:
    cmpl %ecx, %esi                # Compare i with array_size
    jge end_outer                  # If i >= array_size, exit outer loop
    
    movl $0, %edi                  # Initialize inner loop counter j = 0
    movl %ecx, %edx                # Copy array_size to edx
    decl %edx                      # Decrement by 1 (array_size - 1)
    subl %esi, %edx                # edx = (array_size - 1) - i
    
inner_loop:
    cmpl %edi, %edx                # Compare j with (array_size - 1 - i)
    jge end_inner                  # If j >= (array_size - 1 - i), exit inner loop
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[j] into eax
    movl array(,%edi,4), %ebx      # Load array[j] into ebx (copy)
    movl array(,%edi,4), %ebx      # Load array[j+1] into ebx
    
    # Compare and swap if needed
    cmpl %ebx, %eax                # Compare array[j] with array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # array[j] = array[j+1]
    movl %eax, array(4,%edi,4)     # array[j+1] = array[j]
    
no_swap:
    incl %edi                      # Increment j
    jmp inner_loop                 # Continue inner loop
    
end_inner:
    incl %esi                      # Increment i
    jmp outer_loop                 # Continue outer loop
    
end_outer:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80                      # system call
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmpl` instruction to compare values
4. **Swapping**: Exchanges elements when the condition is met
5. **Termination**: Loops continue until the entire array is sorted

## Key Assembly Concepts Demonstrated:

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: `array(,%edi,4)` for indexed array access
- **Control flow**: `jmp`, `jge`, `jle` conditional jumps
- **Function calls**: `call` and `ret` instructions
- **System calls**: `int $0x80` for program termination

The algorithm has O(n²) time complexity, which is typical for bubble sort.

