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
    
inner_loop:
    cmpl %edi, %edx                # Compare j with (array_size - 1)
    jge end_inner                  # If j >= (array_size - 1), exit inner loop
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[j] into eax
    movl array(,%edi,4), %ebx      # Load array[j] into ebx
    movl array(,%edi,4), %ebx      # Load array[j+1] into ebx
    
    cmpl %ebx, %eax                # Compare array[j] and array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # array[j] = array[j+1]
    movl %eax, array(,%edi,4)      # array[j+1] = array[j]
    
no_swap:
    incl %edi                      # j++
    jmp inner_loop
    
end_inner:
    decl %ecx                      # array_size--
    incl %esi                      # i++
    jmp outer_loop
    
end_outer:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses conditional jumps to determine if swapping is needed
4. **Swapping**: Direct memory operations to exchange array elements

## Key Assembly Concepts Demonstrated:

- **Register usage**: %eax, %ebx, %ecx, %edx, %esi, %edi
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: Conditional jumps (jge, jle, jmp)
- **Function calling**: Stack management with push/pop
- **System calls**: Exit function using int $0x80

This example shows how high-level algorithm concepts are translated into low-level assembly instructions.

