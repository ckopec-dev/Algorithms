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
    jz end_sort                    # If size = 0, exit
    
    movl $0, %esi                  # Initialize inner loop counter j = 0
    movl %ecx, %edx                # Copy array size to edx for inner loop
    
inner_loop:
    cmpl $1, %edx                  # Check if we have at least 2 elements
    jz outer_loop_end              # If only 1 element, exit inner loop
    
    movl array(,%esi,4), %eax     # Load array[j] into eax
    movl array(,%esi,4), %ebx     # Load array[j] into ebx (copy)
    movl array(,%esi,4), %ebx     # Load array[j] into ebx (copy)
    addl $4, %ebx                 # Load array[j+1] into ebx
    
    cmpl %ebx, %eax               # Compare array[j] with array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%esi,4)     # array[j] = array[j+1]
    movl %eax, array(,%esi,4)     # array[j+1] = array[j]
    
no_swap:
    addl $1, %esi                  # j++
    decl %edx                      # decrement inner loop counter
    jmp inner_loop                 # continue inner loop
    
outer_loop_end:
    decl %ecx                      # decrement outer loop counter
    jmp outer_loop                 # continue outer loop
    
end_sort:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80                      # interrupt
```

## Algorithm Explanation:

This bubble sort implementation:
1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Swapping**: Uses temporary registers to exchange element values
4. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Register usage**: %eax, %ebx, %ecx, %edx, %esi, %edi
- **Memory addressing**: Using base + index + scale for array access
- **Control flow**: Conditional jumps (jz, jle, jmp)
- **Function calls**: Stack management with push/pop
- **System calls**: Exit function using int $0x80

The algorithm sorts an array of integers in ascending order using the classic bubble sort approach.

