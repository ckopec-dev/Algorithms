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
    movl $0, %esi                  # Initialize outer loop index i = 0
    
outer_loop:
    cmp $0, %ecx                   # Check if we've completed all passes
    je sort_done
    
    movl $0, %edi                  # Initialize inner loop index j = 0
    movl %ecx, %edx                # Copy array_size to edx for inner loop
    
inner_loop:
    cmp %edi, %edx                 # Compare j with (array_size - i - 1)
    jge inner_compare
    
    movl array(,%edi,4), %eax      # Load array[j]
    movl array(,%edi,4), %ebx      # Load array[j] (duplicate for comparison)
    movl array(,%edi,4), %ebx      # Load array[j+1] into ebx
    
    cmp %ebx, %eax                 # Compare array[j] and array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # array[j] = array[j+1]
    movl %eax, array(,%edi,4)      # array[j+1] = array[j]
    
no_swap:
    inc %edi                       # j++
    jmp inner_loop
    
    dec %ecx                       # i++
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

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Swapping**: Uses temporary registers to exchange element values
4. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Register usage**: %eax, %ebx, %ecx, %edx, %esi, %edi
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: Conditional jumps (cmp, jg, je, jmp)
- **Function calls**: Stack management with push/pop
- **System calls**: Exit syscall using int $0x80

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

