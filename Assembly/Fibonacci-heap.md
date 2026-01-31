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
    
    movl array_size(%ebp), %ecx        # Load array size into ecx (outer loop counter)
    movl $0, %esi                      # Initialize outer loop counter i = 0
    
outer_loop:
    cmp $0, %ecx                       # Check if array size is 0
    je sort_done                       # If size is 0, sorting is done
    
    dec %ecx                           # Decrement outer loop counter
    movl $0, %edi                      # Initialize inner loop counter j = 0
    
inner_loop:
    movl array(,%edi,4), %eax         # Load array[j] into eax
    movl array(,%edi,4), %ebx         # Load array[j] into ebx (copy)
    cmp %ebx, %eax                    # Compare array[j] with array[j+1]
    jge next_iteration                # If array[j] >= array[j+1], skip swap
    
    # Swap elements
    movl array(,%edi,4), %ebx         # Load array[j] into ebx
    movl array(4,%edi,4), %eax        # Load array[j+1] into eax
    movl %eax, array(,%edi,4)         # array[j] = array[j+1]
    movl %ebx, array(4,%edi,4)        # array[j+1] = array[j]
    
next_iteration:
    inc %edi                           # Increment inner loop counter
    cmp array_size(%ebp), %edi         # Compare with array size
    jl inner_loop                      # Continue inner loop if j < size
    
    jmp outer_loop                     # Continue outer loop

sort_done:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                      # sys_exit
    movl $0, %ebx                      # exit status
    int $0x80                          # system call
```

## Algorithm Explanation

This bubble sort implementation:
1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Swapping**: Uses temporary registers to exchange element values
4. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Register usage**: %eax, %ebx, %ecx, %edx, %esi, %edi, %ebp
- **Memory addressing**: Array elements accessed via base + index * scale
- **Control flow**: Conditional jumps (cmp, jge, jl, je)
- **Function calls**: Stack management with push/pop
- **System calls**: Exit function using int $0x80

This algorithm has O(n²) time complexity and sorts the array in ascending order.

