# Assembly Algorithm Example: Bubble Sort

Here's an example of a bubble sort algorithm implemented in x86 Assembly language:

```assembly
.section .data
    array:      .long 64, 34, 25, 12, 22, 11, 90
    array_size: .long 7

.section .text
    .global _start

bubble_sort:
    pushl   %ebp
    movl    %esp, %ebp
    
    movl    8(%ebp), %eax      # array address
    movl    12(%ebp), %ecx     # array size
    
    movl    %ecx, %edx         # outer loop counter
    
outer_loop:
    testl   %edx, %edx         # check if counter is zero
    jz      sort_done          # if zero, sorting complete
    
    movl    %ecx, %esi         # inner loop counter = array_size
    decl    %esi               # decrement for array indexing
    
inner_loop:
    testl   %esi, %esi         # check if inner counter is zero
    jz      outer_dec          # if zero, go to next outer iteration
    
    movl    (%eax,%esi,4), %ebx   # load array[i]
    movl    -4(%eax,%esi,4), %edi # load array[i-1]
    
    cmpl    %ebx, %edi         # compare array[i-1] with array[i]
    jle     skip_swap          # if array[i-1] <= array[i], no swap needed
    
    # Swap elements
    movl    %ebx, -4(%eax,%esi,4)  # array[i-1] = array[i]
    movl    %edi, (%eax,%esi,4)    # array[i] = array[i-1]
    
skip_swap:
    decl    %esi               # decrement inner counter
    jmp     inner_loop         # continue inner loop
    
outer_dec:
    decl    %edx               # decrement outer counter
    jmp     outer_loop         # continue outer loop
    
sort_done:
    popl    %ebp
    ret

_start:
    # Call bubble_sort with array and size
    movl    $array, %eax
    movl    $array_size, %ecx
    call    bubble_sort
    
    # Exit program
    movl    $1, %eax           # sys_exit
    movl    $0, %ebx           # exit status
    int     $0x80
```

## Algorithm Explanation

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmpl` instruction to compare elements
4. **Swapping**: Exchanges elements when the condition is met
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Function calls** with stack management
- **Loop control** using conditional jumps
- **Memory addressing** with base + index + scale
- **Register usage** for variables and temporary storage
- **Comparison and branching** instructions
- **System calls** for program termination

The algorithm has O(n²) time complexity, which is typical for bubble sort, and works by repeatedly stepping through the list, comparing adjacent elements and swapping them if they're in the wrong order.

