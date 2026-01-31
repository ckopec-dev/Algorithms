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
    decl    %esi               # decrement for 0-based indexing
    
inner_loop:
    testl   %esi, %esi         # check if inner counter is zero
    jz      outer_dec          # if zero, go to next outer iteration
    
    movl    (%eax), %ebx       # load first element
    movl    4(%eax), %edi      # load second element
    
    cmpl    %edi, %ebx         # compare elements
    jle     skip_swap          # if first <= second, no swap needed
    
    # Swap elements
    movl    %edi, (%eax)       # store second element in first position
    movl    %ebx, 4(%eax)      # store first element in second position
    
skip_swap:
    addl    $4, %eax           # move to next pair
    decl    %esi               # decrement inner counter
    jmp     inner_loop         # continue inner loop
    
outer_dec:
    decl    %edx               # decrement outer counter
    jmp     outer_loop         # continue outer loop
    
sort_done:
    popl    %ebp
    ret

_start:
    movl    $array, %eax       # load array address
    movl    array_size, %ecx   # load array size
    call    bubble_sort        # call bubble sort function
    
    # Exit program
    movl    $1, %eax           # sys_exit
    movl    $0, %ebx           # exit status
    int     $0x80              # system call
```

## Algorithm Explanation

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses `cmpl` instruction to compare elements
4. **Swapping**: Exchanges elements when the first is greater than the second
5. **Loop Control**: Uses counters and conditional jumps to manage iteration

## Key Assembly Concepts Demonstrated

- **Function calling conventions** with stack management
- **Conditional branching** (`jz`, `jle`, `jmp`)
- **Memory operations** (`movl`, `addl`)
- **Register usage** for counters and data manipulation
- **System calls** for program termination

The algorithm has O(n²) time complexity, which is typical for bubble sort.

