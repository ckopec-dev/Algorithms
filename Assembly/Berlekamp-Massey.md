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
    movl    $1, %esi           # inner loop counter (start at 1)
    
outer_loop:
    cmpl    $1, %edx           # if outer counter <= 1, exit
    jle     end_sort
    
    movl    %ecx, %edi         # reset inner counter to array_size
    movl    $1, %esi           # reset inner counter
    
inner_loop:
    cmpl    %edi, %esi         # if inner counter >= array_size, exit inner loop
    jge     inner_loop_end
    
    # Compare adjacent elements
    movl    (%eax,%esi,4), %ebx    # load array[i]
    movl    -4(%eax,%esi,4), %edi  # load array[i-1]
    
    cmpl    %ebx, %edi             # compare array[i-1] and array[i]
    jle     skip_swap              # if array[i-1] <= array[i], no swap needed
    
    # Swap elements
    movl    %ebx, -4(%eax,%esi,4)  # array[i-1] = array[i]
    movl    %edi, (%eax,%esi,4)    # array[i] = array[i-1]
    
skip_swap:
    incl    %esi                 # increment inner counter
    jmp     inner_loop
    
inner_loop_end:
    decl    %edx                 # decrement outer counter
    jmp     outer_loop
    
end_sort:
    movl    %ebp, %esp
    popl    %ebp
    ret

_start:
    # Call bubble_sort function
    movl    $array, %eax
    movl    $array_size, %ecx
    call    bubble_sort
    
    # Exit program
    movl    $1, %eax           # sys_exit
    movl    $0, %ebx           # exit status
    int     $0x80
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmpl` instruction to compare values
4. **Swapping**: Uses `movl` instructions to exchange element values
5. **Loop Control**: Uses conditional jumps (`jle`, `jge`) to control loop execution

## Key Assembly Concepts Demonstrated:

- **Function calls** with stack management
- **Register usage** for variables and calculations
- **Memory addressing** with base + index + scale
- **Conditional branching** with jump instructions
- **Loop control** using counters and comparisons

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

