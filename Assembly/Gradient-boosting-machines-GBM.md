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
    movl    $1, %esi           # inner loop counter
    
outer_loop:
    cmpl    $1, %edx           # if size <= 1, exit
    jle     end_sort
    
    movl    %ecx, %edi         # reset inner loop counter
    
inner_loop:
    cmpl    $1, %edi           # if inner counter <= 1, exit inner loop
    jle     outer_loop
    
    movl    (%eax), %ebx       # load current element
    movl    4(%eax), %edi      # load next element
    
    cmpl    %edi, %ebx         # compare elements
    jle     continue           # if current <= next, continue
    
    # swap elements
    movl    %edi, (%eax)       # store next element in current position
    movl    %ebx, 4(%eax)      # store current element in next position
    
continue:
    addl    $4, %eax           # move to next element
    decl    %edi               # decrement inner counter
    jmp     inner_loop
    
end_sort:
    movl    %ebp, %esp
    popl    %ebp
    ret

_start:
    movl    array, %eax
    movl    array_size, %ecx
    call    bubble_sort
    
    # Exit program
    movl    $1, %eax           # sys_exit
    movl    $0, %ebx           # exit status
    int     $0x80
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine if elements need swapping
4. **Swapping**: Direct memory operations to exchange element values
5. **Termination**: The algorithm stops when no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Direct and offset addressing modes
- **Conditional jumps**: `jle`, `jmp` for loop control
- **Function calling**: Stack management and parameter passing
- **System calls**: Using `int $0x80` for program termination

The algorithm has O(n²) time complexity, typical for bubble sort, and operates by repeatedly stepping through the list, comparing adjacent elements and swapping them if they are in the wrong order.

