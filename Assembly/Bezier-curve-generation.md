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
    
    # Outer loop - i = 0 to n-2
outer_loop:
    cmpl    $1, %ecx           # if size <= 1, done
    jle     done
    
    movl    %ecx, %edx         # edx = size
    decl    %edx               # edx = size - 1
    
    # Inner loop - j = 0 to n-2-i
inner_loop:
    cmpl    $0, %edx           # if j >= size-1, break
    jle     inner_done
    
    movl    (%eax), %ebx       # load array[j]
    movl    4(%eax), %edi      # load array[j+1]
    
    cmpl    %edi, %ebx         # compare array[j] and array[j+1]
    jle     skip_swap          # if array[j] <= array[j+1], no swap
    
    # Swap elements
    movl    %edi, (%eax)       # array[j] = array[j+1]
    movl    %ebx, 4(%eax)      # array[j+1] = array[j]
    
skip_swap:
    addl    $4, %eax           # array++
    decl    %edx               # j--
    jmp     inner_loop
    
inner_done:
    decl    %ecx               # size--
    jmp     outer_loop
    
done:
    popl    %ebp
    ret

_start:
    # Call bubble_sort with array and size
    movl    $array, %eax
    movl    $array_size, %ebx
    pushl   %ebx
    pushl   %eax
    call    bubble_sort
    addl    $8, %esp           # clean up stack
    
    # Exit program
    movl    $1, %eax           # sys_exit
    movl    $0, %ebx           # exit status
    int     $0x80
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses conditional jumps to determine if swapping is needed
4. **Swapping**: Direct memory operations to exchange array elements
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Function calling convention** with stack management
- **Conditional branching** using `jle`, `jmp`
- **Memory addressing** and pointer manipulation
- **Register usage** for temporary storage
- **Loop control** with decrement and comparison operations

The algorithm has O(n²) time complexity, which is typical for bubble sort, but implemented at the assembly level with explicit memory operations and control flow.

