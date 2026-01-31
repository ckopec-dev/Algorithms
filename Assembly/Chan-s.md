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
    
    movl    %ecx, %edx         # save size in edx
    decl    %edx               # edx = size - 1
    
    # Inner loop - j = 0 to size-2
inner_loop:
    cmpl    $0, %edx           # if j < 0, break
    jle     inner_done
    
    # Compare array[j] and array[j+1]
    movl    (%eax), %esi       # load array[j]
    movl    4(%eax), %edi      # load array[j+1]
    
    cmpl    %edi, %esi         # compare elements
    jle     skip_swap          # if array[j] <= array[j+1], skip
    
    # Swap elements
    movl    %edi, (%eax)       # array[j] = array[j+1]
    movl    %esi, 4(%eax)      # array[j+1] = array[j]
    
skip_swap:
    addl    $4, %eax           # move to next element
    decl    %edx               # j--
    jmp     inner_loop
    
inner_done:
    decl    %ecx               # decrement size
    jmp     outer_loop
    
done:
    popl    %ebp
    ret

_start:
    # Call bubble sort
    movl    $array, %eax       # array address
    movl    array_size, %ecx   # array size
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
4. **Swapping**: Exchanges values using memory operations
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Function calls** with stack management
- **Loop control** using conditional jumps
- **Memory addressing** and pointer arithmetic
- **Register usage** for temporary storage
- **Comparison and branching** instructions
- **System calls** for program termination

This example shows how high-level algorithmic concepts are implemented using low-level assembly instructions and concepts.

