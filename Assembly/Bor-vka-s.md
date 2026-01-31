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
    movl    $0, %esi           # i = 0
    
outer_loop:
    cmpl    $1, %edx           # if (n <= 1) return
    jle     end_sort
    
    movl    %edx, %edi         # inner loop counter = n-1
    movl    $0, %ebx           # j = 0
    
inner_loop:
    cmpl    %edi, %ebx         # if (j >= n-1) break
    jge     end_inner
    
    # Compare array[j] and array[j+1]
    movl    (%eax,%ebx,4), %edi    # load array[j]
    movl    4(%eax,%ebx,4), %ebp   # load array[j+1]
    
    cmpl    %ebp, %edi             # compare array[j] and array[j+1]
    jle     no_swap                  # if (array[j] <= array[j+1]) continue
    
    # Swap elements
    movl    %ebp, (%eax,%ebx,4)    # array[j] = array[j+1]
    movl    %edi, 4(%eax,%ebx,4)   # array[j+1] = array[j]
    
no_swap:
    incl    %ebx                 # j++
    jmp     inner_loop
    
end_inner:
    decl    %edx                 # n--
    jmp     outer_loop
    
end_sort:
    popl    %ebp
    ret

_start:
    # Call bubble sort
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
3. **Comparison**: Uses `cmpl` instruction to compare values
4. **Swapping**: Direct memory manipulation to exchange element values
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Function calls** with stack management
- **Loop control** using conditional jumps
- **Memory addressing** with base + index + scale
- **Register usage** for variables and temporary storage
- **Conditional branching** for algorithm logic
- **System calls** for program termination

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

