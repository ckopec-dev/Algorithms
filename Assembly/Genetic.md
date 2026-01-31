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
    decl    %esi               # decrement for inner loop
    
inner_loop:
    testl   %esi, %esi         # check if inner counter is zero
    jz      outer_dec          # if zero, go to next outer iteration
    
    movl    (%eax), %ebx       # load current element
    movl    4(%eax), %edi      # load next element
    
    cmpl    %edi, %ebx         # compare current with next
    jle     skip_swap          # if current <= next, no swap needed
    
    # Swap elements
    movl    %edi, (%eax)       # store next element in current position
    movl    %ebx, 4(%eax)      # store current element in next position
    
skip_swap:
    addl    $4, %eax           # move to next element
    decl    %esi               # decrement inner counter
    jmp     inner_loop         # continue inner loop
    
outer_dec:
    decl    %edx               # decrement outer counter
    movl    array, %eax        # reset array pointer
    jmp     outer_loop         # continue outer loop
    
sort_done:
    popl    %ebp
    ret

_start:
    # Call bubble_sort function
    movl    $array, %eax       # array address
    movl    array_size, %ecx   # array size
    pushl   %ecx               # push size
    pushl   %eax               # push array address
    call    bubble_sort        # call sorting function
    addl    $8, %esp           # clean up stack
    
    # Exit program
    movl    $1, %eax           # sys_exit
    movl    $0, %ebx           # exit status
    int     $0x80              # system call
```

## Algorithm Explanation

This bubble sort implementation:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses `cmpl` instruction to compare elements
4. **Swapping**: Exchanges adjacent elements when they're in wrong order
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Shown

- **Function calls** with stack management
- **Loop control** using conditional jumps
- **Memory operations** with pointers
- **Register usage** for temporary storage
- **System calls** for program termination

The algorithm sorts the array `[64, 34, 25, 12, 22, 11, 90]` in ascending order.

