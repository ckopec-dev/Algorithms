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
    
    movl    %edx, %edi         # j = n-1
    movl    $0, %ebx           # swapped = false
    
inner_loop:
    cmpl    $0, %edi           # if (j <= 0) break
    jle     inner_end
    
    decl    %edi               # j--
    
    movl    (%eax,%edi,4), %edi # temp = array[j]
    movl    -4(%eax,%edi,4), %ebp # temp2 = array[j-1]
    
    cmpl    %edi, %ebp         # if (array[j-1] > array[j])
    jle     inner_continue
    
    # swap elements
    movl    %edi, -4(%eax,%edi,4) # array[j-1] = array[j]
    movl    %ebp, (%eax,%edi,4)   # array[j] = array[j-1]
    movl    $1, %ebx           # swapped = true
    
inner_continue:
    jmp     inner_loop
    
inner_end:
    cmpl    $0, %ebx           # if (!swapped) break
    jz      end_sort
    
    decl    %edx               # n--
    jmp     outer_loop
    
end_sort:
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

## Algorithm Explanation

This bubble sort implementation:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Optimization**: Uses a `swapped` flag to detect when the array is sorted
4. **Memory Access**: Uses indexed addressing to access array elements

## Key Assembly Concepts Shown

- **Function Calls**: Using `call` and `ret` instructions
- **Stack Management**: Pushing/popping registers
- **Conditional Jumps**: `jle`, `jz`, `jmp` for control flow
- **Memory Operations**: Loading/storing with indexed addressing
- **Register Usage**: Proper use of EAX, EBX, ECX, EDX, ESI, EDI

The algorithm has O(n²) time complexity and sorts the array in ascending order.

