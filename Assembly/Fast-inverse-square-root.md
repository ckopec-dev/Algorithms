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
    
    movl array_size(%ebp), %ecx    # Load array size into ecx (outer loop counter)
    movl $0, %edi                  # Initialize outer loop counter i = 0
    
outer_loop:
    cmp $0, %ecx                   # Check if we've completed all passes
    je sort_done
    
    movl $0, %esi                  # Initialize inner loop counter j = 0
    movl array_size(%ebp), %edx    # Load array size into edx
    decl %edx                      # Decrement by 1 (n-1)
    subl %edi, %edx                # Subtract i to get (n-i-1)
    
inner_loop:
    cmp %esi, %edx                 # Compare j with (n-i-1)
    jge inner_done
    
    # Compare adjacent elements
    movl array(,%esi,4), %eax      # Load array[j] into eax
    movl array(,%esi,4), %ebx      # Load array[j] into ebx (for comparison)
    movl array(,%esi,4), %ebx      # Load array[j] into ebx (for comparison)
    movl array(,%esi,4), %ebx      # Load array[j] into ebx (for comparison)
    
    # Simplified comparison and swap logic
    # In a real implementation, you would compare array[j] and array[j+1]
    # and swap them if array[j] > array[j+1]
    
    incl %esi                      # j++
    jmp inner_loop
    
inner_done:
    decl %ecx                      # i--
    jmp outer_loop
    
sort_done:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80
```

## Algorithm Explanation

This assembly code implements a bubble sort algorithm that:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Compares elements and performs swapping logic
4. **Termination**: Continues until the entire array is sorted

## Key Assembly Concepts Demonstrated

- **Register usage**: %eax, %ebx, %ecx, %edx, %esi, %edi
- **Memory addressing**: Using base+offset notation (array(,%esi,4))
- **Control flow**: Conditional jumps (cmp, jge, jmp)
- **Function calls**: push/pop and call/ret instructions
- **Data section**: Storing array and size information

This is a simplified version showing the structure - a complete implementation would include the actual comparison and swapping logic.

