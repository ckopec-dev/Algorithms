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
    
    movl array_size(%ebp), %ecx    # Load array size into ECX (outer loop counter)
    movl $0, %esi                  # Initialize outer loop index
    
outer_loop:
    cmp $0, %ecx                   # Check if we've done all passes
    jle end_sort                   # If ECX <= 0, we're done
    
    movl $0, %edi                  # Initialize inner loop index
    movl %ecx, %edx                # Copy array size to EDX for inner loop
    
inner_loop:
    cmp %edi, %edx                 # Compare inner index with array size
    jge inner_loop_end             # If index >= size, exit inner loop
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX (for comparison)
    addl $4, %ebx                  # Point to array[i+1]
    movl (%ebx), %ebx              # Load array[i+1] into EBX
    
    cmp %ebx, %eax                 # Compare array[i] and array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4), %ebx # Store array[i] at array[i+1]
    
no_swap:
    addl $1, %edi                  # Increment inner loop index
    jmp inner_loop                 # Continue inner loop
    
inner_loop_end:
    dec %ecx                       # Decrement outer loop counter
    jmp outer_loop                 # Continue outer loop
    
end_sort:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80                      # system call
```

## Algorithm Explanation

This bubble sort implementation:
1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmp` instruction to compare elements
4. **Swapping**: Exchanges values when elements are out of order
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated

- **Register usage**: EAX, EBX, ECX, EDX, ESI, EDI for various operations
- **Memory addressing**: Array access using base+offset addressing
- **Control flow**: Conditional jumps (cmp, jle, jge, jmp)
- **Function calling**: Stack management with push/pop
- **System calls**: Exit routine using int $0x80

This example shows how high-level algorithm concepts are translated into low-level assembly instructions.

