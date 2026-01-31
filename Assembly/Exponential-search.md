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
    
    movl array_size(%ebp), %ecx        # Load array size into ECX (outer loop counter)
    movl $0, %esi                      # Initialize outer loop counter
    
outer_loop:
    cmp $0, %ecx                       # Check if we've completed all passes
    je end_sort
    
    movl $0, %edi                      # Initialize inner loop counter
    
inner_loop:
    cmp %edi, %ecx                     # Compare inner counter with array size
    je inner_loop_end
    
    movl array(,%edi,4), %eax         # Load current element
    movl array(,%edi,4), %ebx         # Load current element (copy)
    addl $4, %ebx                     # Load next element
    
    cmp %ebx, %eax                    # Compare current with next element
    jle no_swap                       # Jump if current <= next
    
    # Swap elements
    movl %ebx, %edx                   # Store next element in temp
    movl %eax, array(,%edi,4)         # Store current element at next position
    movl %edx, array(,%edi,4)         # Store next element at current position
    
no_swap:
    inc %edi                           # Increment inner counter
    jmp inner_loop                     # Continue inner loop
    
inner_loop_end:
    dec %ecx                           # Decrement outer counter
    jmp outer_loop                     # Continue outer loop
    
end_sort:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                      # sys_exit
    movl $0, %ebx                      # exit status
    int $0x80
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Swapping**: Uses temporary registers to exchange element values
4. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Register usage**: EAX, EBX, ECX, EDX, ESI, EDI for various operations
- **Memory addressing**: Array access using base+offset addressing
- **Control flow**: Conditional jumps (je, jle) for loop and comparison logic
- **Function calling**: Stack management for subroutine calls
- **System calls**: Exit syscall for program termination

The algorithm has O(n²) time complexity, which is typical for bubble sort, but implemented in low-level assembly instructions.

