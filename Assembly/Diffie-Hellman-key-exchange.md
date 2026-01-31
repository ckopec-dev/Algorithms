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
    cmp $0, %ecx                   # Check if array size is 0
    je end_sort                    # If size is 0, exit
    
    movl $0, %edi                  # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner index with array size
    jge end_inner                  # If inner >= size, exit inner loop
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX (for comparison)
    movl array(,%edi,4), %ebx      # Load array[i+1] into EBX
    
    # Compare and swap if needed
    cmp %ebx, %eax                 # Compare array[i] and array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    incl %edi                      # Increment inner loop counter
    jmp inner_loop                 # Continue inner loop
    
end_inner:
    decl %ecx                      # Decrement outer loop counter
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

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses conditional jumps to determine if swapping is needed
4. **Swapping**: Direct memory operations to exchange element values

## Key Assembly Concepts Demonstrated

- **Register usage**: EAX, EBX, ECX, EDI, ESI for various operations
- **Memory addressing**: Array elements accessed using base + index + scale
- **Control flow**: Conditional jumps (je, jg, jl) for loop and condition logic
- **Function calling**: Stack management for procedure calls
- **System calls**: Exit routine using interrupt

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

