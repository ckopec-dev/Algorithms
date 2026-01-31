# Assembly Algorithm Example: Bubble Sort

Here's an example of a bubble sort algorithm implemented in x86 Assembly language:

```assembly
.section .data
    array:      .long 64, 34, 25, 12, 22, 11, 90
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
    jle sort_done
    
    movl $0, %edi                  # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner index with array size
    jge inner_done
    
    # Load two adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX
    addl $4, %ebx                  # Load array[i+1] into EBX
    
    # Compare elements
    cmp %ebx, %eax                 # Compare array[i] and array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    incl %edi                      # Increment inner loop index
    jmp inner_loop
    
inner_done:
    decl %ecx                      # Decrement outer loop counter
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

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Swapping**: Uses temporary registers to exchange values
4. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Register usage**: EAX, EBX, ECX, EDI, ESI for data manipulation
- **Memory addressing**: Array access using base + index + scale
- **Control flow**: Conditional jumps (cmp, jle, jge, jmp)
- **Function calling**: Stack management with push/pop
- **System calls**: Exit routine using int $0x80

This example shows how high-level algorithmic concepts are translated into low-level assembly instructions.

