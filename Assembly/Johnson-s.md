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
    cmp $0, %ecx                   # Check if we've completed all passes
    je sort_done
    
    movl $0, %edi                  # Initialize inner loop index
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner index with array size
    jge inner_loop_end
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX
    movl array(,%edi,4), %edx      # Load array[i] into EDX
    
    # Compare array[i] and array[i+1]
    addl $4, %edi                  # Increment inner index
    movl array(,%edi,4), %ebx      # Load array[i+1] into EBX
    
    cmpl %ebx, %eax                # Compare elements
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    jmp inner_loop                 # Continue inner loop
    
inner_loop_end:
    dec %ecx                       # Decrement outer loop counter
    jmp outer_loop                 # Continue outer loop
    
sort_done:
    pop %ebp
    ret

_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80                      # system call
```

## Algorithm Explanation:

This bubble sort implementation:
1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses `cmpl` instruction to compare elements
4. **Swapping**: Exchanges elements when the condition is met
5. **Termination**: Continues until no more swaps are needed

## Key Assembly Concepts Demonstrated:

- **Register usage**: EAX, EBX, ECX, EDX, ESI, EDI
- **Memory addressing**: Array elements accessed with base + index scaling
- **Control flow**: Conditional jumps (je, jg, jl, jmp)
- **Function calling**: Stack management with push/pop
- **System calls**: Exit routine using int $0x80

The algorithm sorts an array of integers in ascending order using the classic bubble sort technique.

