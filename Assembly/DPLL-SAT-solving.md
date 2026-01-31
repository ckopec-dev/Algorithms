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
    movl $0, %esi                  # Initialize outer loop counter
    
outer_loop:
    cmp $0, %ecx                   # Check if we've completed all passes
    jle end_sort
    
    movl $0, %edi                  # Initialize inner loop counter
    
inner_loop:
    cmp %edi, %ecx                 # Compare inner counter with array size
    jge outer_loop_end
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i]
    movl array(,%edi,4), %ebx      # Load array[i] (duplicate for comparison)
    movl array(,%edi,4), %ebx      # Load array[i+1]
    addl $4, %edi                  # Increment inner counter
    
    # Compare and swap if needed
    cmpl %ebx, %eax                # Compare array[i] and array[i+1]
    jle no_swap                    # Jump if array[i] <= array[i+1]
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # Store array[i+1] at array[i]
    movl %eax, array(,%edi,4)      # Store array[i] at array[i+1]
    
no_swap:
    jmp inner_loop                 # Continue inner loop
    
outer_loop_end:
    decl %ecx                      # Decrement outer counter
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

## Key Features of This Assembly Algorithm:

1. **Data Section**: Contains the array to be sorted and its size
2. **Function Structure**: Uses proper stack frame setup and cleanup
3. **Loop Control**: Implements nested loops for bubble sort algorithm
4. **Memory Operations**: Loads and stores array elements
5. **Conditional Logic**: Uses comparisons and jumps for sorting decisions
6. **System Calls**: Includes proper program termination

## Algorithm Explanation:
- **Outer Loop**: Controls the number of passes (n-1 passes for n elements)
- **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
- **Comparison**: Uses `cmpl` instruction to compare values
- **Jump Instructions**: Control flow based on comparison results

This example demonstrates fundamental assembly programming concepts including:
- Register usage
- Memory addressing
- Loop structures
- Conditional branching
- Function calling conventions

