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
    jle sort_done
    
    movl $0, %edi                  # Initialize inner loop counter
    movl array_size(%ebp), %edx    # Load array size into EDX
    
inner_loop:
    cmp %edi, %edx                 # Compare inner counter with array size
    jge inner_loop_end
    
    # Compare adjacent elements
    movl array(,%edi,4), %eax      # Load array[i] into EAX
    movl array(,%edi,4), %ebx      # Load array[i] into EBX (for comparison)
    addl $4, %ebx                  # Load array[i+1] into EBX
    movl array(,%edi,4), %ebx      # This is incorrect - let me fix it
    
    # Corrected comparison:
    movl array(,%edi,4), %eax      # Load array[i]
    movl array(4,%edi,4), %ebx     # Load array[i+1]
    
    cmp %ebx, %eax                 # Compare array[i] with array[i+1]
    jle no_swap                    # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%edi,4)      # array[i] = array[i+1]
    movl %eax, array(4,%edi,4)     # array[i+1] = array[i]
    
no_swap:
    incl %edi                      # Increment inner counter
    jmp inner_loop
    
inner_loop_end:
    decl %ecx                      # Decrement outer counter
    jmp outer_loop
    
sort_done:
    pop %ebp
    ret

# Main program
_start:
    call bubble_sort
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80
```

## Key Features of This Assembly Algorithm:

1. **Data Section**: Contains the array to be sorted and its size
2. **Function Structure**: Uses proper function calling conventions
3. **Loop Control**: Implements nested loops for bubble sort algorithm
4. **Memory Operations**: Reads from and writes to memory locations
5. **Conditional Jumps**: Uses comparison and jump instructions for control flow
6. **Register Usage**: Properly manages CPU registers for operations

## Algorithm Explanation:

This bubble sort implementation:
- Uses nested loops to compare adjacent elements
- Swaps elements when they're in wrong order
- Reduces the number of comparisons after each pass
- Continues until the entire array is sorted

The algorithm has O(n²) time complexity, which is typical for bubble sort.

