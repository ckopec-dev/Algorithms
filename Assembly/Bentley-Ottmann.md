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
    jle sort_done
    
    movl $0, %esi                  # Initialize inner loop counter j = 0
    movl %ecx, %edx                # Copy array_size to edx for comparison
    
inner_loop:
    cmp %esi, %edx                 # Compare j < array_size - i - 1
    jge inner_done
    
    # Compare adjacent elements
    movl array(,%esi,4), %eax      # Load array[j]
    movl array(,%esi,4), %ebx      # Load array[j] (copy)
    addl $4, %esi                  # Increment j
    movl array(,%esi,4), %ebx      # Load array[j+1]
    
    cmp %ebx, %eax                 # Compare array[j] and array[j+1]
    jle no_swap                    # If array[j] <= array[j+1], no swap needed
    
    # Swap elements
    movl %ebx, array(,%esi,4)      # array[j+1] = array[j]
    movl %eax, array-4(,%esi,4)    # array[j] = array[j+1] (previous value)
    
no_swap:
    jmp inner_loop
    
inner_done:
    dec %ecx                       # Decrement outer loop counter
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

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Comparison**: Uses conditional jumps to determine if swapping is needed
4. **Swapping**: Direct memory operations to exchange array elements

## Key Assembly Concepts Demonstrated

- **Register usage**: `%eax`, `%ebx`, `%ecx`, `%edx`, `%esi`, `%edi`
- **Memory addressing**: Array elements accessed via base+offset addressing
- **Conditional jumps**: `cmp`, `jle`, `jge` for loop control
- **Function calls**: Stack management with `push`/`pop`
- **System calls**: `int $0x80` for program termination

The algorithm has O(n²) time complexity, typical for bubble sort, and sorts the array in ascending order.

