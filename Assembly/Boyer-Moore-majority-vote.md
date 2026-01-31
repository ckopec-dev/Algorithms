# Assembly Algorithm Example: Finding Maximum Value in an Array

Here's an example of an Assembly algorithm that finds the maximum value in an array:

```assembly
.section .data
    array:      .long 3, 7, 2, 9, 1, 5, 8, 4    # Array of integers
    array_size: .long 8                         # Size of array

.section .text
    .global _start

_start:
    # Initialize registers
    movl array_size, %ecx       # Load array size into ECX (loop counter)
    movl array, %eax            # Load first element as initial maximum
    movl array, %edi            # Load address of array into EDI
    
    # Loop through array starting from second element
    movl %edi, %esi             # Copy array address to ESI
    addl $4, %esi               # Move to second element (4 bytes per int)
    
loop_start:
    cmpl %eax, %esi             # Compare current element with max
    jle skip_update             # Jump if current <= max
    movl %esi, %eax             # Update max with current element
    
skip_update:
    addl $4, %esi               # Move to next element
    decl %ecx                   # Decrement counter
    jnz loop_start              # Continue if not zero
    
    # At this point, %eax contains the maximum value
    # Exit program
    movl $1, %eax               # sys_exit
    movl $0, %ebx               # exit status
    int $0x80                   # system call
```

## Algorithm Explanation:

1. **Initialization**: Load array size and first element into registers
2. **Loop**: Iterate through remaining array elements
3. **Comparison**: Compare each element with current maximum
4. **Update**: Replace maximum if current element is larger
5. **Termination**: Return the maximum value found

## Key Assembly Concepts Demonstrated:

- **Data Section**: Storing array and size
- **Register Usage**: EAX (max), ECX (counter), EDI (array base), ESI (current element)
- **Control Flow**: Jumps and conditional execution
- **Memory Access**: Loading values from memory addresses
- **System Calls**: Program termination

This algorithm has O(n) time complexity and finds the maximum value in the array efficiently.

