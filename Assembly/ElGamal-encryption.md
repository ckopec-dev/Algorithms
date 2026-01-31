# Assembly Algorithm Example: Sum of Array Elements

Here's an example of an Assembly algorithm that calculates the sum of elements in an array:

```assembly
.section .data
    array:      .long 10, 20, 30, 40, 50    # Array of 5 integers
    array_size: .long 5                     # Size of array

.section .text
    .global _start

_start:
    # Initialize registers
    movl array_size, %ecx       # Load array size into ECX (loop counter)
    movl $0, %eax               # Initialize sum = 0
    movl $array, %edi           # Load array address into EDI (pointer)

loop_start:
    # Check if we've processed all elements
    cmpl $0, %ecx               # Compare counter with 0
    je   loop_end               # If counter = 0, exit loop

    # Add current array element to sum
    addl (%edi), %eax           # Add value at EDI to EAX
    addl $4, %edi               # Move pointer to next integer (4 bytes)
    decl %ecx                   # Decrement counter
    jmp  loop_start             # Jump back to loop start

loop_end:
    # At this point, EAX contains the sum
    # Exit program (Linux system call)
    movl $1, %eax               # sys_exit
    movl %eax, %ebx             # exit status
    int  $0x80                  # system call
```

## Algorithm Explanation:

1. **Initialization**: Load array size into ECX (counter), initialize sum to 0 in EAX, and load array address into EDI
2. **Loop**: 
   - Check if counter is zero (end condition)
   - Add current array element to sum
   - Move pointer to next element
   - Decrement counter and repeat
3. **Result**: EAX contains the sum of all array elements

## Key Assembly Concepts Demonstrated:

- **Data Section**: `.data` section for storing initialized data
- **Text Section**: `.text` section for executable code
- **Register Usage**: EAX (accumulator), ECX (counter), EDI (pointer)
- **Memory Operations**: `movl` for loading data, `addl` for arithmetic
- **Control Flow**: Conditional jumps (`je`, `jmp`)
- **System Calls**: Linux system call interface

This algorithm calculates the sum of `[10, 20, 30, 40, 50]` = `150` and stores the result in EAX register.

