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
    movl $0, %eax               # Initialize sum to 0
    movl $array, %edi           # Load array address into EDI

sum_loop:
    addl (%edi), %eax           # Add current array element to sum
    addl $4, %edi               # Move to next integer (4 bytes)
    decl %ecx                   # Decrement counter
    jnz sum_loop                # Jump if not zero (continue loop)

    # At this point, %eax contains the sum
    # Exit program
    movl $1, %eax               # sys_exit
    movl $0, %ebx               # exit status
    int $0x80                   # system call
```

## Algorithm Explanation:

1. **Data Section**: Defines an array of 5 integers and its size
2. **Initialization**: 
   - Load array size into ECX (loop counter)
   - Initialize sum accumulator in EAX to 0
   - Load array address into EDI
3. **Loop Logic**:
   - Add current array element to sum
   - Move to next element (4 bytes forward)
   - Decrement counter and continue if not zero
4. **Result**: Final sum is stored in EAX register

## Key Assembly Concepts Demonstrated:

- **Data declaration** with `.data` and `.text` sections
- **Register usage** (EAX, ECX, EDI)
- **Memory addressing** (`(%edi)` syntax)
- **Loop control** with conditional jumps (`jnz`)
- **System calls** for program termination

This algorithm has O(n) time complexity where n is the array size.

