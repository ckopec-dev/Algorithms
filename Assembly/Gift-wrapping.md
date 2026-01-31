# Assembly Algorithm Example: Sum of Array Elements

Here's an example of an assembly algorithm that calculates the sum of elements in an array:

```assembly
.data
    array:      .word 10, 20, 30, 40, 50    # Array of 5 integers
    array_size: .word 5                     # Size of array
    sum:        .word 0                     # Variable to store sum

.text
.globl _start

_start:
    # Load base address of array
    la $t0, array           # $t0 = address of array
    
    # Load array size
    lw $t1, array_size      # $t1 = size of array (5)
    
    # Initialize sum and counter
    li $t2, 0               # $t2 = sum = 0
    li $t3, 0               # $t3 = counter = 0
    
loop:
    # Check if counter >= array_size
    bge $t3, $t1, end_loop  # if counter >= size, exit loop
    
    # Load array element
    sll $t4, $t3, 2         # $t4 = counter * 4 (word size)
    add $t5, $t0, $t4       # $t5 = address of array[counter]
    lw $t6, 0($t5)          # $t6 = array[counter]
    
    # Add to sum
    add $t2, $t2, $t6       # sum = sum + array[counter]
    
    # Increment counter
    addi $t3, $t3, 1        # counter++
    
    # Jump back to loop
    j loop
    
end_loop:
    # Store result
    sw $t2, sum             # store sum in memory
    
    # Exit program
    li $v0, 10              # system call for exit
    syscall
```

## Algorithm Explanation:

1. **Data Section**: Defines the array and variables
2. **Initialization**: Sets up pointers and counters
3. **Loop**: 
   - Check if we've processed all elements
   - Calculate memory address of current element
   - Load element value
   - Add to running sum
   - Increment counter
   - Continue loop
4. **Result**: Store final sum and exit

## Key Assembly Concepts Demonstrated:

- **Memory addressing** using load/store instructions
- **Loop control** with conditional branches
- **Arithmetic operations** (add, addi)
- **Address calculation** (shift left for word size)
- **System calls** for program termination

This algorithm has O(n) time complexity where n is the array size.

