# Assembly Algorithm Example: Finding Maximum Value in an Array

Here's an example of an Assembly algorithm that finds the maximum value in an array:

```assembly
.section .data
    array:      .long 10, 25, 3, 47, 15, 8, 92, 6
    array_size: .long 8

.section .text
    .global _start

find_max:
    # Function to find maximum value in array
    # Input: r0 = array address, r1 = array size
    # Output: r0 = maximum value
    
    mov r2, #0          @ index counter
    ldr r3, [r0]        @ load first element as initial max
    mov r4, r3          @ r4 = current_max
    
loop:
    cmp r2, r1          @ compare index with array size
    bge end_loop        @ if index >= size, exit loop
    
    ldr r5, [r0, r2, lsl #2] @ load array[index] (multiply by 4 for int size)
    
    cmp r5, r4          @ compare current element with max
    movlt r4, r5        @ if element > max, update max
    
    add r2, r2, #1      @ increment index
    b loop              @ repeat loop

end_loop:
    mov r0, r4          @ return max value in r0
    bx lr               @ return from function

_start:
    # Main program
    ldr r0, =array      @ load array address
    ldr r1, =array_size @ load array size
    bl find_max         @ call find_max function
    
    # Exit program
    mov r7, #1          @ sys_exit
    mov r0, #0          @ exit status
    swi 0               @ system call
```

## Algorithm Explanation:

1. **Initialization**: Load the first array element as the initial maximum value
2. **Loop**: Iterate through each element of the array
3. **Comparison**: Compare each element with the current maximum
4. **Update**: If a larger element is found, update the maximum
5. **Return**: Return the maximum value found

## Key Assembly Concepts Demonstrated:

- **Data section**: Storing array data and size
- **Function calls**: Using `bl` for branching and `bx lr` for return
- **Memory addressing**: Loading values from array using base + offset
- **Conditional branching**: Using `cmp` and `b` instructions
- **Register operations**: Moving, loading, and comparing values

This algorithm has O(n) time complexity and finds the maximum value in an array of integers.

