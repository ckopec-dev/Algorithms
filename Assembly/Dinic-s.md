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
    # Parameters: r0 = array address, r1 = array size
    # Returns: r0 = maximum value
    
    mov r2, #0          @ index counter
    ldr r3, [r0]        @ load first element as initial max
    mov r4, r3          @ r4 = current_max
    
loop:
    cmp r2, r1          @ compare index with size
    bge end_loop        @ if index >= size, exit loop
    
    add r0, r0, #4      @ move to next array element (4 bytes)
    ldr r5, [r0]        @ load current element
    cmp r5, r4          @ compare with current max
    movlt r4, r5        @ if current > max, update max
    
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

1. **Initialization**: Load first array element as initial maximum value
2. **Loop**: Iterate through remaining elements
3. **Comparison**: Compare each element with current maximum
4. **Update**: Replace maximum if current element is larger
5. **Return**: Return the maximum value found

## Key Assembly Concepts Demonstrated:

- **Data Section**: `.data` for storing array and size
- **Text Section**: `.text` for executable code
- **Registers**: Using r0-r4 for various operations
- **Memory Operations**: `ldr` (load), `str` (store)
- **Control Flow**: `cmp`, `b` (branch), `bl` (branch with link)
- **Function Calls**: Using `bl` and `bx lr` for subroutine calls

This algorithm has O(n) time complexity and finds the maximum value in the given array.

