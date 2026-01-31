# Assembly Algorithm Example: Finding Maximum Value in an Array

Here's an example of an Assembly algorithm that finds the maximum value in an array using x86-64 assembly language:

```assembly
.section .data
    array:      .long 10, 25, 3, 47, 15, 8, 92, 6
    array_size: .long 8

.section .text
    .global _start

find_max:
    # Function to find maximum value in array
    # Parameters: rdi = array address, rsi = array size
    # Returns: rax = maximum value
    
    mov rax, [rdi]          # Load first element as initial max
    mov rcx, 1              # Initialize counter (start from index 1)
    
loop_start:
    cmp rcx, rsi            # Compare counter with array size
    jge loop_end            # If counter >= size, exit loop
    
    mov rdx, [rdi + rcx*4]  # Load current array element
    cmp rdx, rax            # Compare with current max
    jle skip_update         # If current element <= max, skip
    
    mov rax, rdx            # Update max with current element
    
skip_update:
    inc rcx                 # Increment counter
    jmp loop_start          # Continue loop
    
loop_end:
    ret                     # Return max value in rax

_start:
    # Main program entry point
    mov rdi, array          # Load array address
    mov rsi, array_size     # Load array size
    call find_max           # Call find_max function
    
    # Exit program (Linux system call)
    mov rax, 60             # sys_exit
    mov rdi, 0              # exit status
    syscall
```

## Algorithm Explanation:

1. **Initialization**: Load the first array element as the initial maximum value
2. **Loop**: Iterate through remaining elements starting from index 1
3. **Comparison**: Compare each element with current maximum
4. **Update**: If current element is larger, update the maximum
5. **Repeat**: Continue until all elements are processed
6. **Return**: Maximum value is returned in register rax

## Key Assembly Concepts Demonstrated:

- **Registers**: rax, rdi, rsi, rcx, rdx
- **Memory addressing**: `[rdi + rcx*4]` for array access
- **Control flow**: cmp, jge, jmp, jle
- **Function calling**: call/ret mechanism
- **System calls**: Linux sys_exit

This algorithm has O(n) time complexity and finds the maximum value in the given array.

