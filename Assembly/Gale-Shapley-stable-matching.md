# Assembly Algorithm Example: Finding Maximum Value in an Array

Here's an example of an Assembly algorithm that finds the maximum value in an array using x86-64 assembly language:

```assembly
.section .data
    array:      .long 3, 7, 2, 9, 1, 5, 8, 4
    array_size: .long 8

.section .text
    .global _start

find_max:
    # Function to find maximum value in array
    # Input: RDI = array address, RSI = array size
    # Output: RAX = maximum value
    
    mov rax, [rdi]          # Load first element as initial max
    dec rsi                 # Decrement counter (we already loaded first element)
    jz done                 # If only one element, we're done
    
loop_start:
    cmp rsi, 0              # Check if we've processed all elements
    jz done                 # If yes, exit loop
    
    mov rbx, [rdi]          # Load current array element
    cmp rbx, rax            # Compare with current max
    jle skip_update         # If current element <= max, skip update
    
    mov rax, rbx            # Update max with current element
    
skip_update:
    add rdi, 4              # Move to next array element (4 bytes)
    dec rsi                 # Decrement counter
    jmp loop_start          # Continue loop
    
done:
    ret                     # Return maximum value in RAX

_start:
    # Initialize registers
    mov rdi, array          # Load array address
    mov rsi, array_size     # Load array size
    call find_max           # Call find_max function
    
    # Exit program
    mov rax, 60             # sys_exit
    mov rdi, 0              # exit status
    syscall
```

## Algorithm Explanation:

1. **Input**: Takes an array address in RDI and array size in RSI
2. **Initialization**: Sets first array element as initial maximum value
3. **Loop**: Iterates through remaining elements comparing each with current maximum
4. **Comparison**: Updates maximum if current element is larger
5. **Output**: Returns maximum value in RAX register

## Key Assembly Concepts Demonstrated:

- **Register usage**: RDI, RSI, RAX, RBX for data manipulation
- **Memory operations**: Loading/storing values with `[register]`
- **Control flow**: Conditional jumps (cmp, jz, jle, jmp)
- **Function calling**: Stack management and return mechanism
- **Arithmetic operations**: Addition, subtraction, comparison

This algorithm has O(n) time complexity and demonstrates fundamental assembly programming techniques.

