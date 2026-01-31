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
    # Input: RDI = array address, RSI = array size
    # Output: RAX = maximum value
    
    mov rax, [rdi]          # Load first element as initial max
    dec rsi                 # Decrement counter (we already loaded first element)
    jz done                 # If only one element, we're done
    
    mov rdx, rdi            # Keep original array address in RDX
    add rdx, 4              # Point to next element (4 bytes per int)
    
loop:
    cmp rsi, 0              # Check if we've processed all elements
    jz done                 # If yes, exit loop
    
    mov ecx, [rdx]          # Load current array element
    cmp rcx, rax            # Compare with current max
    jle skip_update         # If current element <= max, skip update
    
    mov rax, rcx            # Update max with current element
    
skip_update:
    add rdx, 4              # Move to next element
    dec rsi                 # Decrement counter
    jmp loop                # Continue loop

done:
    ret                     # Return max value in RAX

_start:
    # Main program entry point
    mov rdi, array          # Load array address
    mov rsi, array_size     # Load array size
    call find_max           # Call find_max function
    
    # Exit program (Linux syscall)
    mov rax, 60             # sys_exit
    mov rdi, 0              # exit status
    syscall
```

## Algorithm Explanation:

1. **Input**: Takes an array address and size as parameters
2. **Initialization**: Sets first array element as initial maximum
3. **Loop**: Iterates through remaining elements comparing each with current maximum
4. **Comparison**: Updates maximum when larger element is found
5. **Output**: Returns maximum value in RAX register

## Key Assembly Concepts Demonstrated:

- **Register usage**: RDI, RSI, RAX, RDX for data manipulation
- **Memory operations**: Loading/storing values with `[register]` syntax
- **Control flow**: Conditional jumps (cmp, jz, jle, jmp)
- **Function calling**: Parameter passing and return values
- **Arithmetic operations**: Addition, subtraction, comparison

This algorithm has O(n) time complexity and demonstrates fundamental assembly programming techniques.

