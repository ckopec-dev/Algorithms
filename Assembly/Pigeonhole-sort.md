# Pigeonhole Sort in Assembly Language

Here's an example implementation of Pigeonhole Sort algorithm in x86-64 Assembly language:

```assembly
.section .data
    # Sample array to sort
    array: .long 4, 2, 5, 1, 3, 8, 6, 7
    array_size: .long 8

.section .text
    .global _start

pigeonhole_sort:
    # Function to sort array using pigeonhole sort
    # Input: array address in RDI, array size in RSI
    # Output: sorted array in place
    
    push rbp
    mov rbp, rsp
    
    # Calculate min and max values
    mov r8, rdi          # r8 = array address
    mov r9, rsi          # r9 = array size
    mov eax, [r8]        # eax = first element (min/max)
    mov ecx, eax         # ecx = min
    mov edx, eax         # edx = max
    
    # Find min and max
    mov r10, 1           # i = 1
find_min_max_loop:
    cmp r10, r9
    jge find_min_max_done
    
    mov eax, [r8 + r10 * 4]  # load array[i]
    
    cmp eax, ecx
    jge check_max
    mov ecx, eax           # update min
    
check_max:
    cmp eax, edx
    jle min_max_continue
    mov edx, eax           # update max
    
min_max_continue:
    inc r10
    jmp find_min_max_loop
    
find_min_max_done:
    # Calculate range
    mov r10, edx         # r10 = max
    sub r10, ecx         # r10 = max - min
    inc r10              # r10 = range + 1 (for inclusive)
    
    # Allocate pigeonhole array (range elements)
    mov rax, r10
    mov rdi, rax
    call malloc          # rax = pigeonhole array address
    
    # Initialize pigeonhole array to 0
    mov r11, rax         # r11 = pigeonhole array
    mov r12, 0           # i = 0
init_loop:
    cmp r12, r10
    jge init_done
    
    mov [r11 + r12 * 4], 0
    inc r12
    jmp init_loop
    
init_done:
    # Fill pigeonholes
    mov r12, 0           # i = 0
fill_loop:
    cmp r12, r9
    jge fill_done
    
    mov eax, [r8 + r12 * 4]  # load array[i]
    sub eax, ecx             # offset = array[i] - min
    mov edx, [r11 + rax * 4] # load pigeonhole value
    inc edx                  # increment count
    mov [r11 + rax * 4], edx # store back
    
    inc r12
    jmp fill_loop
    
fill_done:
    # Reconstruct sorted array
    mov r12, 0           # i = 0 (output index)
    mov r13, 0           # j = 0 (pigeonhole index)
reconstruct_loop:
    cmp r13, r10
    jge reconstruct_done
    
    mov eax, [r11 + r13 * 4]  # load count
    cmp eax, 0
    je next_pigeonhole
    
    # Place elements in sorted order
    mov r14, 0           # k = 0 (count index)
place_loop:
    cmp r14, eax
    jge place_done
    
    mov edx, r13         # edx = pigeonhole index
    add edx, ecx         # edx = actual value (offset + min)
    mov [r8 + r12 * 4], edx  # store in original array
    inc r12              # increment output index
    inc r14
    jmp place_loop
    
place_done:
next_pigeonhole:
    inc r13
    jmp reconstruct_loop
    
reconstruct_done:
    # Free pigeonhole array
    mov rdi, r11
    call free
    
    pop rbp
    ret

# Simple malloc implementation (simplified for example)
malloc:
    # This is a simplified malloc - in real code would use system calls
    # For demonstration, just return a fixed address
    mov rax, 0x1000000   # Return some address
    ret

# Simple free implementation (simplified)
free:
    # Do nothing in this example
    ret

_start:
    # Call pigeonhole sort on our array
    mov rdi, array       # array address
    mov rsi, array_size  # array size
    call pigeonhole_sort
    
    # Exit program
    mov rax, 60          # sys_exit
    mov rdi, 0           # exit status
    syscall
```

## Algorithm Explanation

This assembly implementation of Pigeonhole Sort follows these steps:

1. **Find Min/Max**: Determine the range of values in the input array
2. **Create Pigeonholes**: Allocate an array of size (max - min + 1)
3. **Fill Pigeonholes**: Count occurrences of each value
4. **Reconstruct Array**: Place elements back in sorted order

## Key Features

- **Input/Output**: Takes array address and size as parameters
- **Memory Management**: Allocates and frees pigeonhole array
- **In-place Sorting**: Modifies original array
- **Time Complexity**: O(n + k) where n is array size and k is range
- **Space Complexity**: O(k) for pigeonhole array

## Notes

- This is a simplified implementation for educational purposes
- Real-world implementations would use proper system calls for memory management
- Error handling is minimal for clarity
- The algorithm works best when range (k) is not significantly larger than array size (n)

