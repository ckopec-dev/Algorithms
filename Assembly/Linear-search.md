# Linear Search Algorithm in Assembly (x86-64)

Here's an example of a linear search algorithm implemented in x86-64 Assembly:

```assembly
.section .data
    # Sample array to search through
    array: .long 10, 25, 3, 47, 15, 8, 92, 33, 56, 71
    array_size: .long 10
    
    # Value to search for
    search_value: .long 47

.section .text
    .global _start

# Function: linear_search
# Parameters:
#   rdi = array pointer
#   rsi = array size
#   rdx = search value
# Returns:
#   rax = index if found, -1 if not found
linear_search:
    push rbp
    mov rbp, rsp
    
    # Initialize variables
    mov rcx, 0          # i = 0 (loop counter)
    mov rax, -1         # result = -1 (not found)
    
search_loop:
    # Check if we've reached the end of array
    cmp rcx, rsi        # compare i with array_size
    jge not_found       # if i >= size, not found
    
    # Load array[i] into r8
    mov r8, [rdi + rcx*4]  # array[i] (assuming 4-byte integers)
    
    # Compare array[i] with search_value
    cmp r8, rdx         # compare array[i] with search_value
    je found            # if equal, found it
    
    # Increment counter
    inc rcx             # i++
    jmp search_loop     # continue loop
    
found:
    mov rax, rcx        # return index
    jmp search_end
    
not_found:
    mov rax, -1         # return -1 (not found)
    
search_end:
    pop rbp
    ret

# Main program
_start:
    # Set up parameters for linear_search
    lea rdi, array      # array pointer
    mov rsi, [array_size]  # array size
    mov rdx, [search_value] # search value
    
    # Call linear_search function
    call linear_search
    
    # Exit program (for Linux)
    mov rax, 60         # sys_exit
    mov rdi, 0          # exit status
    syscall
```

## How it works:

1. **Function Parameters**: 
   - `rdi`: pointer to the array
   - `rsi`: size of the array
   - `rdx`: value to search for

2. **Algorithm Steps**:
   - Initialize counter `i = 0`
   - Loop through each element of the array
   - Compare current element with search value
   - If match found, return index
   - If end of array reached without match, return -1

3. **Key Instructions**:
   - `cmp`: Compare two values
   - `je`: Jump if equal
   - `jge`: Jump if greater or equal
   - `mov`: Move data between registers/memory
   - `lea`: Load effective address

## Example Usage:
If searching for value `47` in array `[10, 25, 3, 47, 15, 8, 92, 33, 56, 71]`, the function will return `3` (the index of 47).

This implementation demonstrates the basic linear search algorithm in assembly language, showing how the sequential comparison approach is implemented at the machine level.

