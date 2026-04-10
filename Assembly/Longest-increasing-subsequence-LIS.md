# Longest Increasing Subsequence (LIS) in Assembly

Here's an implementation of the LIS algorithm in x86-64 Assembly using the dynamic programming approach:

```assembly
.section .data
    # Sample array for demonstration
    array: .long 10, 9, 2, 5, 3, 7, 101, 18
    array_size: .long 8

    # Buffer for storing LIS lengths
    lis_lengths: .space 32    # 8 * 4 bytes for 8 elements

.section .text
    .global _start

# Function: lis_length
# Input: array pointer, array_size
# Output: maximum length of LIS in eax
lis_length:
    push rbp
    mov rbp, rsp
    
    # Parameters
    mov r8, rdi      # array pointer
    mov r9, rsi      # array_size
    
    # Initialize loop counter and max_length
    xor rcx, rcx     # i = 0
    xor r10, r10     # max_length = 0
    
    # Initialize lis_lengths array with 1s
init_loop:
    cmp rcx, r9
    jge init_done
    
    # lis_lengths[i] = 1
    mov eax, 1
    mov [lis_lengths + rcx*4], eax
    
    inc rcx
    jmp init_loop
init_done:
    
    # Main DP loop
    xor rcx, rcx     # i = 0
outer_loop:
    cmp rcx, r9
    jge outer_done
    
    # Reset inner loop counter
    xor rdx, rdx     # j = 0
    
inner_loop:
    cmp rdx, rcx
    jge inner_done
    
    # Get array[i] and array[j]
    mov eax, [r8 + rcx*4]     # array[i]
    mov ebx, [r8 + rdx*4]     # array[j]
    
    # If array[j] < array[i], update lis_lengths[i]
    cmp ebx, eax
    jge skip_update
    
    # lis_lengths[i] = max(lis_lengths[i], lis_lengths[j] + 1)
    mov eax, [lis_lengths + rcx*4]     # lis_lengths[i]
    mov edi, [lis_lengths + rdx*4]     # lis_lengths[j]
    inc edi                            # lis_lengths[j] + 1
    cmp eax, edi
    jge skip_update
    mov [lis_lengths + rcx*4], edi     # lis_lengths[i] = lis_lengths[j] + 1
    
skip_update:
    inc rdx
    jmp inner_loop
inner_done:
    
    # Update max_length
    mov eax, [lis_lengths + rcx*4]
    cmp r10, eax
    jge skip_max_update
    mov r10, rax
    
skip_max_update:
    inc rcx
    jmp outer_loop
outer_done:
    
    # Return result
    mov eax, r10
    pop rbp
    ret

# Main program
_start:
    # Load array and size
    lea rdi, array
    mov rsi, array_size
    mov rsi, [rsi]      # Load array_size
    
    # Call LIS function
    call lis_length
    
    # Exit program
    mov eax, 1          # sys_exit
    mov ebx, 0          # exit status
    int 0x80
```

## Algorithm Explanation

This assembly implementation follows the classic dynamic programming approach for LIS:

### Key Steps:
1. **Initialization**: Create a `lis_lengths` array where each element starts with 1
2. **Outer Loop**: For each element `i` in the array
3. **Inner Loop**: For each previous element `j` where `j < i`
4. **Comparison**: If `array[j] < array[i]`, update `lis_lengths[i]`
5. **Maximum Tracking**: Keep track of the maximum LIS length found

### Time Complexity: O(n²)
### Space Complexity: O(n)

### Key Assembly Features Used:
- **Register usage**: RDI, RSI, R8, R9, RCX, RDX, R10 for parameters and counters
- **Memory operations**: Loading/storing array elements and lis_lengths
- **Conditional jumps**: For loop control and comparisons
- **Arithmetic operations**: Addition and comparison operations

### Sample Input:
Array: [10, 9, 2, 5, 3, 7, 101, 18]

### Expected Output:
LIS length: 4 (subsequence: 2, 3, 7, 18 or 2, 3, 7, 101)

This implementation demonstrates how to translate a classic algorithm into assembly language while maintaining the same logic and efficiency as a high-level implementation.

