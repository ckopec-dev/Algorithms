# Reservoir Sampling Algorithm in Assembly

Here's an example implementation of the Reservoir Sampling algorithm in x86-64 Assembly language:

```assembly
.section .data
    .align 8
    reservoir: .quad 0        # Array to store reservoir samples
    .quad 0
    .quad 0
    .quad 0
    count: .quad 0           # Counter for total elements seen
    k: .quad 4               # Size of reservoir (sample size)

.section .text
    .global _reservoir_sampling
    .type _reservoir_sampling, @function

_reservoir_sampling:
    # Function parameters:
    # RDI = pointer to input data array
    # RSI = size of input array
    # RDX = sample size k
    
    push rbp
    mov rbp, rsp
    
    # Initialize variables
    mov r8, rdi              # r8 = input array pointer
    mov r9, rsi              # r9 = array size
    mov r10, rdx             # r10 = sample size k
    mov r11, 0               # r11 = current index
    mov r12, 0               # r12 = count of elements processed
    
    # Initialize reservoir array to zero
    mov rdi, offset reservoir
    mov rsi, 32              # 4 * 8 bytes = 32 bytes
    xor rax, rax
    rep stosb
    
    # Main loop
loop_start:
    # Check if we've processed all elements
    cmp r11, r9
    jge loop_end
    
    # Increment count
    inc r12
    
    # If we haven't filled reservoir yet, copy element
    cmp r12, r10
    jle copy_element
    
    # Generate random number between 0 and count-1
    mov rax, r12
    xor rdx, rdx
    mov rbx, r10             # Use sample size as divisor
    div rbx                  # rax = r12 / k, rdx = random index
    mov rax, rdx             # rax = random index (0 to k-1)
    
    # If random index is less than k, replace element
    cmp rax, r10
    jge skip_replace
    
    # Replace reservoir element at random index
    mov rdi, offset reservoir
    mov rsi, rax
    shl rsi, 3               # Multiply by 8 (size of qword)
    add rdi, rsi
    mov rsi, r8
    mov rdx, r11
    shl rdx, 3               # Multiply by 8
    add rsi, rdx
    mov rax, [rsi]           # Load input element
    mov [rdi], rax           # Store in reservoir
    
skip_replace:
copy_element:
    # Copy element to reservoir if we haven't filled it yet
    cmp r12, r10
    jge skip_copy
    
    mov rdi, offset reservoir
    mov rsi, r11
    shl rsi, 3               # Multiply by 8
    add rdi, rsi
    mov rsi, r8
    mov rdx, r11
    shl rdx, 3               # Multiply by 8
    add rsi, rdx
    mov rax, [rsi]           # Load input element
    mov [rdi], rax           # Store in reservoir
    
skip_copy:
    # Move to next element
    inc r11
    jmp loop_start

loop_end:
    # Return reservoir array pointer
    mov rax, offset reservoir
    
    pop rbp
    ret

# Helper function to get random number (simplified)
_get_random:
    # Simple linear congruential generator
    push rax
    push rbx
    push rcx
    push rdx
    
    mov rax, [rdi]           # Load seed
    mov rbx, 1103515245      # Multiplier
    mov rcx, 12345           # Addend
    mov rdx, 2147483648      # Modulus
    mul rbx
    add rax, rcx
    mov [rdi], rax           # Store new seed
    xor rdx, rdx
    div rdx                  # Return random number in rax
    
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
```

## Key Features of this Implementation:

1. **Input Parameters**: 
   - `RDI`: Pointer to input data array
   - `RSI`: Size of input array
   - `RDX`: Sample size `k`

2. **Algorithm Steps**:
   - Initialize reservoir array
   - Process each element in the input array
   - For first `k` elements, copy them to reservoir
   - For subsequent elements, replace random reservoir elements with probability `k/n`

3. **Memory Management**:
   - Uses stack space for local variables
   - Reservoir stored in `.data` section
   - Proper pointer arithmetic for array access

4. **Assembly-Specific Features**:
   - Uses x86-64 registers efficiently
   - Implements bit shifting for array indexing
   - Handles division and modulo operations manually
   - Proper function prologue/epilogue

This implementation demonstrates how reservoir sampling can be efficiently implemented in assembly language, maintaining the O(n) time complexity and O(k) space complexity of the original algorithm.

