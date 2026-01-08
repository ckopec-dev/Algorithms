# Warshall's Algorithm in Assembly Language

Here's an example implementation of Warshall's algorithm for finding the transitive closure of a graph using x86-64 Assembly language:

```assembly
.section .data
    # Graph adjacency matrix (4x4 example)
    matrix: .long 1, 0, 1, 0
            .long 0, 1, 1, 0
            .long 0, 0, 1, 1
            .long 0, 0, 0, 1
    
    # Matrix size
    matrix_size: .long 4

.section .text
    .global _start

warshall_algorithm:
    # Function to compute transitive closure using Warshall's algorithm
    # Input: matrix pointer, size
    # Output: modified matrix with transitive closure
    
    push %rbp
    mov %rsp, %rbp
    
    # Parameters
    mov %rdi, %r8      # matrix pointer
    mov %rsi, %r9      # matrix size
    
    # i = 0 to size-1
    xor %rax, %rax     # i = 0
outer_loop:
    cmp %r9, %rax      # compare i with size
    jge end_warshall   # if i >= size, exit
    
    # j = 0 to size-1
    xor %rbx, %rbx     # j = 0
inner_loop:
    cmp %r9, %rbx      # compare j with size
    jge next_i         # if j >= size, next i
    
    # k = 0 to size-1
    xor %rcx, %rcx     # k = 0
innermost_loop:
    cmp %r9, %rcx      # compare k with size
    jge next_j         # if k >= size, next j
    
    # Calculate matrix[i][j] = matrix[i][j] OR (matrix[i][k] AND matrix[k][j])
    
    # Calculate addresses for matrix[i][k]
    mov %rax, %r10     # i
    mov %rcx, %r11     # k
    mov %r10, %r12
    shl $2, %r12       # i * 4 (word size)
    mov %r11, %r13
    shl $2, %r13       # k * 4
    add %r12, %r13     # (i * 4) + (k * 4)
    add %r8, %r13      # matrix[i][k] address
    
    # Load matrix[i][k]
    movl (%r13), %r14d
    
    # Skip if matrix[i][k] = 0
    test %r14d, %r14d
    jz skip_update
    
    # Calculate address for matrix[k][j]
    mov %rcx, %r12     # k
    mov %rbx, %r13     # j
    mov %r12, %r15
    shl $2, %r15       # k * 4
    mov %r13, %r12
    shl $2, %r12       # j * 4
    add %r15, %r12     # (k * 4) + (j * 4)
    add %r8, %r12      # matrix[k][j] address
    
    # Load matrix[k][j]
    movl (%r12), %r14d
    
    # Skip if matrix[k][j] = 0
    test %r14d, %r14d
    jz skip_update
    
    # Calculate address for matrix[i][j]
    mov %rax, %r12     # i
    mov %rbx, %r13     # j
    mov %r12, %r15
    shl $2, %r15       # i * 4
    mov %r13, %r12
    shl $2, %r12       # j * 4
    add %r15, %r12     # (i * 4) + (j * 4)
    add %r8, %r12      # matrix[i][j] address
    
    # Load current matrix[i][j]
    movl (%r12), %r14d
    
    # Update matrix[i][j] = matrix[i][j] OR 1 (if both are 1)
    orl $1, %r14d
    
    # Store back to matrix[i][j]
    movl %r14d, (%r12)
    
skip_update:
    inc %rcx           # k++
    jmp innermost_loop
    
next_j:
    inc %rbx           # j++
    jmp inner_loop
    
next_i:
    inc %rax           # i++
    jmp outer_loop
    
end_warshall:
    pop %rbp
    ret

# Main program
_start:
    # Call Warshall's algorithm
    mov $matrix, %rdi   # matrix pointer
    mov $4, %rsi        # matrix size
    call warshall_algorithm
    
    # Exit program
    mov $60, %rax       # sys_exit
    mov $0, %rdi        # exit status
    syscall
```

## Explanation of Key Components

### Algorithm Logic
The Warshall's algorithm implementation follows this core logic:
1. **Three nested loops** iterate through all combinations of (i, j, k)
2. **Matrix update** uses the formula: `matrix[i][j] = matrix[i][j] OR (matrix[i][k] AND matrix[k][j])`
3. **Memory addressing** calculates correct matrix indices using row-major order

### Key Assembly Concepts Used
- **Register usage**: `%rax`, `%rbx`, `%rcx`, `%rdi`, `%rsi`, `%r8`, `%r9` for parameters and counters
- **Memory operations**: `movl` for 32-bit loads/stores, `add` for address calculations
- **Control flow**: `cmp`, `jge`, `jz`, `jmp` for loop and conditional logic
- **Bit operations**: `test` and `orl` for logical operations

### Matrix Representation
The adjacency matrix is stored in row-major order:
```
[1 0 1 0]
[0 1 1 0]  
[0 0 1 1]
[0 0 0 1]
```

This implementation demonstrates how Warshall's algorithm can be translated from high-level pseudocode to assembly language while maintaining the same computational complexity of O(n³).

