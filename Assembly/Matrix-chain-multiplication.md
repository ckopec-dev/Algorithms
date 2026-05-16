# Matrix Chain Multiplication in Assembly Language

Here's an example implementation of the Matrix Chain Multiplication algorithm in x86-64 Assembly language:

```assembly
.section .data
    # Matrix dimensions array (example: 4 matrices with dimensions 10x100, 100x5, 5x50, 50x10)
    matrix_dims: .long 10, 100, 5, 50, 10
    n: .long 4                    # Number of matrices
    
    # Memoization table for dynamic programming
    # Size: (n+1) x (n+1) = 5x5 = 25 integers
    memo_table: .space 100         # 25 * 4 bytes = 100 bytes

.section .text
    .global _start

# Function: matrix_chain_multiplication
# Input: 
#   - rdi: pointer to matrix dimensions array
#   - rsi: number of matrices (n)
# Output: minimum number of scalar multiplications
matrix_chain_multiplication:
    push rbp
    mov rbp, rsp
    
    # Save parameters
    mov r12, rdi        # r12 = dimensions array pointer
    mov r13, rsi        # r13 = n (number of matrices)
    
    # Initialize memoization table with zeros
    mov rax, 0
    mov rdx, 0
    mov r8, 25          # 5x5 table = 25 elements
    
init_memo_loop:
    mov DWORD PTR [memo_table + rax*4], 0
    add rax, 1
    dec r8
    jnz init_memo_loop
    
    # Call recursive function
    mov rdi, 1          # i = 1
    mov rsi, r13        # j = n
    call matrix_chain_recursive
    
    mov eax, eax        # Return result in eax
    
    pop rbp
    ret

# Recursive function: matrix_chain_recursive(i, j)
# Input: rdi = i, rsi = j
# Output: minimum cost in eax
matrix_chain_recursive:
    push rbp
    mov rbp, rsp
    
    # Base case: if i >= j, cost is 0
    cmp rdi, rsi
    jge base_case
    
    # Check if already computed
    mov rax, rdi        # rax = i
    mov rdx, rsi        # rdx = j
    mov r8, 5           # table size = 5
    imul rax, r8        # rax = i * 5
    add rax, rdx        # rax = i * 5 + j
    mov eax, DWORD PTR [memo_table + rax*4]
    test eax, eax
    jnz memoized_result
    
    # Compute minimum cost
    mov rax, rdi        # rax = i
    mov rdx, rsi        # rdx = j
    mov r10, 0          # min_cost = 0
    mov r11, 0          # k = i
    
    # Loop through k from i to j-1
k_loop:
    cmp r11, rsi
    jge k_loop_end
    
    # Check bounds
    cmp r11, rdi
    jl k_loop_continue
    
    # Calculate cost for split at k
    mov r8, r11         # r8 = k
    mov r9, rdi         # r9 = i
    mov r10, rsi        # r10 = j
    
    # Calculate cost = cost[i][k] + cost[k+1][j] + p[i-1]*p[k]*p[j]
    # First: cost[i][k]
    mov rax, r9
    dec rax
    mov rdx, r8
    mov r12, 5
    imul rax, r12
    add rax, rdx
    mov eax, DWORD PTR [memo_table + rax*4]
    
    # Second: cost[k+1][j]
    mov rax, r8
    inc rax
    mov rdx, r10
    mov r12, 5
    imul rax, r12
    add rax, rdx
    mov ecx, DWORD PTR [memo_table + rax*4]
    add eax, ecx
    
    # Third: p[i-1] * p[k] * p[j]
    # Get p[i-1] from array
    mov rax, r9
    dec rax
    mov rdx, 4
    imul rax, rdx
    mov rdx, DWORD PTR [matrix_dims + rax]
    
    # Get p[k]
    mov rax, r8
    mov rax, 4
    imul rax, rdx
    mov rdx, DWORD PTR [matrix_dims + rax]
    
    # Get p[j]
    mov rax, r10
    mov rax, 4
    imul rax, rdx
    mov ecx, DWORD PTR [matrix_dims + rax]
    
    # Multiply: p[i-1] * p[k] * p[j]
    imul edx, ecx
    imul eax, edx
    
    # Add all costs
    add eax, DWORD PTR [memo_table + rax*4]
    
    # Update minimum
    cmp r10, 0
    jne check_min
    mov r10, eax
    jmp k_loop_continue
    
check_min:
    cmp eax, r10
    jg k_loop_continue
    mov r10, eax
    
k_loop_continue:
    inc r11
    jmp k_loop
    
k_loop_end:
    # Store result in memo table
    mov rax, rdi
    mov rdx, rsi
    mov r12, 5
    imul rax, r12
    add rax, rdx
    mov DWORD PTR [memo_table + rax*4], r10
    
    mov eax, r10
    jmp end_recursive
    
base_case:
    mov eax, 0
    
end_recursive:
    pop rbp
    ret

# Main function
_start:
    # Initialize data
    mov rdi, matrix_dims
    mov rsi, 4          # n = 4 matrices
    call matrix_chain_multiplication
    
    # Exit program
    mov rax, 60         # sys_exit
    mov rdi, 0          # exit status
    syscall
```

## Key Features of this Implementation:

1. **Dynamic Programming Approach**: Uses memoization to avoid recomputing subproblems
2. **Matrix Dimension Storage**: Stores matrix dimensions in an array
3. **Memoization Table**: 5x5 table to cache computed results
4. **Recursive Structure**: Implements the classic matrix chain multiplication algorithm
5. **Memory Management**: Properly initializes and uses memory for the DP table

## Algorithm Explanation:

The matrix chain multiplication problem finds the optimal way to parenthesize matrix products to minimize scalar multiplications. The algorithm uses dynamic programming with the recurrence relation:

```
m[i][j] = min{m[i][k] + m[k+1][j] + p[i-1] * p[k] * p[j]}
```

Where:
- `m[i][j]` = minimum cost to multiply matrices from i to j
- `p[i]` = dimension of matrix i
- `k` = split point between i and j

This assembly implementation demonstrates how the mathematical algorithm translates to low-level code with proper memory management and recursive function calls.

