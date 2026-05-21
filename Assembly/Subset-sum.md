# Subset Sum Algorithm in Assembly (x86-64)

Here's an implementation of the subset sum algorithm using dynamic programming in x86-64 assembly:

```assembly
.section .data
    # Sample input array
    array: .long 3, 34, 4, 12, 5, 2
    array_size: .long 6
    target_sum: .long 9
    
    # DP table - 2D array (size x target)
    # We'll allocate space dynamically

.section .text
    .global _start

# Function: subset_sum
# Parameters: array pointer, array_size, target_sum
# Returns: 1 if subset exists, 0 otherwise
subset_sum:
    push    %rbp
    mov     %rsp, %rbp
    push    %rbx
    push    %rcx
    push    %rdx
    push    %rsi
    push    %rdi

    # Parameters:
    # %rdi = array pointer
    # %rsi = array_size
    # %rdx = target_sum
    
    # Allocate DP table: dp[size+1][target+1]
    # We'll use a simple approach - allocate on stack
    
    mov     %rsi, %rax          # array_size
    inc     %rax                # +1 for base case
    mov     %rdx, %rbx          # target_sum
    inc     %rbx                # +1 for base case
    imul    %rbx, %rax          # size * target
    
    # Allocate space for dp table (each element is 4 bytes)
    mov     %rax, %rcx
    shl     $2, %rcx            # multiply by 4 (bytes per int)
    
    # Allocate stack space for DP table
    sub     %rcx, %rsp          # allocate space
    
    # Initialize DP table
    # dp[0][0] = 1 (empty subset sums to 0)
    mov     $1, (%rsp)          # dp[0][0] = 1
    
    # dp[0][j] = 0 for j > 0 (empty subset can't make non-zero sum)
    mov     %rdx, %rcx          # target_sum
    mov     %rsp, %rbx          # pointer to dp table
    add     $4, %rbx            # skip dp[0][0]
    
    # Initialize first row to 0
    xor     %rax, %rax          # rax = 0
    mov     %rcx, %r8           # counter
    mov     %rbx, %rdi          # pointer to dp table
    
    # Loop to set dp[0][j] = 0 for j > 0
    loop_init:
        cmp     %r8, %rax
        je      init_done
        mov     %rax, (%rdi)
        add     $4, %rdi
        inc     %rax
        jmp     loop_init
    init_done:
    
    # Fill DP table
    mov     $1, %r9             # i = 1 (current element index)
    mov     %rsi, %r10          # array_size
    
    outer_loop:
        cmp     %r10, %r9
        jg      outer_loop_end
        
        # Get current element
        mov     %rdi, %r11          # array pointer
        mov     %r9, %rax
        dec     %rax                # i-1
        mov     (%r11,%rax,4), %eax # array[i-1]
        mov     %eax, %ebx          # current element value
        
        # Inner loop: j = 0 to target
        mov     $0, %r12            # j = 0
        mov     %rdx, %r13          # target_sum
        
    inner_loop:
        cmp     %r13, %r12
        jg      inner_loop_end
        
        # dp[i][j] = dp[i-1][j] OR dp[i-1][j-element]
        # Calculate indices for dp table
        mov     %r9, %rax           # i
        mov     %r12, %rbx          # j
        imul    %rdx, %rax          # i * target
        add     %rbx, %rax          # i * target + j
        mov     $4, %rcx            # bytes per int
        imul    %rcx, %rax          # offset in bytes
        add     %rsp, %rax          # address of dp[i][j]
        
        # Calculate dp[i-1][j]
        mov     %r9, %rax           # i
        dec     %rax                # i-1
        mov     %r12, %rbx          # j
        imul    %rdx, %rax          # (i-1) * target
        add     %rbx, %rax          # (i-1) * target + j
        imul    %rcx, %rax          # offset in bytes
        add     %rsp, %rax          # address of dp[i-1][j]
        mov     (%rax), %ecx        # dp[i-1][j]
        
        # Check if j >= element
        cmp     %ebx, %r12
        jl      skip_element_check
        
        # Calculate dp[i-1][j-element]
        mov     %r12, %rax
        sub     %ebx, %rax          # j - element
        mov     %r9, %rbx           # i
        dec     %rbx                # i-1
        imul    %rdx, %rbx          # (i-1) * target
        add     %rax, %rbx          # (i-1) * target + (j-element)
        imul    %rcx, %rbx          # offset in bytes
        add     %rsp, %rbx          # address of dp[i-1][j-element]
        mov     (%rbx), %edx        # dp[i-1][j-element]
        
        # dp[i][j] = dp[i-1][j] OR dp[i-1][j-element]
        or      %edx, %ecx
        jmp     store_result
        
    skip_element_check:
        # dp[i][j] = dp[i-1][j]
        mov     %ecx, %edx
        
    store_result:
        # Store result back
        mov     %r9, %rax           # i
        mov     %r12, %rbx          # j
        imul    %rdx, %rax          # i * target
        add     %rbx, %rax          # i * target + j
        imul    %rcx, %rax          # offset in bytes
        add     %rsp, %rax          # address of dp[i][j]
        mov     %edx, (%rax)
        
    inner_loop_end:
        inc     %r12
        jmp     inner_loop
        
    outer_loop_end:
        # Return result: dp[array_size][target]
        mov     %rsi, %rax          # array_size
        mov     %rdx, %rbx          # target_sum
        imul    %rdx, %rax          # array_size * target
        add     %rbx, %rax          # array_size * target + target
        imul    %rcx, %rax          # offset in bytes
        add     %rsp, %rax          # address of dp[array_size][target]
        mov     (%rax), %eax        # return value
        
        # Restore stack and return
        mov     %rbp, %rsp
        pop     %rdi
        pop     %rsi
        pop     %rdx
        pop     %rcx
        pop     %rbx
        pop     %rbp
        ret

# Main program
_start:
    # Initialize parameters
    lea     array(%rip), %rdi   # array pointer
    mov     array_size(%rip), %rsi # array_size
    mov     target_sum(%rip), %rdx # target_sum
    
    # Call subset_sum function
    call    subset_sum
    
    # Exit program
    mov     $60, %rax           # sys_exit
    mov     $0, %rdi            # exit status
    syscall
```

## Explanation of Key Components:

### 1. **Data Section**
- Contains sample array `[3, 34, 4, 12, 5, 2]`
- Defines array size and target sum (9)

### 2. **Function Structure**
- Uses dynamic programming approach
- Creates a 2D boolean DP table
- `dp[i][j]` represents whether sum `j` can be achieved using first `i` elements

### 3. **Algorithm Logic**
- **Base case**: `dp[0][0] = 1` (empty subset sums to 0)
- **Recurrence relation**: 
  ```
  dp[i][j] = dp[i-1][j] OR dp[i-1][j-element]
  ```
- **Time complexity**: O(n × target)
- **Space complexity**: O(n × target)

### 4. **Key Assembly Concepts**
- Stack management for local variables
- Pointer arithmetic for array indexing
- Loop control with conditional jumps
- Register usage for efficient computation

### 5. **Expected Output**
For the given input, the function should return `1` (true) since subset `{4, 5}` sums to 9.

This implementation demonstrates how to translate a classic dynamic programming algorithm into low-level assembly language while maintaining the core algorithmic logic.

