# Longest Common Subsequence (LCS) Algorithm in Assembly

Here's an example implementation of the LCS algorithm in x86-64 Assembly using AT&T syntax:

```assembly
.section .data
    # Input strings
    str1: .ascii "ABCDGH"
    str2: .ascii "AEDFHR"
    len1 = 6
    len2 = 6

.section .text
    .global _start

# Function: lcs_length
# Parameters: 
#   %rdi = pointer to string1
#   %rsi = pointer to string2
#   %rdx = length of string1
#   %rcx = length of string2
# Returns: length of LCS in %rax
lcs_length:
    # Allocate space for DP table (len1+1) x (len2+1)
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %r12
    push %r13
    push %r14
    
    # Calculate table dimensions
    mov %rdx, %r8          # len1
    mov %rcx, %r9          # len2
    inc %r8                # len1 + 1
    inc %r9                # len2 + 1
    mov %r8, %r10          # r10 = len1 + 1
    mov %r9, %r11          # r11 = len2 + 1
    
    # Allocate memory for DP table (2D array)
    mov %r10, %rax
    imul %r11              # size = (len1+1) * (len2+1)
    shl $3, %rax           # multiply by 8 (size of long)
    mov %rax, %r12         # r12 = total bytes needed
    
    # Allocate memory (simplified - in practice would use malloc)
    # For this example, we'll use stack space
    sub %r12, %rsp         # Allocate space on stack
    
    # Initialize DP table
    # Set first row to zeros
    mov $0, %r13           # row counter
outer_loop:
    cmp %r10, %r13         # if row >= len1 + 1
    jge end_outer_loop
    
    # Set first column to zeros
    mov $0, %r14           # col counter
inner_loop:
    cmp %r11, %r14         # if col >= len2 + 1
    jge end_inner_loop
    
    # Calculate table position: table[row][col]
    mov %r13, %rax
    imul %r11, %rax        # row * (len2 + 1)
    add %r14, %rax         # + col
    shl $3, %rax           # multiply by 8 (size of long)
    add %rsp, %rax         # add base address
    
    # Set table[row][col] = 0
    mov $0, (%rax)
    
    inc %r14
    jmp inner_loop
end_inner_loop:
    inc %r13
    jmp outer_loop
end_outer_loop:
    
    # Fill the DP table
    mov $1, %r13           # row = 1
fill_outer_loop:
    cmp %r10, %r13         # if row > len1
    jg end_fill_outer_loop
    
    mov $1, %r14           # col = 1
fill_inner_loop:
    cmp %r11, %r14         # if col > len2
    jg end_fill_inner_loop
    
    # Get characters
    mov %rdi, %rax
    dec %r13               # row - 1
    add %rax, %rax         # pointer to str1[row-1]
    movb (%rax), %al       # str1[row-1]
    
    mov %rsi, %rax
    dec %r14               # col - 1
    add %rax, %rax         # pointer to str2[col-1]
    movb (%rax), %bl       # str2[col-1]
    
    # Compare characters
    cmp %al, %bl
    jne not_equal
    
    # Characters equal: table[i][j] = table[i-1][j-1] + 1
    mov %r13, %rax
    dec %rax
    imul %r11, %rax        # (i-1) * (len2+1)
    add %r14, %rax         # + (j-1)
    shl $3, %rax           # * 8
    add %rsp, %rax         # base address
    
    mov (%rax), %rax       # table[i-1][j-1]
    inc %rax               # + 1
    
    # Store result
    mov %r13, %rax
    imul %r11, %rax        # i * (len2+1)
    add %r14, %rax         # + j
    shl $3, %rax           # * 8
    add %rsp, %rax         # base address
    mov %rax, (%rax)       # table[i][j] = result
    
    jmp next_iteration
    
not_equal:
    # Characters not equal: table[i][j] = max(table[i-1][j], table[i][j-1])
    # table[i-1][j]
    mov %r13, %rax
    dec %rax
    imul %r11, %rax        # (i-1) * (len2+1)
    add %r14, %rax         # + j
    shl $3, %rax           # * 8
    add %rsp, %rax         # base address
    mov (%rax), %rax       # table[i-1][j]
    
    # table[i][j-1]
    mov %r13, %rbx
    imul %r11, %rbx        # i * (len2+1)
    add %r14, %rbx         # + (j-1)
    shl $3, %rbx           # * 8
    add %rsp, %rbx         # base address
    mov (%rbx), %rbx       # table[i][j-1]
    
    # Compare and take max
    cmp %rax, %rbx
    jg use_first
    mov %rbx, %rax         # max = table[i][j-1]
    jmp store_result
    
use_first:
    # max = table[i-1][j]
    
store_result:
    # Store result in table[i][j]
    mov %r13, %rcx
    imul %r11, %rcx        # i * (len2+1)
    add %r14, %rcx         # + j
    shl $3, %rcx           # * 8
    add %rsp, %rcx         # base address
    mov %rax, (%rcx)       # table[i][j] = max
    
next_iteration:
    inc %r14
    jmp fill_inner_loop
end_fill_inner_loop:
    inc %r13
    jmp fill_outer_loop
end_fill_outer_loop:
    
    # Return LCS length (table[len1][len2])
    mov %r10, %rax
    dec %rax               # len1
    imul %r11, %rax        # (len1) * (len2+1)
    add %r11, %rax         # + len2
    dec %rax               # - 1 (since we want len2)
    shl $3, %rax           # * 8
    add %rsp, %rax         # base address
    mov (%rax), %rax       # table[len1][len2]
    
    # Cleanup
    add %r12, %rsp         # Free memory
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    pop %rbp
    ret

# Main program
_start:
    # Initialize parameters
    mov $str1, %rdi        # pointer to string1
    mov $str2, %rsi        # pointer to string2
    mov $len1, %rdx        # length of string1
    mov $len2, %rcx        # length of string2
    
    # Call LCS function
    call lcs_length
    
    # Exit program
    mov $60, %rax          # sys_exit
    mov $0, %rdi           # exit status
    syscall
```

## Key Features of This Implementation:

1. **Dynamic Programming Approach**: Uses a 2D table to store intermediate results
2. **Memory Management**: Allocates space for the DP table on the stack
3. **Character Comparison**: Compares characters from both strings
4. **Recurrence Relation**: Implements the standard LCS formula:
   - If characters match: `LCS[i][j] = LCS[i-1][j-1] + 1`
   - If characters don't match: `LCS[i][j] = max(LCS[i-1][j], LCS[i][j-1])`

## Expected Output:
For strings "ABCDGH" and "AEDFHR", the LCS length is **3** (the common subsequence is "ADH").

## Notes:
- This is a simplified implementation for educational purposes
- In practice, you'd want to use proper memory allocation functions
- Error handling and edge cases are omitted for clarity
- The algorithm runs in O(m×n) time and space complexity

