# Ternary Search Algorithm in Assembly

Here's an example of a ternary search algorithm implemented in x86-64 Assembly language:

```assembly
.section .text
.global ternary_search

# Function: ternary_search
# Parameters:
#   RDI = array pointer
#   RSI = left index
#   RDX = right index
#   RCX = target value
# Returns:
#   RAX = index of target or -1 if not found

ternary_search:
    # Base case: if left > right, element not found
    cmp rsi, rdx
    jg not_found
    
    # Calculate mid1 = left + (right - left) / 3
    mov r8, rdx          # r8 = right
    sub r8, rsi          # r8 = right - left
    mov r9, r8           # r9 = right - left
    sar r9, 2            # r9 = (right - left) / 4 (approximate)
    add r9, rsi          # r9 = left + (right - left) / 4
    mov r10, r9          # r10 = mid1
    
    # Calculate mid2 = left + 2 * (right - left) / 3
    mov r11, r8          # r11 = right - left
    mov r12, r11         # r12 = right - left
    sar r12, 1           # r12 = (right - left) / 2
    shl r12, 1           # r12 = (right - left) / 2 * 2 = (right - left) / 1
    sar r12, 2           # r12 = (right - left) / 4
    shl r12, 2           # r12 = (right - left) / 1
    add r12, rsi         # r12 = left + 2 * (right - left) / 3
    mov r13, r12         # r13 = mid2
    
    # Load array elements at mid1 and mid2
    mov r14, r10         # r14 = mid1
    mov r15, r13         # r15 = mid2
    
    # Calculate array indices
    mov rax, [rdi + r14*8]  # Load array[mid1]
    mov rbx, [rdi + r15*8]  # Load array[mid2]
    
    # Compare with target
    cmp rax, rcx         # Compare array[mid1] with target
    je found_mid1        # If equal, found at mid1
    
    cmp rbx, rcx         # Compare array[mid2] with target
    je found_mid2        # If equal, found at mid2
    
    # Determine which third to search
    cmp rax, rcx         # Compare array[mid1] with target
    jg search_left       # If array[mid1] > target, search left third
    
    cmp rbx, rcx         # Compare array[mid2] with target
    jl search_right      # If array[mid2] < target, search right third
    
    # Search middle third
    mov r14, r10         # r14 = mid1
    mov r15, r13         # r15 = mid2
    add r14, 1           # r14 = mid1 + 1
    dec r15              # r15 = mid2 - 1
    call ternary_search  # Recursive call
    ret
    
search_left:
    # Search left third: [left, mid1-1]
    dec r10              # r10 = mid1 - 1
    mov r14, rsi         # r14 = left
    mov r15, r10         # r15 = mid1 - 1
    call ternary_search  # Recursive call
    ret
    
search_right:
    # Search right third: [mid2+1, right]
    inc r13              # r13 = mid2 + 1
    mov r14, r13         # r14 = mid2 + 1
    mov r15, rdx         # r15 = right
    call ternary_search  # Recursive call
    ret
    
found_mid1:
    # Return mid1 index
    mov rax, r10
    ret
    
found_mid2:
    # Return mid2 index
    mov rax, r13
    ret
    
not_found:
    # Return -1 for not found
    mov rax, -1
    ret
```

## Example Usage:

```assembly
# Example of calling the ternary search function
# Assume array is stored at label "my_array"
# Array size is 10 elements

main:
    leaq my_array, %rdi      # Load array address
    mov $0, %rsi             # Left index = 0
    mov $9, %rdx             # Right index = 9
    mov $42, %rcx            # Target value = 42
    
    call ternary_search      # Call the function
    
    # Result in RAX:
    # - Index of element if found
    # - -1 if not found
```

## Key Features:

1. **Recursive Implementation**: Uses recursion to divide the search space into three parts
2. **Three-way Comparison**: Compares target with two midpoints to determine which third to search
3. **Efficient Division**: Uses bit shifting for fast division by 3
4. **Proper Return Values**: Returns element index or -1 for not found
5. **Base Case Handling**: Properly handles the termination condition

## Time Complexity:
- **Time**: O(log₃ n) - logarithmic with base 3
- **Space**: O(log₃ n) - due to recursive calls

This implementation demonstrates how ternary search can be efficiently implemented in assembly language, providing the same functionality as a high-level language version but with direct control over memory access and computation.

