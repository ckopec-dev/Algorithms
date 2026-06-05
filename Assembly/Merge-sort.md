# Merge Sort in Assembly Language

Here's an example of merge sort implemented in x86-64 Assembly language:

```assembly
.section .data
    .align 8
    array: .quad 64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42
    array_size: .quad 11
    temp_array: .space 88    # 11 * 8 bytes for 64-bit integers

.section .text
    .global _start

# merge_sort function
# Parameters: rdi = array pointer, rsi = left index, rdx = right index
merge_sort:
    # Base case: if left >= right, return
    cmp rsi, rdx
    jge merge_sort_end
    
    # Calculate mid point
    mov rsi, rdx
    sub rsi, rsi, rsi    # rsi = right - left
    shr rsi, 1           # rsi = (right - left) / 2
    add rsi, rsi, rsi    # rsi = (right - left) / 2 * 2 = (right - left)
    add rsi, rsi, rsi    # rsi = (right - left) * 2
    mov rax, rsi         # rax = (right - left) * 2
    add rax, rsi, rsi    # rax = (right - left) * 3
    shr rax, 1           # rax = (right - left) * 1.5
    
    # More precise calculation of mid
    mov rax, rsi         # rax = left
    add rax, rax, rdx    # rax = left + right
    shr rax, 1           # rax = (left + right) / 2 (mid)
    
    # Recursive call for left half
    push rdi             # save array pointer
    push rsi             # save left index
    push rdx             # save right index
    
    mov r8, rsi          # r8 = left
    mov r9, rax          # r9 = mid
    sub r9, r9, 1        # r9 = mid - 1
    call merge_sort
    
    # Recursive call for right half
    mov r8, rax          # r8 = mid
    add r8, r8, 1        # r8 = mid + 1
    mov r9, rdx          # r9 = right
    call merge_sort
    
    # Merge the two sorted halves
    pop rdx              # restore right index
    pop rsi              # restore left index
    pop rdi              # restore array pointer
    
    call merge
    
merge_sort_end:
    ret

# merge function
# Parameters: rdi = array pointer, rsi = left index, rdx = mid index, rcx = right index
merge:
    # Copy left subarray to temp
    mov r8, rsi          # r8 = left
    mov r9, rdx          # r9 = mid
    mov r10, rdx         # r10 = mid
    add r10, r10, 1      # r10 = mid + 1
    mov r11, rdx         # r11 = mid
    add r11, r11, rdx    # r11 = mid * 2
    sub r11, r11, rsi    # r11 = mid * 2 - left
    
    # Copy left subarray to temp_array
    mov r12, rsi         # r12 = i = left
    mov r13, 0           # r13 = temp_index = 0
    
copy_left_loop:
    cmp r12, r10         # compare i with mid
    jg copy_left_end
    
    # Calculate array offset
    mov rax, r12
    shl rax, 3           # rax = i * 8 (8 bytes per integer)
    mov r14, rax         # r14 = offset
    
    # Copy from array to temp_array
    mov rax, [rdi + r14] # load array[i]
    mov [temp_array + r13], rax # store to temp_array[temp_index]
    
    inc r12              # i++
    inc r13              # temp_index++
    jmp copy_left_loop
    
copy_left_end:
    # Copy right subarray to temp
    mov r12, r10         # r12 = mid + 1
    mov r13, r10         # r13 = temp_index = mid
    add r13, r13, 1      # r13 = mid + 1
    
copy_right_loop:
    cmp r12, rdx         # compare i with right
    jg copy_right_end
    
    # Calculate array offset
    mov rax, r12
    shl rax, 3           # rax = i * 8
    mov r14, rax         # r14 = offset
    
    # Copy from array to temp_array
    mov rax, [rdi + r14] # load array[i]
    mov [temp_array + r13], rax # store to temp_array[temp_index]
    
    inc r12              # i++
    inc r13              # temp_index++
    jmp copy_right_loop
    
copy_right_end:
    # Merge back to original array
    mov r12, rsi         # r12 = left
    mov r13, rsi         # r13 = i = left
    mov r14, r10         # r14 = mid
    add r14, r14, 1      # r14 = mid + 1
    mov r15, rdx         # r15 = right
    
merge_loop:
    cmp r13, r14         # compare i with mid
    jge merge_right
    cmp r14, r15         # compare mid with right
    jg merge_left
    
    # Compare elements
    mov rax, [temp_array + r13]  # load temp_array[i]
    mov rbx, [temp_array + r14]  # load temp_array[mid]
    
    cmp rax, rbx
    jle merge_left
    
merge_right:
    # Take from right subarray
    mov rax, [temp_array + r14]
    mov [rdi + r12], rax
    
    inc r14              # mid++
    inc r12              # left++
    jmp merge_loop
    
merge_left:
    # Take from left subarray
    mov rax, [temp_array + r13]
    mov [rdi + r12], rax
    
    inc r13              # i++
    inc r12              # left++
    jmp merge_loop
    
merge_end:
    ret

# Main function
_start:
    # Initialize registers
    mov rdi, array       # array pointer
    mov rsi, 0           # left index = 0
    mov rdx, array_size  # right index = size - 1
    sub rdx, rdx, 1      # rdx = size - 1
    
    # Call merge_sort
    call merge_sort
    
    # Exit program
    mov rax, 60          # sys_exit
    mov rdi, 0           # exit status
    syscall
```

## Key Features of This Implementation:

1. **Recursive Structure**: Uses recursive calls to divide the array into smaller subarrays
2. **Memory Management**: Uses a temporary array for merging operations
3. **Index Tracking**: Properly manages left, middle, and right indices
4. **Merge Logic**: Implements the core merge operation to combine sorted subarrays
5. **64-bit Architecture**: Uses x86-64 registers and calling conventions

## How It Works:

1. **Divide**: Split the array into two halves recursively
2. **Conquer**: Sort each half using recursion
3. **Combine**: Merge the two sorted halves back together

## Notes:

- This is a simplified version for educational purposes
- Actual implementation would need more robust error handling
- Memory management and optimization could be improved
- The code assumes 64-bit integers and uses appropriate addressing modes

This example demonstrates the fundamental concepts of merge sort implemented at the assembly level, showing how the algorithm's recursive nature translates into function calls and memory operations.