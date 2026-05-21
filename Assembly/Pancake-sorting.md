# Pancake Sorting Algorithm in Assembly

Here's an implementation of the Pancake sorting algorithm in x86-64 Assembly language:

```assembly
.section .data
    # Sample array to sort
    array: .long 4, 3, 2, 1, 5
    array_size: .long 5

.section .text
    .global _start

# Pancake sort implementation
# Parameters: array pointer in RDI, array size in RSI
# Returns: sorted array in place
pancake_sort:
    push rbp
    mov rbp, rsp
    
    # Initialize variables
    mov r12, rdi        # array pointer
    mov r13, rsi        # array size
    mov r14, 0          # flip_count
    
    # Main sorting loop
sort_loop:
    # Base case: if size <= 1, we're done
    cmp r13, 1
    jle done
    
    # Find maximum element in current array
    call find_max_index
    mov r15, rax        # max_index
    
    # If max element is already at the end, skip
    cmp r15, r13
    je skip_flip
    
    # Flip to bring max element to front
    call flip_array
    inc r14             # increment flip count
    
    # Flip to bring max element to correct position
    dec r13             # reduce size
    call flip_array
    inc r14             # increment flip count
    
skip_flip:
    dec r13             # reduce size
    jmp sort_loop
    
done:
    pop rbp
    ret

# Find index of maximum element in array
# Parameters: array pointer in RDI, array size in RSI
# Returns: index of maximum element in RAX
find_max_index:
    push rbp
    mov rbp, rsp
    
    mov rax, 0          # max_index
    mov r8, 0           # current_index
    mov r9, [rdi]       # max_value = first element
    
find_loop:
    cmp r8, rsi
    jge find_done
    
    mov r10, [rdi + r8*4]  # current_element
    cmp r10, r9
    jle find_continue
    
    mov r9, r10         # update max_value
    mov rax, r8         # update max_index
    
find_continue:
    inc r8
    jmp find_loop
    
find_done:
    pop rbp
    ret

# Flip array from index 0 to index
# Parameters: array pointer in RDI, index in RSI
flip_array:
    push rbp
    mov rbp, rsp
    
    mov r8, 0           # left_index
    mov r9, rsi         # right_index
    
flip_loop:
    cmp r8, r9
    jge flip_done
    
    # Swap elements at positions r8 and r9
    mov r10, [rdi + r8*4]
    mov r11, [rdi + r9*4]
    
    mov [rdi + r8*4], r11
    mov [rdi + r9*4], r10
    
    inc r8
    dec r9
    jmp flip_loop
    
flip_done:
    pop rbp
    ret

# Main program entry point
_start:
    # Initialize array
    mov rdi, array      # array pointer
    mov rsi, array_size # array size
    
    # Call pancake sort
    call pancake_sort
    
    # Exit program
    mov rax, 60         # sys_exit
    mov rdi, 0          # exit status
    syscall
```

## Algorithm Explanation

This assembly implementation follows the pancake sorting algorithm with these key steps:

1. **Find Maximum**: Locate the index of the largest element in the current unsorted portion
2. **Flip to Front**: Bring the maximum element to the front using a flip operation
3. **Flip to Correct Position**: Bring the maximum element to its final sorted position
4. **Reduce Size**: Decrease the size of the unsorted portion and repeat

## Key Features

- **Register Usage**: 
  - RDI: Array pointer
  - RSI: Array size
  - R12-R15: Temporary variables
- **Memory Operations**: Direct array element access and swapping
- **Control Flow**: Loop structures for sorting and element comparison
- **Function Calls**: Modular design with separate functions for max finding and flipping

## Time Complexity
- **Worst Case**: O(n²) flips
- **Best Case**: O(n) flips
- **Space Complexity**: O(1) - in-place sorting

This implementation demonstrates how pancake sorting can be efficiently implemented at the assembly level, showcasing the fundamental operations of array manipulation and control flow.

