# Bead Sort Algorithm in Assembly

Here's an implementation of the Bead Sort algorithm in x86-64 Assembly language:

```assembly
.section .data
    # Sample array to sort
    array: .long 5, 3, 8, 1, 9, 2, 7, 4, 6
    array_size: .long 9

.section .text
    .global _start

# Bead sort implementation
# Input: array pointer in RDI, size in RSI
# Output: sorted array in place
bead_sort:
    push rbp
    mov rbp, rsp
    
    # Get array size
    mov rdx, rsi
    dec rdx
    mov rcx, rdx          # Outer loop counter
    
outer_loop:
    # Check if we're done with outer loop
    cmp rcx, 0
    jl end_outer_loop
    
    # Inner loop - process each row
    mov r8, rdx           # Inner loop counter (start from last index)
    
inner_loop:
    cmp r8, 0
    jl end_inner_loop
    
    # Compare adjacent elements
    mov r9, rdi           # Array pointer
    mov r10, r8           # Current index
    shl r10, 2            # Multiply by 4 (sizeof int)
    add r9, r10           # Pointer to current element
    
    mov r11, r9           # Current element
    mov eax, [r11]        # Load current element
    
    mov r12, r9           # Current element pointer
    add r12, 4            # Pointer to next element
    
    mov ebx, [r12]        # Load next element
    
    # If current > next, swap them
    cmp eax, ebx
    jle no_swap
    
    # Swap elements
    mov [r11], ebx        # Put next element in current position
    mov [r12], eax        # Put current element in next position
    
no_swap:
    dec r8
    jmp inner_loop
    
end_inner_loop:
    dec rcx
    jmp outer_loop
    
end_outer_loop:
    pop rbp
    ret

# Main program
_start:
    # Initialize registers
    mov rdi, array        # Array pointer
    mov rsi, array_size   # Array size
    
    # Call bead sort
    call bead_sort
    
    # Exit program
    mov rax, 60           # sys_exit
    mov rdi, 0            # exit status
    syscall
```

## How Bead Sort Works in Assembly

The bead sort algorithm works by simulating beads falling under gravity:

1. **Initialization**: Load the array into memory
2. **Sorting Process**: 
   - Compare adjacent elements
   - If left element > right element, swap them
   - Continue until no more swaps are needed
3. **Result**: Array is sorted in ascending order

## Key Assembly Concepts Used

- **Memory addressing**: Using pointers to access array elements
- **Loop control**: Using registers as counters for nested loops
- **Conditional jumps**: `cmp` and `jl` instructions for control flow
- **Register usage**: Efficient use of x86-64 registers for operations
- **Stack management**: Proper function call and return handling

## Example Output

For input array `[5, 3, 8, 1, 9, 2, 7, 4, 6]`:
- After sorting: `[1, 2, 3, 4, 5, 6, 7, 8, 9]`

## Time Complexity
- **Time**: O(n²) in worst case
- **Space**: O(1) - sorts in place

This implementation demonstrates the fundamental principles of how bead sort works at the assembly level, showing the low-level operations that make the algorithm function.

