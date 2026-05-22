# Patience Sorting Algorithm in Assembly

Here's an implementation of the Patience Sorting algorithm in x86-64 Assembly language:

```assembly
.section .data
    # Array to be sorted
    array: .long 3, 1, 4, 1, 5, 9, 2, 6, 5, 3
    array_size: .long 10
    
    # Stack arrays for patience sorting
    stacks: .space 100  # Enough space for 10 elements
    stack_sizes: .space 40  # 10 stacks * 4 bytes each
    
    # Temporary array for merging
    temp_array: .space 40  # 10 * 4 bytes

.section .text
    .global _start

# Patience sort implementation
patience_sort:
    push %rbp
    mov %rsp, %rbp
    
    # Parameters: %rdi = array address, %rsi = array size
    mov %rdi, %r8      # Save array address
    mov %rsi, %r9      # Save array size
    
    # Initialize stacks
    xor %rax, %rax     # Clear rax
    mov $10, %rcx      # Maximum 10 stacks
    mov $stacks, %r10  # Stack pointer
    
init_stacks:
    movl $0, (%r10)    # Initialize stack size to 0
    add $4, %r10       # Move to next stack
    dec %rcx
    jnz init_stacks
    
    # Main patience sorting loop
    xor %rcx, %rcx     # i = 0
    mov %r9, %r11      # r11 = array_size
    
outer_loop:
    cmp %r11, %rcx
    jge outer_done
    
    # Get current element
    movl (%r8,%rcx,4), %eax
    
    # Find correct stack using binary search
    mov $0, %r12       # low = 0
    mov $10, %r13      # high = 10
    mov $0, %r14       # result = 0
    
binary_search:
    cmp %r13, %r12
    jle binary_search_done
    
    mov %r12, %r15
    add %r13, %r15
    shr $1, %r15       # mid = (low + high) / 2
    
    # Check if stack is empty
    movl stack_sizes(%r15,4), %r15
    cmp $0, %r15
    jz found_stack
    
    # Compare with top of stack
    mov $stacks, %r15
    movl stack_sizes(%r15,4), %r15
    dec %r15
    movl (%r15,4), %r15
    
    cmp %eax, %r15
    jl search_higher
    jg search_lower
    
found_stack:
    # Found stack, add element
    movl stack_sizes(%r15,4), %r15
    movl %eax, stacks(%r15,4)
    incl stack_sizes(%r15,4)
    jmp continue_outer
    
search_higher:
    mov %r15, %r12
    inc %r12
    jmp binary_search
    
search_lower:
    mov %r15, %r13
    dec %r13
    jmp binary_search
    
binary_search_done:
    # Add to stack (use first available)
    mov $0, %r15
    mov $stacks, %r12
    
find_empty_stack:
    movl stack_sizes(%r12), %r13
    cmp $0, %r13
    jz stack_found
    add $4, %r12
    inc %r15
    cmp $10, %r15
    jge stack_found
    
    jmp find_empty_stack
    
stack_found:
    # Add element to stack
    movl stack_sizes(%r12), %r13
    movl %eax, stacks(%r13,4)
    inc stack_sizes(%r12)
    
continue_outer:
    inc %rcx
    jmp outer_loop
    
outer_done:
    # Merge all stacks
    call merge_stacks
    
    # Restore stack pointer
    mov %rbp, %rsp
    pop %rbp
    ret

# Merge stacks function
merge_stacks:
    push %rbp
    mov %rsp, %rbp
    
    # Initialize merge process
    mov $0, %r12       # total elements processed
    
    # Find minimum element from all stacks
    mov $0, %r13       # current stack index
    mov $0, %r14       # minimum value
    mov $0, %r15       # minimum stack index
    
    # Find first minimum
    mov $stacks, %r10
    mov $0, %r8
    
find_first_min:
    movl stack_sizes(%r10), %r9
    cmp $0, %r9
    jz next_stack
    
    movl (%r10), %r14  # First element of stack
    mov %r13, %r15
    jmp found_first_min
    
next_stack:
    add $4, %r10
    inc %r13
    cmp $10, %r13
    jge merge_done
    
    jmp find_first_min
    
found_first_min:
    # Process all elements
    mov $0, %r13       # stack index
    mov $0, %r14       # element count
    
merge_loop:
    # Find minimum element
    mov $stacks, %r10
    mov $0, %r13
    mov $0, %r15       # minimum element
    
    find_min:
        movl stack_sizes(%r10), %r9
        cmp $0, %r9
        jz next_stack_merge
        
        movl (%r10), %r15
        jmp found_min
        
    next_stack_merge:
        add $4, %r10
        inc %r13
        cmp $10, %r13
        jge merge_done
        
        jmp find_min
    
    found_min:
        # Place minimum in temp array
        movl %r15, temp_array(%r14,4)
        inc %r14
        
        # Remove element from stack
        movl stack_sizes(%r10), %r9
        dec %r9
        movl %r9, stack_sizes(%r10)
        
        # Shift elements in stack
        mov %r10, %r12
        add $4, %r12
        movl (%r12), %r15
        movl %r15, (%r10)
        add $4, %r10
        dec %r9
        jnz shift_loop
        
    shift_loop:
        movl (%r12), %r15
        movl %r15, (%r10)
        add $4, %r12
        add $4, %r10
        dec %r9
        jnz shift_loop
        
        jmp merge_loop
    
merge_done:
    # Copy back to original array
    mov $0, %r13
    mov $array, %r10
    
copy_back:
    cmp %r14, %r13
    jge copy_done
    
    movl temp_array(%r13,4), (%r10)
    add $4, %r10
    inc %r13
    jmp copy_back
    
copy_done:
    mov %rbp, %rsp
    pop %rbp
    ret

# Main program entry point
_start:
    # Initialize array and call patience sort
    mov $array, %rdi
    mov $array_size, %rsi
    
    call patience_sort
    
    # Exit program
    mov $60, %rax      # sys_exit
    mov $0, %rdi       # exit status
    syscall
```

## Key Features of this Implementation:

1. **Stack Management**: Uses arrays to simulate stacks for patience sorting
2. **Binary Search**: Implements binary search to efficiently find the correct stack
3. **Merging**: Merges multiple sorted stacks back into a single sorted array
4. **Memory Management**: Properly manages stack pointers and element placement
5. **Assembly Syntax**: Uses x86-64 assembly syntax with proper calling conventions

## Algorithm Overview:

Patience sorting works by:
1. Placing elements into stacks (like solitaire)
2. Each stack maintains elements in decreasing order
3. Finding the correct stack using binary search
4. Merging all stacks to produce the final sorted array

This implementation demonstrates the core concepts of patience sorting in assembly language while maintaining proper stack management and memory operations.

