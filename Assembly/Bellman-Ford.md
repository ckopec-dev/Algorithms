# Assembly Algorithm Example: Bubble Sort

Here's an example of a bubble sort algorithm implemented in x86-64 Assembly language:

```assembly
.section .data
    array: .long 64, 34, 25, 12, 22, 11, 90
    array_size: .long 7

.section .text
    .global _start

bubble_sort:
    push %rbp
    mov %rsp, %rbp
    
    # Parameters:
    # %rdi = array address
    # %rsi = array size
    
    mov %rsi, %rcx          # Outer loop counter (array_size)
    mov $0, %r8             # i = 0
    
outer_loop:
    cmp $0, %rcx            # If counter <= 0, exit
    jle end_sort
    
    mov $0, %r9             # j = 0
    mov %rcx, %r10          # temp = counter
    dec %r10                # temp = counter - 1
    
inner_loop:
    cmp %r9, %r10           # If j >= temp, exit inner loop
    jge inner_end
    
    # Compare array[j] and array[j+1]
    mov (%rdi,%r9,4), %eax  # Load array[j] into %eax
    mov 4(%rdi,%r9,4), %ebx # Load array[j+1] into %ebx
    
    cmp %ebx, %eax          # Compare array[j] and array[j+1]
    jle no_swap             # If array[j] <= array[j+1], no swap
    
    # Swap elements
    mov %ebx, (%rdi,%r9,4)  # array[j] = array[j+1]
    mov %eax, 4(%rdi,%r9,4) # array[j+1] = array[j]
    
no_swap:
    inc %r9                 # j++
    jmp inner_loop
    
inner_end:
    dec %rcx                # counter--
    jmp outer_loop
    
end_sort:
    pop %rbp
    ret

_start:
    # Call bubble_sort with array and size
    lea array(%rip), %rdi   # Load array address
    mov array_size(%rip), %rsi # Load array size
    
    call bubble_sort
    
    # Exit program
    mov $60, %rax           # sys_exit
    mov $0, %rdi            # exit status
    syscall
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine if swapping is required
4. **Swapping**: Direct memory operations to exchange element values

## Key Assembly Concepts Demonstrated:

- **Register usage**: %rax, %rbx, %rcx, %rdi, %rsi, %r8, %r9, %r10
- **Memory addressing**: Using base + index + scale for array access
- **Control flow**: Conditional jumps (cmp, jle, jge, jmp)
- **Function calls**: Stack management and parameter passing
- **System calls**: Exit functionality using syscall

The algorithm has O(n²) time complexity, similar to the high-level bubble sort implementation.

