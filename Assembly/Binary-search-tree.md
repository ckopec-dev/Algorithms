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
    # %rdi = array pointer
    # %rsi = array size
    
    mov %rsi, %rcx          # Loop counter (outer loop)
    mov %rsi, %r8           # Copy size for inner loop
    
outer_loop:
    cmp $1, %rcx            # If size <= 1, done
    jle end_sort
    
    mov $1, %r9             # Inner loop counter
    mov %r8, %r10           # Copy array size
    
inner_loop:
    cmp %r9, %r10           # If inner counter >= array_size
    jge inner_loop_end
    
    # Compare adjacent elements
    mov (%rdi,%r9,4), %eax  # Load array[i]
    mov -4(%rdi,%r9,4), %ebx # Load array[i-1]
    
    cmp %eax, %ebx          # Compare array[i-1] and array[i]
    jle no_swap             # If array[i-1] <= array[i], no swap
    
    # Swap elements
    mov %eax, -4(%rdi,%r9,4) # array[i-1] = array[i]
    mov %ebx, (%rdi,%r9,4)   # array[i] = array[i-1]
    
no_swap:
    inc %r9                 # i++
    dec %r10                # Decrement inner counter
    jmp inner_loop
    
inner_loop_end:
    dec %rcx                # Decrement outer counter
    jmp outer_loop
    
end_sort:
    pop %rbp
    ret

# Main program
_start:
    # Call bubble sort
    mov $array, %rdi        # Array pointer
    mov array_size, %rsi    # Array size
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

- **Register usage**: %rax, %rbx, %rcx, %rdi, %rsi, %r8, %r9, %r10, %rbp
- **Memory addressing**: Using base register with offset and scale factor
- **Control flow**: Conditional jumps (cmp, jle, jmp)
- **Function calls**: Stack management with push/pop
- **System calls**: Exit syscall for program termination

The algorithm has O(n²) time complexity, similar to the high-level bubble sort implementation.

