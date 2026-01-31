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
    mov %rdi, %rax          # Array pointer
    
outer_loop:
    cmp $1, %rcx            # If counter <= 1, done
    jle end_sort
    
    mov %rcx, %r8           # Save outer loop counter
    mov $1, %r9             # Inner loop counter
    
inner_loop:
    cmp %r9, %rcx           # If inner counter >= outer counter, break
    jge inner_end
    
    # Compare adjacent elements
    mov (%rax), %r10        # Load array[i]
    mov 8(%rax), %r11       # Load array[i+1]
    
    cmp %r11, %r10          # Compare array[i] and array[i+1]
    jle no_swap             # If array[i] <= array[i+1], no swap needed
    
    # Swap elements
    mov %r11, (%rax)        # array[i] = array[i+1]
    mov %r10, 8(%rax)       # array[i+1] = array[i]
    
no_swap:
    add $8, %rax            # Move to next element
    inc %r9                 # Increment inner counter
    jmp inner_loop
    
inner_end:
    mov %r8, %rcx           # Restore outer loop counter
    dec %rcx                # Decrement outer counter
    mov %rdi, %rax          # Reset array pointer
    jmp outer_loop
    
end_sort:
    pop %rbp
    ret

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

## Algorithm Explanation

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if needed
3. **Comparison**: Uses conditional jumps to determine if swapping is required
4. **Swapping**: Direct memory operations to exchange element values

## Key Assembly Concepts Shown

- **Register usage**: `%rax`, `%rbx`, `%rcx`, `%rdx`, `%rsi`, `%rdi`, `%r8`, `%r9`
- **Memory addressing**: Direct and indirect addressing modes
- **Control flow**: Conditional jumps (`jle`, `jge`, `jmp`)
- **Function calls**: Stack management and parameter passing
- **System calls**: Exit routine using `syscall`

The algorithm has O(n²) time complexity, which is typical for bubble sort, but implemented in low-level assembly instructions.

