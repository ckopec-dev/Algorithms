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
    
    mov %rsi, %rcx          # Loop counter (outer loop)
    mov $0, %r8             # i = 0
    
outer_loop:
    cmp $0, %rcx            # if (n == 0) return
    je end_sort
    
    mov $0, %r9             # j = 0
    mov %rcx, %r10          # temp = n
    dec %r10                # temp = n - 1
    
inner_loop:
    cmp %r9, %r10           # if (j >= temp) break
    jge inner_end
    
    # Compare array[j] and array[j+1]
    mov (%rdi,%r9,4), %eax  # load array[j]
    mov 4(%rdi,%r9,4), %ebx # load array[j+1]
    
    cmp %ebx, %eax          # if (array[j] > array[j+1])
    jle no_swap
    
    # Swap elements
    mov %ebx, (%rdi,%r9,4)  # array[j] = array[j+1]
    mov %eax, 4(%rdi,%r9,4) # array[j+1] = array[j]
    
no_swap:
    inc %r9                 # j++
    jmp inner_loop
    
inner_end:
    dec %rcx                # n--
    jmp outer_loop
    
end_sort:
    pop %rbp
    ret

# Main program
_start:
    # Call bubble sort
    lea array(%rip), %rdi   # load array address
    mov array_size(%rip), %rsi # load array size
    call bubble_sort
    
    # Exit program
    mov $60, %rax           # sys_exit
    mov $0, %rdi            # exit status
    syscall
```

## Algorithm Explanation:

This assembly implementation performs a bubble sort on an array of integers:

1. **Outer Loop**: Controls the number of passes through the array
2. **Inner Loop**: Compares adjacent elements and swaps them if they're in wrong order
3. **Swapping**: Uses registers to temporarily store values during swap operation
4. **Termination**: Decreases the pass count after each complete iteration

## Key Assembly Concepts Demonstrated:

- **Register usage** for variables and temporary storage
- **Memory addressing** with base + index + scale
- **Conditional jumps** for loop control
- **Function calling conventions** (parameter passing, return)
- **Stack management** (push/pop for function calls)

The algorithm has O(n²) time complexity, which is typical for bubble sort, and sorts the array in ascending order.

