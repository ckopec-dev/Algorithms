# Selection Sort in Assembly Language

Here's an example of the Selection Sort algorithm implemented in x86-64 Assembly language:

```assembly
.section .data
    array: .long 64, 34, 25, 12, 22, 11, 90
    array_size: .long 7

.section .text
    .global _start

selection_sort:
    # Parameters: %rdi = array pointer, %rsi = array size
    # Returns: sorted array in place
    
    push %rbp
    mov %rsp, %rbp
    
    # i = 0
    xor %rax, %rax          # i = 0
    mov %rsi, %rcx          # rcx = array_size (outer loop counter)
    dec %rcx                # rcx = array_size - 1
    
outer_loop:
    # Check if i >= array_size - 1
    cmp %rsi, %rax
    jge end_sort
    
    # Find minimum element from i to end
    mov %rax, %r8           # min_index = i
    mov %rax, %r9           # j = i
    
    # Inner loop: find minimum
    mov %rsi, %rcx          # rcx = array_size
    dec %rcx                # rcx = array_size - 1
    inc %rax                # i + 1
    
inner_loop:
    # Check if j >= array_size
    cmp %rsi, %r9
    jge swap_elements
    
    # Compare array[j] with array[min_index]
    mov (%rdi,%r9,4), %edx  # Load array[j]
    mov (%rdi,%r8,4), %esi  # Load array[min_index]
    
    # If array[j] < array[min_index], update min_index
    cmp %esi, %edx
    jge skip_update
    
    mov %r9, %r8            # min_index = j
    
skip_update:
    inc %r9                 # j++
    jmp inner_loop
    
swap_elements:
    # Swap array[i] and array[min_index]
    cmp %rax, %r8
    je skip_swap            # If i == min_index, no swap needed
    
    # Load values to swap
    mov (%rdi,%rax,4), %edx  # array[i]
    mov (%rdi,%r8,4), %esi   # array[min_index]
    
    # Perform swap
    mov %esi, (%rdi,%rax,4)  # array[i] = array[min_index]
    mov %edx, (%rdi,%r8,4)   # array[min_index] = array[i]
    
skip_swap:
    inc %rax                # i++
    jmp outer_loop
    
end_sort:
    pop %rbp
    ret

# Main program
_start:
    # Call selection sort
    lea array(%rip), %rdi   # Load array address
    mov array_size(%rip), %rsi  # Load array size
    call selection_sort
    
    # Exit program
    mov $60, %rax           # sys_exit
    mov $0, %rdi            # exit status
    syscall
```

## Algorithm Explanation

This selection sort implementation works as follows:

1. **Outer Loop**: Iterates through each position `i` from 0 to n-2
2. **Inner Loop**: Finds the minimum element in the remaining unsorted portion
3. **Swap**: Places the minimum element at position `i`
4. **Repeat**: Continue until the entire array is sorted

## Key Features

- **Time Complexity**: O(n²) in all cases
- **Space Complexity**: O(1) - sorts in place
- **In-place sorting**: No additional memory allocation needed
- **Stable**: Maintains relative order of equal elements

## Assembly-Specific Notes

- Uses `mov` instructions for data movement
- Implements loops with conditional jumps (`cmp`, `jge`, `jmp`)
- Array indexing uses scaled addressing (`(%rdi,%r9,4)` for 4-byte integers)
- Proper function call conventions with stack management
- Uses registers efficiently: `%rax`, `%rbx`, `%rcx`, `%rdx`, `%rsi`, `%rdi`, `%r8`, `%r9`

The algorithm sorts the array `[64, 34, 25, 12, 22, 11, 90]` into `[11, 12, 22, 25, 34, 64, 90]`.

