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
    # Check if i < array_size - 1
    cmp %rsi, %rax
    jge end_sort            # if i >= array_size, exit
    
    # Find minimum element from remaining array
    mov %rax, %r8           # min_index = i
    mov %rax, %r9           # j = i
    
    # Inner loop: find minimum element
    inc %r9                 # j = i + 1
inner_loop:
    # Check if j < array_size
    cmp %rsi, %r9
    jge found_min           # if j >= array_size, found minimum
    
    # Compare array[j] with array[min_index]
    mov (%rdi,%r9,4), %edx  # load array[j] into %edx
    mov (%rdi,%r8,4), %ecx  # load array[min_index] into %ecx
    
    cmp %ecx, %edx          # compare array[j] and array[min_index]
    jge skip_swap           # if array[j] >= array[min_index], skip swap
    
    # Found new minimum, update min_index
    mov %r9, %r8            # min_index = j
    
skip_swap:
    inc %r9                 # j++
    jmp inner_loop          # continue inner loop
    
found_min:
    # If min_index != i, swap elements
    cmp %rax, %r8
    je skip_swap_elements   # if min_index == i, no swap needed
    
    # Swap array[i] and array[min_index]
    mov (%rdi,%rax,4), %edx # load array[i] into %edx
    mov (%rdi,%r8,4), %ecx  # load array[min_index] into %ecx
    
    # Swap the values
    mov %ecx, (%rdi,%rax,4) # array[i] = array[min_index]
    mov %edx, (%rdi,%r8,4)  # array[min_index] = array[i]
    
skip_swap_elements:
    # i++
    inc %rax
    jmp outer_loop          # continue outer loop
    
end_sort:
    pop %rbp
    ret

# Main program
_start:
    # Call selection_sort
    lea array(%rip), %rdi   # load array address
    mov array_size(%rip), %rsi  # load array size
    
    call selection_sort
    
    # Exit program
    mov $60, %rax           # sys_exit
    mov $0, %rdi            # exit status
    syscall
```

## Algorithm Explanation

This selection sort implementation works as follows:

1. **Outer Loop**: Iterates through each position in the array (i from 0 to n-2)
2. **Inner Loop**: Finds the minimum element in the remaining unsorted portion
3. **Swap**: If a smaller element is found, swap it with the element at position i
4. **Repeat**: Continue until the entire array is sorted

## Key Features

- **Time Complexity**: O(n²) in all cases
- **Space Complexity**: O(1) - sorts in place
- **Algorithm**: Finds the minimum element and places it at the beginning
- **Stability**: Not stable (doesn't preserve relative order of equal elements)

## Register Usage

- `%rdi`: Array pointer
- `%rsi`: Array size
- `%rax`: Outer loop counter (i)
- `%r8`: Minimum index
- `%r9`: Inner loop counter (j)
- `%edx`, `%ecx`: Temporary storage for element values

The algorithm maintains the selection sort property where in each iteration, the smallest remaining element is selected and placed in its correct position.