# Quick Select Algorithm in Assembly (x86-64)

```assembly
.section .text
.global quick_select
.type quick_select, @function

# quick_select(array, low, high, k)
# Parameters:
#   %rdi = array pointer
#   %rsi = low index
#   %rdx = high index  
#   %rcx = k (target position)
quick_select:
    # Base case: if low >= high, return
    cmpq %rdx, %rsi
    jge return_result
    
    # Partition the array and get pivot index
    movq %rsi, %r8      # Save low to r8
    movq %rdx, %r9      # Save high to r9
    call partition
    
    # Check if pivot is the answer
    cmpq %rcx, %rax
    je return_result
    
    # If k < pivot, search left subarray
    jl search_left
    
    # If k > pivot, search right subarray
    jmp search_right

search_left:
    # high = pivot - 1
    decq %rax
    movq %rax, %rdx
    movq %r8, %rsi      # low = original low
    jmp quick_select

search_right:
    # low = pivot + 1
    incq %rax
    movq %rax, %rsi
    movq %r9, %rdx      # high = original high
    jmp quick_select

return_result:
    # Return array[k] value
    movq (%rdi,%rcx,8), %rax
    ret

# partition(array, low, high)
# Returns pivot index in %rax
partition:
    # Save registers
    pushq %rbp
    pushq %rbx
    pushq %r12
    pushq %r13
    
    # Get pivot element (last element)
    movq %rdx, %r12     # high to r12
    movq (%rdi,%r12,8), %r13  # pivot = array[high]
    
    # i = low - 1
    movq %rsi, %rbx
    decq %rbx           # i = low - 1
    
    # j = low; while j < high
    movq %rsi, %r10     # j = low
    
partition_loop:
    cmpq %rdx, %r10     # j < high?
    jge partition_done
    
    # if array[j] <= pivot
    movq (%rdi,%r10,8), %rax
    cmpq %r13, %rax
    jg skip_swap
    
    # swap array[i+1] with array[j]
    incq %rbx           # i++
    movq (%rdi,%rbx,8), %rax
    movq (%rdi,%r10,8), %r8
    movq %r8, (%rdi,%rbx,8)
    movq %rax, (%rdi,%r10,8)
    
skip_swap:
    incq %r10           # j++
    jmp partition_loop

partition_done:
    # Place pivot in correct position
    incq %rbx           # i++
    movq (%rdi,%rbx,8), %rax
    movq (%rdi,%r12,8), %r8
    movq %r8, (%rdi,%rbx,8)
    movq %rax, (%rdi,%r12,8)
    
    # Return pivot index
    movq %rbx, %rax
    
    # Restore registers
    popq %r13
    popq %r12
    popq %rbx
    popq %rbp
    ret

.size quick_select, .-quick_select
```

## Usage Example

```assembly
# Example usage:
# Find the 3rd smallest element in array [7, 10, 4, 3, 20, 15]
# Array: [7, 10, 4, 3, 20, 15]
# Array address in %rdi
# low = 0, high = 5, k = 2 (0-indexed)
# Result should be 7 (3rd smallest element)

    leaq array(%rip), %rdi    # Load array address
    movq $0, %rsi             # low = 0
    movq $5, %rdx             # high = 5  
    movq $2, %rcx             # k = 2 (3rd element)
    call quick_select
    # Result in %rax
```

## Key Features

- **Partition Function**: Implements Lomuto partition scheme
- **Recursive Selection**: Recursively searches left or right subarray
- **In-place Operations**: Modifies array in-place without extra space
- **Time Complexity**: O(n) average case, O(n²) worst case
- **Space Complexity**: O(log n) due to recursion stack

## Register Usage

- `%rdi`: Array pointer
- `%rsi`: Low index  
- `%rdx`: High index
- `%rcx`: Target position (k)
- `%rax`: Return value/intermediate results
- `%r8-%r13`: Temporary storage for algorithm variables

This implementation demonstrates the core Quick Select algorithm using assembly language concepts while maintaining the essential logic of the divide-and-conquer approach.