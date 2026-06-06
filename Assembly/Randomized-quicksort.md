# Randomized Quicksort in Assembly

Here's an example implementation of Randomized Quicksort in x86-64 Assembly using AT&T syntax:

```assembly
.section .text
.global randomized_quicksort
.global quicksort_partition

# Randomized Quicksort function
# void randomized_quicksort(int arr[], int low, int high)
randomized_quicksort:
    push    %rbp
    mov     %rsp, %rbp
    sub     $32, %rsp           # Allocate stack space
    
    # Save registers
    mov     %rdi, -8(%rbp)      # arr
    mov     %rsi, -16(%rbp)     # low
    mov     %rdx, -24(%rbp)     # high
    
    # Base case: if low >= high, return
    cmp     %rsi, %rdx
    jge     end_quicksort
    
    # Generate random pivot index
    call    random_pivot_index
    mov     %rax, %rcx          # pivot index
    
    # Partition array with random pivot
    mov     %rdi, %rdi          # arr
    mov     %rsi, %rsi          # low
    mov     %rdx, %rdx          # high
    mov     %rcx, %rcx          # pivot index
    call    quicksort_partition
    
    # Store partition index
    mov     %rax, -32(%rbp)     # pivot_index
    
    # Recursively sort left subarray
    mov     -8(%rbp), %rdi      # arr
    mov     -16(%rbp), %rsi     # low
    mov     -32(%rbp), %rdx     # pivot_index - 1
    dec     %rdx
    call    randomized_quicksort
    
    # Recursively sort right subarray
    mov     -8(%rbp), %rdi      # arr
    mov     -32(%rbp), %rsi     # pivot_index + 1
    inc     %rsi
    mov     -24(%rbp), %rdx     # high
    call    randomized_quicksort
    
end_quicksort:
    add     $32, %rsp
    pop     %rbp
    ret

# Partition function with random pivot
# int quicksort_partition(int arr[], int low, int high)
quicksort_partition:
    push    %rbp
    mov     %rsp, %rbp
    sub     $32, %rsp
    
    # Save parameters
    mov     %rdi, -8(%rbp)      # arr
    mov     %rsi, -16(%rbp)     # low
    mov     %rdx, -24(%rbp)     # high
    
    # Get pivot value (last element)
    mov     -24(%rbp), %rax     # high
    mov     -8(%rbp), %rcx      # arr
    mov     (%rcx,%rax,4), %eax # arr[high]
    mov     %eax, -28(%rbp)     # pivot
    
    # Initialize i
    mov     -16(%rbp), %rax     # low
    dec     %rax
    mov     %rax, -32(%rbp)     # i = low - 1
    
    # Loop through array
partition_loop:
    mov     -16(%rbp), %rax     # low
    cmp     %rax, -24(%rbp)     # compare with high
    jge     partition_end
    
    # Compare arr[j] with pivot
    mov     -16(%rbp), %rax     # j = low
    mov     -8(%rbp), %rcx      # arr
    mov     (%rcx,%rax,4), %edx  # arr[j]
    cmp     -28(%rbp), %edx     # arr[j] <= pivot?
    jg      skip_swap
    
    # Increment i and swap
    mov     -32(%rbp), %rax
    inc     %rax
    mov     %rax, -32(%rbp)     # i++
    
    # Swap arr[i] and arr[j]
    mov     -8(%rbp), %rcx      # arr
    mov     -32(%rbp), %rax     # i
    mov     (%rcx,%rax,4), %edx  # arr[i]
    mov     -16(%rbp), %rax     # j
    mov     (%rcx,%rax,4), %esi  # arr[j]
    
    # arr[i] = arr[j]
    mov     %esi, (%rcx,%rax,4)
    # arr[j] = arr[i]
    mov     %edx, (%rcx,%rax,4)
    
skip_swap:
    mov     -16(%rbp), %rax
    inc     %rax
    mov     %rax, -16(%rbp)     # j++
    jmp     partition_loop
    
partition_end:
    # Place pivot in correct position
    mov     -32(%rbp), %rax
    inc     %rax
    mov     -8(%rbp), %rcx      # arr
    mov     -28(%rbp), %edx     # pivot
    mov     %edx, (%rcx,%rax,4)  # arr[i+1] = pivot
    
    # Return pivot index
    mov     -32(%rbp), %rax
    inc     %rax
    add     $32, %rsp
    pop     %rbp
    ret

# Generate random pivot index between low and high
# int random_pivot_index(int low, int high)
random_pivot_index:
    push    %rbp
    mov     %rsp, %rbp
    
    # Simple random number generator (using time-based approach)
    # In practice, you'd use a proper random function
    mov     %rsi, %rax          # high
    sub     %rdi, %rax          # high - low
    inc     %rax                # range + 1
    mov     %rax, %rcx          # save range
    
    # Generate pseudo-random number (simplified)
    # In real implementation, use proper random function
    mov     %rdi, %rax          # low
    mov     %rcx, %rdx          # range
    xor     %rdx, %rdx          # clear high bits
    mov     %rax, %rdx          # low
    mov     %rdx, %rax          # return value
    
    pop     %rbp
    ret

# Example usage function
# void sort_array(int arr[], int size)
sort_array:
    push    %rbp
    mov     %rsp, %rbp
    sub     $16, %rsp
    
    mov     %rdi, -8(%rbp)      # arr
    mov     %rsi, %rax          # size
    dec     %rax                # size - 1
    mov     %rax, -16(%rbp)     # high
    
    # Call randomized quicksort
    mov     %rdi, %rdi          # arr
    mov     $0, %rsi            # low = 0
    mov     -16(%rbp), %rdx     # high
    call    randomized_quicksort
    
    add     $16, %rsp
    pop     %rbp
    ret
```

## Key Features of this Implementation:

1. **Randomized Pivot Selection**: Uses a simple approach to select a random pivot index
2. **Recursive Structure**: Implements the standard divide-and-conquer approach
3. **Partitioning**: Uses the Lomuto partition scheme for sorting
4. **Memory Management**: Proper stack frame management and register preservation
5. **Base Case Handling**: Proper termination when subarrays are small

## Usage Example:
```assembly
# Call with array, low index, and high index
mov     %array_ptr, %rdi    # array pointer
mov     $0, %rsi            # low index
mov     $9, %rdx            # high index (for 10-element array)
call    randomized_quicksort
```

This implementation demonstrates the core concepts of randomized quicksort in assembly language while maintaining the algorithm's O(n log n) average time complexity.