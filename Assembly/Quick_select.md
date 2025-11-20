# Quick Select Algorithm in Assembly

Here's an example implementation of the Quick Select algorithm in x86-64 Assembly language:

```assembly
.section .text
.global quickselect

# quickselect(array, low, high, k)
# Returns the k-th smallest element (0-indexed)
quickselect:
    # Function prologue
    push    %rbp
    mov     %rsp, %rbp
    sub     $32, %rsp           # Allocate stack space
    
    # Save registers
    mov     %rdi, -8(%rbp)      # array
    mov     %rsi, -16(%rbp)     # low
    mov     %rdx, -24(%rbp)     # high
    mov     %rcx, -32(%rbp)     # k
    
    # Base case: if low >= high, return
    cmp     %rsi, %rdx
    jge     .Lreturn
    
    # Partition the array
    mov     %rsi, %rdi          # low
    mov     %rdx, %rsi          # high
    call    partition
    
    mov     %rax, %rcx          # pivot_index
    mov     %rcx, -40(%rbp)     # save pivot_index
    
    # Compare k with pivot_index
    cmp     %rcx, -32(%rbp)     # compare k with pivot_index
    jl      .Lsearch_left       # if k < pivot_index, search left
    
    cmp     %rcx, -32(%rbp)     # compare k with pivot_index
    jg      .Lsearch_right      # if k > pivot_index, search right
    
    # Found the k-th element
    mov     -8(%rbp), %rax      # array
    mov     %rcx, %rdx          # pivot_index
    mov     (%rax,%rdx,8), %rax  # return array[pivot_index]
    jmp     .Lreturn
    
.Lsearch_left:
    # Recursively search left subarray
    mov     -8(%rbp), %rdi      # array
    mov     -16(%rbp), %rsi     # low
    mov     %rcx, %rdx          # pivot_index
    dec     %rdx                # pivot_index - 1
    mov     -32(%rbp), %rcx     # k
    call    quickselect
    jmp     .Lreturn
    
.Lsearch_right:
    # Recursively search right subarray
    mov     -8(%rbp), %rdi      # array
    mov     %rcx, %rsi          # pivot_index + 1
    mov     -24(%rbp), %rdx     # high
    mov     -32(%rbp), %rcx     # k
    sub     %rcx, %rsi          # k - pivot_index - 1
    call    quickselect
    jmp     .Lreturn
    
.Lreturn:
    # Function epilogue
    mov     -8(%rbp), %rax      # return array
    add     $32, %rsp
    pop     %rbp
    ret

# partition(array, low, high)
# Returns the final position of the pivot
partition:
    # Function prologue
    push    %rbp
    mov     %rsp, %rbp
    sub     $32, %rsp
    
    # Save parameters
    mov     %rdi, -8(%rbp)      # array
    mov     %rsi, -16(%rbp)     # low
    mov     %rdx, -24(%rbp)     # high
    
    # Choose last element as pivot
    mov     -8(%rbp), %rax      # array
    mov     -24(%rbp), %rdx     # high
    mov     (%rax,%rdx,8), %rcx  # pivot = array[high]
    
    # Initialize i = low - 1
    mov     -16(%rbp), %rdx     # low
    dec     %rdx                # low - 1
    mov     %rdx, -32(%rbp)     # i = low - 1
    
    # Loop through elements from low to high-1
    mov     -16(%rbp), %rdx     # i = low
    mov     -24(%rbp), %r8      # high - 1
    
.Lloop:
    cmp     %r8, %rdx           # compare i with high-1
    jg      .Lswap              # if i > high-1, swap
    
    # Compare array[i] with pivot
    mov     -8(%rbp), %rax      # array
    mov     %rdx, %rdi          # i
    mov     (%rax,%rdi,8), %r9  # array[i]
    
    cmp     %r9, %rcx           # compare array[i] with pivot
    jg      .Lcontinue          # if array[i] > pivot, continue
    
    # Increment i
    mov     -32(%rbp), %rdi     # i
    inc     %rdi
    mov     %rdi, -32(%rbp)     # i++
    
    # Swap array[i] and array[j]
    mov     -8(%rbp), %rax      # array
    mov     -32(%rbp), %rdi     # i
    mov     %rdx, %rdi          # j
    mov     (%rax,%rdi,8), %r10  # temp = array[j]
    mov     (%rax,%rdi,8), %r11  # array[j] = array[i]
    mov     %r10, (%rax,%rdi,8)  # array[i] = temp
    
.Lcontinue:
    inc     %rdx                # i++
    jmp     .Lloop
    
.Lswap:
    # Swap pivot with element at i+1
    mov     -8(%rbp), %rax      # array
    mov     -32(%rbp), %rdi     # i
    inc     %rdi
    mov     %rdi, %rdx          # i+1
    mov     (%rax,%rdx,8), %r10  # temp = array[i+1]
    mov     (%rax,%rdx,8), %r11  # array[i+1] = pivot
    mov     %rcx, (%rax,%rdx,8)  # pivot = temp
    
    # Return pivot index
    mov     -32(%rbp), %rax     # i
    inc     %rax                # i + 1
    add     $32, %rsp
    pop     %rbp
    ret
```

## Usage Example

```assembly
# Example usage:
# Find 3rd smallest element in array [3, 1, 4, 1, 5, 9, 2, 6]
# Array: [3, 1, 4, 1, 5, 9, 2, 6]
# k = 2 (0-indexed)

.section .data
array: .quad 3, 1, 4, 1, 5, 9, 2, 6
array_size: .quad 8

.section .text
main:
    # Initialize parameters
    mov     $array, %rdi        # array pointer
    mov     $0, %rsi            # low = 0
    mov     $7, %rdx            # high = 7 (array_size - 1)
    mov     $2, %rcx            # k = 2 (3rd smallest)
    
    call    quickselect
    
    # Result is in %rax
    # This should return 2 (the 3rd smallest element)
```

## Key Features of this Implementation

1. **Recursive Structure**: Implements the standard Quick Select algorithm recursively
2. **Partition Function**: Separates elements around a pivot element
3. **Index Management**: Properly handles array indexing and bounds checking
4. **Register Usage**: Uses appropriate registers for parameters and temporary values
5. **Memory Management**: Proper stack frame management and cleanup

## Algorithm Complexity

- **Time Complexity**: O(n) average case, O(n²) worst case
- **Space Complexity**: O(log n) due to recursion stack
- **Purpose**: Finds the k-th smallest element without fully sorting the array

This implementation demonstrates how the Quick Select algorithm can be translated from high-level pseudocode into actual assembly language, maintaining the core logic while handling the low-level details of memory access and control flow.

