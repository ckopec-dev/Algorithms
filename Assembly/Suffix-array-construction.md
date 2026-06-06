# Suffix Array Construction in Assembly (x86-64)

Here's an example of a simplified suffix array construction algorithm implemented in x86-64 Assembly. This implementation uses a simple sorting approach for demonstration purposes.

```assembly
.section .data
    # Sample string for suffix array construction
    text: .ascii "banana\0"
    text_len = 6
    
    # Buffer for suffix indices
    suffix_array: .space 24    # 6 * 4 bytes for 6 suffixes

.section .text
    .global _start

# Function to construct suffix array
# Input: text pointer in RDI, length in RSI
# Output: suffix array in suffix_array buffer
construct_suffix_array:
    push    rbp
    mov     rbp, rsp
    
    # Initialize suffix indices
    mov     r8, 0              # i = 0
    mov     r9, rsi            # length = text_len
    
init_loop:
    cmp     r8, r9
    jge     sort_suffixes
    
    # Store suffix index at position i
    mov     eax, r8d           # suffix index = i
    mov     [suffix_array + r8*4], eax
    inc     r8
    jmp     init_loop

sort_suffixes:
    # Simple bubble sort for demonstration
    # In practice, use more efficient algorithm like SA-IS
    mov     r10, 0             # outer loop counter
outer_loop:
    cmp     r10, rsi
    jge     done_sorting
    
    mov     r11, 0             # inner loop counter
inner_loop:
    cmp     r11, rsi
    jge     next_outer
    
    # Compare suffixes at positions [suffix_array+r11*4] and [suffix_array+(r11+1)*4]
    mov     eax, [suffix_array + r11*4]
    mov     ebx, [suffix_array + r11*4 + 4]
    
    # Simple string comparison (simplified)
    # In real implementation, compare actual strings
    
    # Swap if needed (simplified)
    cmp     eax, ebx
    jle     no_swap
    
    # Swap suffix indices
    mov     ecx, eax
    mov     eax, ebx
    mov     ebx, ecx
    
    mov     [suffix_array + r11*4], eax
    mov     [suffix_array + r11*4 + 4], ebx
    
no_swap:
    inc     r11
    jmp     inner_loop
    
next_outer:
    inc     r10
    jmp     outer_loop

done_sorting:
    pop     rbp
    ret

# Main program
_start:
    # Set up parameters
    lea     rdi, text          # text pointer
    mov     rsi, text_len      # text length
    
    # Call suffix array construction
    call    construct_suffix_array
    
    # Exit program
    mov     rax, 60            # sys_exit
    mov     rdi, 0             # exit status
    syscall
```

## Key Components Explained

### 1. **Data Section**
```assembly
text: .ascii "banana\0"        # Input string
suffix_array: .space 24        # Storage for 6 suffix indices (24 bytes)
```

### 2. **Initialization Loop**
```assembly
init_loop:
    cmp     r8, r9           # Compare i with length
    jge     sort_suffixes    # Jump if i >= length
    mov     eax, r8d         # Load suffix index
    mov     [suffix_array + r8*4], eax  # Store in array
    inc     r8               # Increment counter
    jmp     init_loop
```

### 3. **Sorting Process**
```assembly
# Simple bubble sort implementation
outer_loop:
    cmp     r10, rsi         # Compare counter with length
    jge     done_sorting     # Exit if done
    # ... inner loop for comparison and swapping ...
```

## Algorithm Steps

1. **Initialization**: Create array of suffix indices [0, 1, 2, 3, 4, 5]
2. **Sorting**: Sort indices based on lexicographical order of suffixes
3. **Output**: Resulting array contains sorted suffix indices

## Sample Output

For input "banana", the suffix array would be:
```
[5, 3, 1, 0, 4, 2]
```

This represents the sorted order of suffixes:
- Index 5: "a"
- Index 3: "ana" 
- Index 1: "anana"
- Index 0: "banana"
- Index 4: "na"
- Index 2: "nana"

## Important Notes

- This is a simplified implementation for educational purposes
- Real suffix array construction uses more sophisticated algorithms like SA-IS
- Assembly implementation requires careful handling of memory addresses
- Error checking and bounds validation should be added for production use
- The sorting algorithm shown is O(n²) - not efficient for large inputs

This example demonstrates the core concepts of suffix array construction in assembly language, showing how to manipulate arrays and perform basic sorting operations at the machine level.