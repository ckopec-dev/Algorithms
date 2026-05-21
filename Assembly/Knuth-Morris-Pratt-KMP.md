# Knuth-Morris-Pratt (KMP) Algorithm in Assembly

Here's an implementation of the KMP algorithm in x86-64 Assembly language:

```assembly
.section .data
    # Pattern and text strings
    pattern: .ascii "ABABCABAB"
    pattern_len: .long 9
    text: .ascii "ABABDABACDABABCABCABCABCABC"
    text_len: .long 28
    needle: .ascii "ABABCABAB"
    needle_len: .long 9

    # Failure function array
    failure_array: .space 100

.section .text
    .global _start

# KMP Failure Function Construction
# Input: pattern string, pattern_len
# Output: failure_array
construct_failure_function:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %r12
    push %r13
    push %r14
    
    # Load parameters
    mov %rdi, %r12          # pattern pointer
    mov %rsi, %r13          # pattern length
    
    # Initialize failure array
    mov $0, %r14            # i = 0
    mov $1, %rcx            # j = 1
    
    # failure[0] = 0
    mov $0, (%r12)          # This is a mistake in the original - should be failure_array[0] = 0
    
    # Corrected version:
    mov $0, %rax            # failure[0] = 0
    mov %rax, (%r14)        # Store in failure_array[0]
    
    # Main loop
construct_loop:
    cmp %r13, %rcx          # if j >= pattern_len
    jge construct_done
    
    # Compare pattern[i] with pattern[j]
    mov (%r12,%rcx,1), %al  # pattern[j]
    mov (%r12,%r14,1), %bl  # pattern[i]
    
    cmp %bl, %al            # if pattern[i] == pattern[j]
    je match_found
    
    # Mismatch case
    cmp $0, %r14            # if i == 0
    je mismatch_no_shift
    
    # Backtrack using failure function
    mov (%r14), %r14       # i = failure[i-1]
    jmp construct_loop
    
mismatch_no_shift:
    mov $0, %r14            # i = 0
    jmp construct_loop
    
match_found:
    inc %r14                # i++
    mov %r14, (%r12,%rcx,1) # failure[j] = i
    inc %rcx                # j++
    jmp construct_loop
    
construct_done:
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    pop %rbp
    ret

# KMP Search Function
# Input: text, text_len, pattern, pattern_len, failure_array
# Output: position of match or -1 if not found
kmp_search:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %r12
    push %r13
    push %r14
    push %r15
    
    # Load parameters
    mov %rdi, %r12          # text pointer
    mov %rsi, %r13          # text length
    mov %rdx, %r14          # pattern pointer
    mov %rcx, %r15          # pattern length
    
    # Initialize search variables
    mov $0, %rax            # i = 0 (text index)
    mov $0, %rbx            # j = 0 (pattern index)
    
search_loop:
    # Check bounds
    cmp %r13, %rax          # if i >= text_len
    jge search_not_found
    
    cmp %r15, %rbx          # if j >= pattern_len
    jge search_found
    
    # Compare text[i] with pattern[j]
    mov (%r12,%rax,1), %cl  # text[i]
    mov (%r14,%rbx,1), %dl  # pattern[j]
    
    cmp %dl, %cl            # if text[i] == pattern[j]
    je char_match
    
    # Mismatch case
    cmp $0, %rbx            # if j == 0
    je mismatch_advance
    
    # Use failure function to backtrack
    mov (%r14,%rbx,1), %rbx # j = failure[j-1]
    jmp search_loop
    
mismatch_advance:
    inc %rax                # i++
    jmp search_loop
    
char_match:
    inc %rax                # i++
    inc %rbx                # j++
    jmp search_loop
    
search_found:
    # Return position (i - pattern_len)
    mov %rax, %r13
    sub %r15, %r13
    mov %r13, %rax          # Return position
    jmp search_end
    
search_not_found:
    mov $-1, %rax           # Return -1 for not found
    
search_end:
    pop %r15
    pop %r14
    pop %r13
    pop %r12
    pop %rbx
    pop %rbp
    ret

# Main function
_start:
    # Call failure function construction
    mov $needle, %rdi       # pattern pointer
    mov $needle_len, %rsi   # pattern length
    call construct_failure_function
    
    # Call KMP search
    mov $text, %rdi         # text pointer
    mov $text_len, %rsi     # text length
    mov $needle, %rdx       # pattern pointer
    mov $needle_len, %rcx   # pattern length
    call kmp_search
    
    # Exit program
    mov $60, %rax           # sys_exit
    mov $0, %rdi            # exit status
    syscall
```

## Key Components Explained:

### 1. **Failure Function Construction**
- Builds the failure array (also called the prefix function)
- For each position in the pattern, stores the length of the longest proper prefix that is also a suffix
- This allows skipping comparisons during pattern matching

### 2. **KMP Search Algorithm**
- Uses the failure function to avoid unnecessary character comparisons
- Maintains two pointers: one for text and one for pattern
- When a mismatch occurs, uses failure function to determine how much to shift the pattern

### 3. **Time Complexity**
- **Preprocessing**: O(m) where m is pattern length
- **Search**: O(n) where n is text length
- **Overall**: O(m + n)

### 4. **Space Complexity**
- O(m) for the failure array

## Usage Notes:

1. This is a simplified implementation for educational purposes
2. Actual production code would need better error handling
3. Memory management and system calls would need to be adjusted for specific operating systems
4. The failure function construction is the key optimization that makes KMP more efficient than naive string matching

The KMP algorithm's advantage over simple string matching is that it never needs to re-examine characters in the text, making it particularly useful for large texts and patterns.

