# Rabin-Karp Algorithm in Assembly (x86-64)

Here's an implementation of the Rabin-Karp string matching algorithm in x86-64 Assembly:

```assembly
.section .data
    # Pattern to search for
    pattern:    .ascii "ABCD"
    pattern_len: .long 4
    
    # Text to search in
    text:       .ascii "ABCABCDABCDABCD"
    text_len:   .long 16
    
    # Hash parameters
    base:       .long 256
    prime:      .long 101

.section .text
    .global _start

# Rabin-Karp implementation
# Input: text, pattern, text_len, pattern_len
# Output: positions where pattern is found

rabin_karp:
    # Function prologue
    push    %rbp
    mov     %rsp, %rbp
    sub     $32, %rsp           # Allocate stack space
    
    # Parameters (passed on stack)
    # [rbp+16] text pointer
    # [rbp+24] pattern pointer
    # [rbp+32] text_len
    # [rbp+40] pattern_len
    
    # Local variables
    mov     %rdi, %r8           # text pointer
    mov     %rsi, %r9           # pattern pointer
    mov     %rdx, %r10          # text_len
    mov     %rcx, %r11          # pattern_len
    
    # Calculate hash of pattern
    mov     $0, %rax            # hash_pattern = 0
    mov     $0, %rcx            # i = 0
    
calculate_pattern_hash:
    cmp     %r11, %rcx
    jge     pattern_hash_done
    mov     (%r9,%rcx,1), %dl    # Load pattern[i]
    mov     %rax, %rbx
    imul    %rbx, %rax          # hash_pattern *= base
    add     %rdx, %rax          # hash_pattern += pattern[i]
    mov     %rax, %rbx
    mov     $101, %rax
    xor     %rax, %rax
    div     %rbx                 # hash_pattern %= prime
    mov     %rax, %rbx
    mov     %rbx, %rax
    inc     %rcx
    jmp     calculate_pattern_hash
    
pattern_hash_done:
    mov     %rax, %r12          # Save pattern hash
    
    # Calculate hash of first window of text
    mov     $0, %rax            # hash_text = 0
    mov     $0, %rcx            # i = 0
    
calculate_text_hash:
    cmp     %r11, %rcx
    jge     text_hash_done
    mov     (%r8,%rcx,1), %dl    # Load text[i]
    mov     %rax, %rbx
    imul    %rbx, %rax          # hash_text *= base
    add     %rdx, %rax          # hash_text += text[i]
    mov     %rax, %rbx
    mov     $101, %rax
    xor     %rax, %rax
    div     %rbx                 # hash_text %= prime
    mov     %rax, %rbx
    mov     %rbx, %rax
    inc     %rcx
    jmp     calculate_text_hash
    
text_hash_done:
    mov     %rax, %r13          # Save first window hash
    
    # Precompute base^(pattern_len-1) % prime
    mov     $1, %rax            # power = 1
    mov     %r11, %rcx
    dec     %rcx                # pattern_len - 1
    mov     $256, %rbx          # base
    
power_loop:
    cmp     $0, %rcx
    jle     power_done
    mov     %rax, %rbx
    imul    %rbx, %rax          # power *= base
    mov     %rax, %rbx
    mov     $101, %rax
    xor     %rax, %rax
    div     %rbx                 # power %= prime
    mov     %rax, %rbx
    mov     %rbx, %rax
    dec     %rcx
    jmp     power_loop
    
power_done:
    mov     %rax, %r14          # Save base^(pattern_len-1) % prime
    
    # Slide the pattern over text one by one
    mov     $0, %rcx            # i = 0
    
slide_loop:
    cmp     %r10, %rcx
    jge     slide_done
    
    # Check if hash values match
    cmp     %r12, %r13          # if (hash_pattern == hash_text)
    jne     skip_match
    
    # If hash matches, check character by character
    mov     %rcx, %r15          # i = current position
    mov     $0, %rdx            # j = 0
    
check_chars:
    cmp     %r11, %rdx
    jge     chars_match
    mov     (%r8,%r15,1), %dl    # text[i+j]
    mov     (%r9,%rdx,1), %dh    # pattern[j]
    cmp     %dl, %dh
    jne     chars_mismatch
    inc     %r15
    inc     %rdx
    jmp     check_chars
    
chars_match:
    # Pattern found at position i
    # Print position (simplified - would normally call printf)
    mov     %rcx, %rax
    # In a real implementation, we'd store this position
    jmp     next_iteration
    
chars_mismatch:
    # Continue searching
    jmp     next_iteration
    
skip_match:
    # Hash doesn't match, continue
    jmp     next_iteration
    
next_iteration:
    # Update hash for next window
    # Remove leftmost character and add rightmost character
    cmp     %r10, %rcx
    jge     slide_done
    
    # Remove leftmost character
    mov     (%r8,%rcx,1), %dl    # text[i]
    mov     %r13, %rax
    imul    %r14, %rax          # hash_text * base^(pattern_len-1)
    add     %rdx, %rax          # hash_text + text[i]
    mov     %rax, %rbx
    mov     $101, %rax
    xor     %rax, %rax
    div     %rbx                 # hash_text %= prime
    mov     %rax, %r13
    
    # Add new character
    mov     (%r8,%rcx,%rax), %dl # text[i+pattern_len]
    mov     %r13, %rax
    imul    %rax, %rax          # hash_text * base
    add     %rdx, %rax          # hash_text + new_char
    mov     %rax, %rbx
    mov     $101, %rax
    xor     %rax, %rax
    div     %rbx                 # hash_text %= prime
    mov     %rax, %r13
    
    inc     %rcx
    jmp     slide_loop
    
slide_done:
    # Function epilogue
    mov     %rbp, %rsp
    pop     %rbp
    ret

# Main program
_start:
    # Call Rabin-Karp function
    mov     $text, %rdi         # text pointer
    mov     $pattern, %rsi      # pattern pointer
    mov     $text_len, %rdx     # text_len
    mov     $pattern_len, %rcx  # pattern_len
    
    call    rabin_karp
    
    # Exit program
    mov     $60, %rax           # sys_exit
    mov     $0, %rdi            # exit status
    syscall
```

## Key Features of This Implementation:

### 1. **Algorithm Components:**
- **Hash calculation** for pattern and text windows
- **Sliding window technique** for efficient searching
- **Modular arithmetic** to prevent hash overflow
- **Character-by-character verification** when hashes match

### 2. **Assembly-Specific Details:**
- Uses x86-64 calling conventions
- Proper stack management with function prologue/epilogue
- Register usage optimization
- Division operations for modular arithmetic

### 3. **Key Operations:**
- **Hash computation**: `hash = (hash * base + char) % prime`
- **Window sliding**: Remove leftmost, add rightmost character
- **Collision handling**: Character verification when hashes match

### 4. **Parameters Used:**
- `base = 256` (ASCII character set)
- `prime = 101` (large prime number)
- Pattern length = 4 characters
- Text length = 16 characters

This implementation demonstrates the core principles of the Rabin-Karp algorithm while showing how it translates to low-level assembly code, including proper handling of modular arithmetic and efficient string matching techniques.