# Naïve String Matching Algorithm in Assembly

Here's an example of a naïve string matching algorithm implemented in x86-64 Assembly language:

```assembly
.section .data
    text:       .ascii "ABABCABABCABCAB"
    text_len:   .long 15
    pattern:    .ascii "ABCAB"
    pattern_len: .long 5
    newline:    .ascii "\n"

.section .text
    .global _start

# Function: naive_string_match
# Parameters: RDI = text address, RSI = pattern address, RDX = text length, RCX = pattern length
# Returns: RAX = starting position of match, -1 if no match found
naive_string_match:
    push    rbp
    mov     rbp, rsp
    
    # Initialize variables
    mov     r8, rdi         # r8 = text address
    mov     r9, rsi         # r9 = pattern address
    mov     r10, rdx        # r10 = text length
    mov     r11, rcx        # r11 = pattern length
    
    # If pattern is longer than text, no match possible
    cmp     r11, r10
    jg      no_match
    
    # If pattern length is 0, return 0
    cmp     r11, 0
    je      found_match
    
    # Outer loop: try each position in text
    xor     r12, r12        # r12 = text index (i)
outer_loop:
    # Check if we have enough characters left in text
    cmp     r12, r10
    jge     no_match
    
    # Inner loop: compare pattern with text at current position
    xor     r13, r13        # r13 = pattern index (j)
inner_loop:
    # Check if we've matched the entire pattern
    cmp     r13, r11
    jge     found_match
    
    # Check if we're still within text bounds
    mov     rax, r12
    add     rax, r13
    cmp     rax, r10
    jge     no_match
    
    # Compare characters
    mov     al, [r8 + r12 + r13]    # text[i + j]
    mov     cl, [r9 + r13]          # pattern[j]
    cmp     al, cl
    jne     next_position           # mismatch, try next position
    
    # Characters match, continue to next character
    inc     r13
    jmp     inner_loop

next_position:
    # Increment text index and restart inner loop
    inc     r12
    jmp     outer_loop

found_match:
    # Return the starting position
    mov     rax, r12
    jmp     cleanup

no_match:
    # Return -1 for no match
    mov     rax, -1

cleanup:
    pop     rbp
    ret

# Main program
_start:
    # Call naive_string_match function
    mov     rdi, text         # text address
    mov     rsi, pattern      # pattern address
    mov     rdx, text_len     # text length
    mov     rcx, pattern_len  # pattern length
    call    naive_string_match
    
    # Print result
    mov     rax, 1            # sys_write
    mov     rdi, 1            # stdout
    mov     rsi, text         # result message
    mov     rdx, 15           # message length
    syscall
    
    # Exit program
    mov     rax, 60           # sys_exit
    mov     rdi, 0            # exit status
    syscall
```

## Algorithm Explanation

This assembly implementation follows the naïve string matching algorithm with the following key steps:

1. **Input validation**: Check if pattern is longer than text
2. **Outer loop**: Try each position in the text as a potential match start
3. **Inner loop**: Compare pattern characters with text characters at current position
4. **Character comparison**: Use byte-by-byte comparison with `cmp` instruction
5. **Match handling**: Return position if complete match found, continue if mismatch

## Key Assembly Concepts Used

- **Register usage**: RDI, RSI, RDX, RCX for parameters, R8-R13 for local variables
- **Memory addressing**: Direct addressing with base + offset calculations
- **Control flow**: Conditional jumps (`jne`, `jge`, `jmp`) for loop and condition logic
- **Function calling**: Proper stack management with `push`/`pop` and `call`/`ret`

## Time Complexity
- **Best case**: O(n) where n is text length (pattern not found)
- **Worst case**: O(n×m) where n is text length and m is pattern length
- **Space complexity**: O(1) - only using registers and stack space

The algorithm is called "naïve" because it doesn't use any advanced optimization techniques and simply checks every possible position in the text.