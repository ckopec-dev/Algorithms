# Levenshtein Distance Algorithm in Assembly

Here's an implementation of the Levenshtein distance algorithm in x86-64 Assembly using NASM syntax:

```assembly
section .text
global levenshtein_distance

; int levenshtein_distance(char* str1, char* str2)
; Returns the Levenshtein distance between two strings
levenshtein_distance:
    push rbp
    mov rbp, rsp
    
    ; Parameters:
    ; rdi = str1 (pointer to first string)
    ; rsi = str2 (pointer to second string)
    
    ; Get string lengths
    mov rdx, rdi        ; rdx = str1
    xor rcx, rcx        ; rcx = length1 = 0
    
count_str1:
    mov al, [rdx]       ; Load character from str1
    test al, al         ; Check if null terminator
    jz count_done1
    inc rcx             ; Increment length counter
    inc rdx             ; Move to next character
    jmp count_str1
    
count_done1:
    mov r8, rcx         ; r8 = length1
    
    mov rdx, rsi        ; rdx = str2
    xor rcx, rcx        ; rcx = length2 = 0
    
count_str2:
    mov al, [rdx]       ; Load character from str2
    test al, al         ; Check if null terminator
    jz count_done2
    inc rcx             ; Increment length counter
    inc rdx             ; Move to next character
    jmp count_str2
    
count_done2:
    mov r9, rcx         ; r9 = length2
    
    ; Handle edge cases
    test r8, r8         ; If length1 = 0
    jz return_length2
    test r9, r9         ; If length2 = 0
    jz return_length1
    
    ; Allocate memory for DP table: (length1 + 1) * (length2 + 1) * sizeof(int)
    mov rax, r8         ; rax = length1
    inc rax             ; +1 for base case
    mov rbx, r9         ; rbx = length2
    inc rbx             ; +1 for base case
    imul rbx, rax       ; rbx = (length1 + 1) * (length2 + 1)
    mov rax, rbx        ; rax = size in int units
    shl rax, 2          ; rax = size in bytes (multiply by 4)
    
    ; Allocate stack space for DP table
    sub rsp, rax        ; Allocate space on stack
    
    ; Initialize DP table
    ; First row: 0, 1, 2, 3, ...
    mov r10, rsp        ; r10 = pointer to DP table
    xor rcx, rcx        ; rcx = column index
    
init_row:
    cmp rcx, r9         ; Compare with length2
    jg init_cols_done
    mov [r10 + rcx * 4], ecx    ; DP[0][j] = j
    inc rcx
    jmp init_row
    
init_cols_done:
    ; First column: 0, 1, 2, 3, ...
    xor rcx, rcx        ; rcx = row index
    mov r11, r8         ; r11 = length1
    
init_col:
    cmp rcx, r11        ; Compare with length1
    jg init_done
    mov [r10 + rcx * 4 * (r9 + 1)], rcx    ; DP[i][0] = i
    inc rcx
    jmp init_col
    
init_done:
    ; Fill the DP table
    xor rcx, rcx        ; rcx = i (row index)
    
outer_loop:
    cmp rcx, r11        ; Compare with length1
    jg end_algorithm
    
    xor rdx, rdx        ; rdx = j (column index)
    
inner_loop:
    cmp rdx, r9         ; Compare with length2
    jg next_row
    
    ; Calculate positions in DP table
    mov r8, rcx         ; r8 = i
    mov r9, rdx         ; r9 = j
    
    ; DP[i][j] = min(DP[i-1][j] + 1, DP[i][j-1] + 1, DP[i-1][j-1] + cost)
    
    ; Get DP[i-1][j] + 1
    mov eax, [r10 + (rcx - 1) * 4 * (r9 + 1) + rdx * 4]    ; DP[i-1][j]
    inc eax             ; + 1
    
    mov r10, rax        ; Save minimum so far
    
    ; Get DP[i][j-1] + 1
    mov eax, [r10 + rcx * 4 * (r9 + 1) + (rdx - 1) * 4]    ; DP[i][j-1]
    inc eax             ; + 1
    
    ; Compare with current minimum
    cmp eax, r10
    jge skip_first_min
    mov r10, rax        ; Update minimum
    
skip_first_min:
    ; Get DP[i-1][j-1] + cost
    mov eax, [r10 + (rcx - 1) * 4 * (r9 + 1) + (rdx - 1) * 4]    ; DP[i-1][j-1]
    
    ; Calculate cost (0 if same character, 1 if different)
    mov al, [rdi + rcx] ; Load str1[i]
    mov bl, [rsi + rdx] ; Load str2[j]
    cmp al, bl          ; Compare characters
    je same_char
    inc eax             ; cost = 1
    
same_char:
    add eax, [r10 + (rcx - 1) * 4 * (r9 + 1) + (rdx - 1) * 4]    ; Add cost
    
    ; Compare with current minimum
    cmp eax, r10
    jge skip_second_min
    mov r10, rax        ; Update minimum
    
skip_second_min:
    ; Store result in DP[i][j]
    mov [r10 + rcx * 4 * (r9 + 1) + rdx * 4], r10
    
    inc rdx             ; Next column
    jmp inner_loop
    
next_row:
    inc rcx             ; Next row
    jmp outer_loop
    
end_algorithm:
    ; Return DP[length1][length2]
    mov eax, [r10 + r8 * 4 * (r9 + 1) + r9 * 4]
    
    ; Clean up stack
    add rsp, rax        ; Deallocate DP table
    
    pop rbp
    ret

return_length1:
    mov eax, r8
    pop rbp
    ret

return_length2:
    mov eax, r9
    pop rbp
    ret
```

## Example Usage

```assembly
section .data
    str1 db "kitten", 0
    str2 db "sitting", 0

section .text
    global _start
    
_start:
    mov rdi, str1       ; First string
    mov rsi, str2       ; Second string
    call levenshtein_distance
    
    ; Result is in eax
    ; For "kitten" and "sitting", result should be 3
    
    ; Exit program
    mov eax, 1          ; sys_exit
    mov ebx, 0          ; exit status
    int 0x80
```

## Key Features

1. **Dynamic Programming Approach**: Uses a 2D matrix to store intermediate results
2. **Memory Management**: Allocates stack space for the DP table
3. **Edge Case Handling**: Properly handles empty strings
4. **Three Operations**: Insertion, deletion, and substitution
5. **Optimized Loop Structure**: Efficient iteration through the strings

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: O(m × n) for the DP table

This implementation demonstrates the core principles of the Levenshtein distance algorithm at the assembly level, showing how the dynamic programming approach is implemented through memory operations and conditional logic.