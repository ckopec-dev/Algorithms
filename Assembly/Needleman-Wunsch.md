# Needleman-Wunsch Algorithm in Assembly

Here's an implementation of the Needleman-Wunsch global sequence alignment algorithm in x86-64 assembly language:

```assembly
.section .data
    # Sequence strings
    seq1: .ascii "ACGTACGT"
    seq2: .ascii "ACGTACGT"
    seq1_len = 8
    seq2_len = 8
    
    # Scoring parameters
    match_score: .long 2
    mismatch_score: .long -1
    gap_penalty: .long -1
    
    # Matrix dimensions
    matrix_size = 9 * 9  # (len1+1) * (len2+1)

.section .bss
    # Dynamic programming matrix
    matrix: .space matrix_size * 4  # 4 bytes per integer

.section .text
    .global _start

# Function to compute Needleman-Wunsch alignment
needleman_wunsch:
    # Arguments: rdi = seq1, rsi = seq2, rdx = len1, rcx = len2
    # Returns: alignment score in rax
    
    push rbp
    mov rbp, rsp
    
    # Initialize matrix dimensions
    mov r8, rdx          # r8 = len1
    mov r9, rcx          # r9 = len2
    
    # Initialize first row and column
    mov r10, 0           # i = 0
init_loop:
    cmp r10, r8
    jg init_col
    
    # Initialize row 0
    mov eax, r10
    imul eax, r9         # eax = i * len2
    add eax, 0           # eax = i * len2 + 0
    imul eax, 4          # eax = index * 4 (bytes)
    mov dword ptr [matrix + rax], 0
    inc r10
    jmp init_loop

init_col:
    mov r10, 0
init_col_loop:
    cmp r10, r9
    jg init_done
    
    # Initialize column 0
    mov eax, r10
    imul eax, r8         # eax = j * len1
    add eax, 0           # eax = j * len1 + 0
    imul eax, 4          # eax = index * 4 (bytes)
    mov dword ptr [matrix + rax], 0
    inc r10
    jmp init_col_loop

init_done:
    # Fill the matrix using dynamic programming
    mov r10, 1           # i = 1
fill_loop_i:
    cmp r10, r8
    jg fill_done
    
    mov r11, 1           # j = 1
fill_loop_j:
    cmp r11, r9
    jg fill_next_i
    
    # Calculate matrix indices
    mov eax, r10         # eax = i
    imul eax, r9         # eax = i * len2
    add eax, r11         # eax = i * len2 + j
    imul eax, 4          # eax = index * 4
    
    # Get diagonal score
    mov ebx, r10
    dec ebx              # ebx = i - 1
    mov ecx, r11
    dec ecx              # ecx = j - 1
    mov edx, ebx
    imul edx, r9         # edx = (i-1) * len2
    add edx, ecx         # edx = (i-1) * len2 + (j-1)
    imul edx, 4          # edx = index * 4
    mov ebx, dword ptr [matrix + rdx]  # diagonal score
    
    # Check if characters match
    mov al, byte ptr [rdi + r10 - 1]  # seq1[i-1]
    mov cl, byte ptr [rsi + r11 - 1]  # seq2[j-1]
    cmp al, cl
    je match_case
    
    # Mismatch case
    mov edx, dword ptr [mismatch_score]
    add ebx, edx         # diagonal + mismatch_score
    jmp score_calc
    
match_case:
    # Match case
    mov edx, dword ptr [match_score]
    add ebx, edx         # diagonal + match_score
    
score_calc:
    # Get left score
    mov edx, r10
    dec edx              # edx = i - 1
    mov ecx, r11
    mov eax, edx
    imul eax, r9         # eax = (i-1) * len2
    add eax, ecx         # eax = (i-1) * len2 + j
    imul eax, 4          # eax = index * 4
    mov edx, dword ptr [matrix + rax]  # left score
    sub edx, dword ptr [gap_penalty]   # left + gap_penalty
    
    # Get up score
    mov eax, r10
    mov ecx, r11
    dec ecx              # ecx = j - 1
    mov edx, eax
    imul edx, r9         # edx = i * len2
    add edx, ecx         # edx = i * len2 + (j-1)
    imul edx, 4          # edx = index * 4
    mov ecx, dword ptr [matrix + rdx]  # up score
    sub ecx, dword ptr [gap_penalty]   # up + gap_penalty
    
    # Find maximum of three scores
    mov edi, ebx         # diagonal score
    cmp edi, edx
    jge skip_left
    mov edi, edx         # left score
skip_left:
    cmp edi, ecx
    jge skip_up
    mov edi, ecx         # up score
skip_up:
    
    # Store result in matrix
    mov dword ptr [matrix + rax], edi
    
    inc r11
    jmp fill_loop_j

fill_next_i:
    inc r10
    jmp fill_loop_i

fill_done:
    # Return the final score (bottom-right corner)
    mov eax, r8
    imul eax, r9
    add eax, r9          # eax = len1 * len2 + len2
    imul eax, 4          # eax = index * 4
    mov eax, dword ptr [matrix + rax]
    
    pop rbp
    ret

# Main program entry point
_start:
    # Initialize sequences
    mov rdi, seq1        # sequence 1
    mov rsi, seq2        # sequence 2
    mov rdx, seq1_len    # length of seq1
    mov rcx, seq2_len    # length of seq2
    
    # Call Needleman-Wunsch algorithm
    call needleman_wunsch
    
    # Exit program
    mov rax, 60          # sys_exit
    mov rdi, 0           # exit status
    syscall
```

## Key Features of this Implementation:

1. **Matrix Initialization**: Sets up the dynamic programming matrix with proper initialization
2. **Scoring System**: Implements match/mismatch scores and gap penalties
3. **Dynamic Programming**: Fills the matrix using the Needleman-Wunsch recurrence relation
4. **Memory Management**: Uses proper addressing for matrix elements
5. **Optimization**: Efficient use of registers and memory operations

## Algorithm Steps:
1. Initialize first row and column with gap penalties
2. Fill the matrix using the recurrence relation:
   ```
   M[i,j] = max(M[i-1,j-1] + score, M[i-1,j] + gap_penalty, M[i,j-1] + gap_penalty)
   ```
3. Return the value in the bottom-right cell as the alignment score

This assembly implementation demonstrates the core concepts of the Needleman-Wunsch algorithm while showing how to work with dynamic programming in low-level code.

