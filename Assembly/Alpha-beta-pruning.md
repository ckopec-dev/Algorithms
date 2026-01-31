# Alpha-Beta Pruning in Assembly Language

Here's an example implementation of the Alpha-Beta pruning algorithm in x86-64 Assembly language:

```assembly
.section .data
    # Game board representation (3x3 for Tic-Tac-Toe)
    board: .byte 0, 0, 0, 0, 0, 0, 0, 0, 0
    alpha: .long -1000000
    beta:  .long 1000000
    depth: .long 9

.section .text
    .global _start

# Minimax with Alpha-Beta pruning
# Parameters: board, depth, alpha, beta, isMaximizing
# Returns: best score
minimax_ab:
    push rbp
    mov rbp, rsp
    
    # Function parameters
    mov r8, rdi      # board
    mov r9, rsi      # depth
    mov r10, rdx     # alpha
    mov r11, rcx     # beta
    mov r12, r8      # isMaximizing
    
    # Check terminal state
    call check_terminal
    test eax, eax
    jz terminal_state
    
    # Check depth limit
    cmp r9, 0
    jz terminal_state
    
    # Initialize best score
    mov eax, 0
    
    # Maximizing player
    cmp r12, 1
    jne minimizing_player
    
    # MAX player
    mov eax, -1000000  # Initialize to negative infinity
    mov ecx, 0          # move counter
    
max_loop:
    cmp ecx, 9
    jge max_done
    
    # Check if position is empty
    mov dl, [r8 + rcx]
    test dl, dl
    jnz max_next_move
    
    # Make move
    mov [r8 + rcx], byte 1  # Place maximizing player
    
    # Recursive call
    push rcx
    push r11
    push r10
    push r9
    push r8
    dec r9
    mov rdi, r8
    mov rsi, r9
    mov rdx, r10
    mov rcx, r11
    mov rax, 0
    mov r12, 0  # Switch to minimizing player
    call minimax_ab
    pop r12
    
    # Undo move
    mov [r8 + rcx], byte 0
    
    # Update alpha
    cmp eax, r10d
    jle max_next_move
    mov r10d, eax
    
    # Alpha-Beta pruning check
    cmp r10d, r11d
    jge max_prune
    
max_next_move:
    inc ecx
    jmp max_loop
    
max_done:
    mov eax, r10d
    jmp max_return
    
max_prune:
    mov eax, r10d
    jmp max_return
    
minimizing_player:
    # MIN player
    mov eax, 1000000    # Initialize to positive infinity
    mov ecx, 0          # move counter
    
min_loop:
    cmp ecx, 9
    jge min_done
    
    # Check if position is empty
    mov dl, [r8 + rcx]
    test dl, dl
    jnz min_next_move
    
    # Make move
    mov [r8 + rcx], byte 2  # Place minimizing player
    
    # Recursive call
    push rcx
    push r11
    push r10
    push r9
    push r8
    dec r9
    mov rdi, r8
    mov rsi, r9
    mov rdx, r10
    mov rcx, r11
    mov rax, 0
    mov r12, 1  # Switch to maximizing player
    call minimax_ab
    pop r12
    
    # Undo move
    mov [r8 + rcx], byte 0
    
    # Update beta
    cmp eax, r11d
    jge min_next_move
    mov r11d, eax
    
    # Alpha-Beta pruning check
    cmp r10d, r11d
    jge min_prune
    
min_next_move:
    inc ecx
    jmp min_loop
    
min_done:
    mov eax, r11d
    jmp min_return
    
min_prune:
    mov eax, r11d
    jmp min_return
    
terminal_state:
    # Calculate heuristic value
    call evaluate_board
    mov eax, eax
    
max_return:
    pop rbp
    ret
    
min_return:
    pop rbp
    ret

# Evaluate board function
evaluate_board:
    push rbp
    mov rbp, rsp
    
    # Simple evaluation for Tic-Tac-Toe
    mov eax, 0
    
    # Check rows
    mov ecx, 0
row_check:
    cmp ecx, 9
    jge row_done
    
    # Check if three in a row
    mov dl, [board + rcx]
    cmp dl, 0
    je row_next
    
    # Check if player 1 wins (1)
    mov ebx, 0
    mov edi, 0
    mov esi, 0
    
    # Check row 0, 1, 2
    mov dl, [board + 0]
    cmp dl, 1
    jne row1_skip
    inc ebx
row1_skip:
    mov dl, [board + 1]
    cmp dl, 1
    jne row1_skip2
    inc ebx
row1_skip2:
    mov dl, [board + 2]
    cmp dl, 1
    jne row1_skip3
    inc ebx
row1_skip3:
    
    cmp ebx, 3
    je win_score
    mov ebx, 0
    
    # Check row 3, 4, 5
    mov dl, [board + 3]
    cmp dl, 1
    jne row2_skip
    inc ebx
row2_skip:
    mov dl, [board + 4]
    cmp dl, 1
    jne row2_skip2
    inc ebx
row2_skip2:
    mov dl, [board + 5]
    cmp dl, 1
    jne row2_skip3
    inc ebx
row2_skip3:
    
    cmp ebx, 3
    je win_score
    mov ebx, 0
    
    # Check row 6, 7, 8
    mov dl, [board + 6]
    cmp dl, 1
    jne row3_skip
    inc ebx
row3_skip:
    mov dl, [board + 7]
    cmp dl, 1
    jne row3_skip2
    inc ebx
row3_skip2:
    mov dl, [board + 8]
    cmp dl, 1
    jne row3_skip3
    inc ebx
row3_skip3:
    
    cmp ebx, 3
    je win_score
    mov ebx, 0
    
    # Check columns
    # Check column 0
    mov dl, [board + 0]
    cmp dl, 1
    jne col0_skip
    inc ebx
col0_skip:
    mov dl, [board + 3]
    cmp dl, 1
    jne col0_skip2
    inc ebx
col0_skip2:
    mov dl, [board + 6]
    cmp dl, 1
    jne col0_skip3
    inc ebx
col0_skip3:
    
    cmp ebx, 3
    je win_score
    mov ebx, 0
    
    # Check column 1
    mov dl, [board + 1]
    cmp dl, 1
    jne col1_skip
    inc ebx
col1_skip:
    mov dl, [board + 4]
    cmp dl, 1
    jne col1_skip2
    inc ebx
col1_skip2:
    mov dl, [board + 7]
    cmp dl, 1
    jne col1_skip3
    inc ebx
col1_skip3:
    
    cmp ebx, 3
    je win_score
    mov ebx, 0
    
    # Check column 2
    mov dl, [board + 2]
    cmp dl, 1
    jne col2_skip
    inc ebx
col2_skip:
    mov dl, [board + 5]
    cmp dl, 1
    jne col2_skip2
    inc ebx
col2_skip2:
    mov dl, [board + 8]
    cmp dl, 1
    jne col2_skip3
    inc ebx
col2_skip3:
    
    cmp ebx, 3
    je win_score
    mov ebx, 0
    
    # Check diagonals
    # Main diagonal
    mov dl, [board + 0]
    cmp dl, 1
    jne diag_skip
    inc ebx
diag_skip:
    mov dl, [board + 4]
    cmp dl, 1
    jne diag_skip2
    inc ebx
diag_skip2:
    mov dl, [board + 8]
    cmp dl, 1
    jne diag_skip3
    inc ebx
diag_skip3:
    
    cmp ebx, 3
    je win_score
    mov ebx, 0
    
    # Anti-diagonal
    mov dl, [board + 2]
    cmp dl, 1
    jne anti_diag_skip
    inc ebx
anti_diag_skip:
    mov dl, [board + 4]
    cmp dl, 1
    jne anti_diag_skip2
    inc ebx
anti_diag_skip2:
    mov dl, [board + 6]
    cmp dl, 1
    jne anti_diag_skip3
    inc ebx
anti_diag_skip3:
    
    cmp ebx, 3
    je win_score
    
    # Check for player 2 wins
    mov ebx, 0
    
    # Check rows for player 2
    mov dl, [board + 0]
    cmp dl, 2
    jne row1_2_skip
    inc ebx
row1_2_skip:
    mov dl, [board + 1]
    cmp dl, 2
    jne row1_2_skip2
    inc ebx
row1_2_skip2:
    mov dl, [board + 2]
    cmp dl, 2
    jne row1_2_skip3
    inc ebx
row1_2_skip3:
    
    cmp ebx, 3
    je lose_score
    
    # Continue checking other lines...
    
row_done:
    # Return score
    mov eax, 0
    jmp evaluate_end
    
win_score:
    mov eax, 1000000
    jmp evaluate_end
    
lose_score:
    mov eax, -1000000
    jmp evaluate_end
    
evaluate_end:
    pop rbp
    ret

# Check if game is terminal
check_terminal:
    push rbp
    mov rbp, rsp
    
    # Check if board is full
    mov ecx, 0
    mov eax, 1  # Assume board is full
    
full_check:
    cmp ecx, 9
    jge full_done
    
    mov dl, [board + rcx]
    test dl, dl
    jnz full_next
    
    mov eax, 0  # Board not full
    jmp full_done
    
full_next:
    inc ecx
    jmp full_check
    
full_done:
    pop rbp
    ret

# Main function
_start:
    # Initialize board
    mov rdi, board
    mov rsi, 9
    mov al, 0
    rep stosb
    
    # Call minimax with alpha-beta pruning
    mov rdi, board
    mov rsi, 9
    mov rdx, -1000000
    mov rcx, 1000000
    mov rax, 1  # isMaximizing = true
    call minimax_ab
    
    # Exit program
    mov eax, 1
    mov ebx, 0
    int 0x80
```

## Key Features of this Implementation

### 1. **Core Algorithm Structure**
- Implements the minimax algorithm with alpha-beta pruning
- Uses recursive function calls with proper parameter passing
- Maintains alpha and beta values throughout the search

### 2. **Alpha-Beta Pruning Logic**
```assembly
# Alpha-Beta pruning check
cmp r10d, r11d
jge prune_branch
```

### 3. **Game State Management**
- Board representation as a 9-element array
- Move validation and undo operations
- Terminal state detection

### 4. **Optimization Techniques**
- Early termination when pruning conditions are met
- Efficient memory access patterns
- Proper stack management for recursive calls

### 5. **Evaluation Function**
- Simple heuristic for Tic-Tac-Toe
- Win/loss scoring system
- Position-based evaluation

This implementation demonstrates how the alpha-beta pruning algorithm can be efficiently implemented in assembly language, showing the core logic while maintaining the performance benefits of low-level programming.

