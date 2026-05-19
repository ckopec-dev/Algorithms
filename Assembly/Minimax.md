# Minimax Algorithm in Assembly (x86-64 NASM)

Here's a complete example of a Minimax algorithm implementation in assembly language for a simple game tree:

```assembly
section .data
    ; Game board representation (3x3 Tic-Tac-Toe)
    board db 0, 0, 0, 0, 0, 0, 0, 0, 0  ; 9 positions, 0 = empty, 1 = X, 2 = O
    max_depth equ 9
    
    ; Evaluation constants
    win_score   equ 10
    lose_score  equ -10
    draw_score  equ 0

section .text
    global _start

; Minimax function
; Parameters: 
;   rdi = board pointer
;   rsi = depth
;   rdx = is_maximizing (1 for maximizing, 0 for minimizing)
; Returns: best score in rax
minimax:
    ; Check if game is over (terminal state)
    push rbp
    mov rbp, rsp
    push rdi
    push rsi
    push rdx
    
    ; Check for win/loss/draw
    call check_game_over
    cmp rax, 1000  ; If game over
    je game_over
    
    ; Check depth limit
    cmp rsi, 0
    je evaluate_position
    
    ; Continue recursion
    mov r12, rsi        ; Save depth
    mov r13, rdx        ; Save player type
    
    ; Initialize best score
    cmp rdx, 1
    je maximize
    mov rax, 1000       ; For minimizing player, start with high score
    jmp continue_minimax
    
maximize:
    mov rax, -1000      ; For maximizing player, start with low score
    
continue_minimax:
    mov r14, 0          ; Move counter (0-8)
    
next_move_loop:
    ; Check if position is empty
    mov r8, [rdi + r14] ; Load board position
    cmp r8, 0
    je valid_move
    
    ; Skip this position
    inc r14
    cmp r14, 9
    jl next_move_loop
    jmp no_moves_left
    
valid_move:
    ; Make move
    mov r8, r13         ; Player type (1 or 2)
    mov [rdi + r14], r8 ; Place player on board
    
    ; Recursively call minimax
    dec r12             ; Decrease depth
    mov rsi, r12        ; New depth
    xor rdx, rdx        ; Toggle player (0 for minimizing)
    cmp r13, 1
    jne is_max_player
    mov rdx, 1          ; Set to maximizing player
    
is_max_player:
    call minimax        ; Recursive call
    
    ; Undo move
    mov byte [rdi + r14], 0
    
    ; Update best score
    cmp r13, 1
    je update_max
    ; Minimizing player
    cmp rax, r15
    jg update_best
    mov r15, rax
    jmp next_move_loop
    
update_max:
    ; Maximizing player
    cmp rax, r15
    jl update_best
    mov r15, rax
    jmp next_move_loop
    
update_best:
    mov r15, rax
    jmp next_move_loop
    
no_moves_left:
    ; No moves left, return draw score
    mov rax, draw_score
    jmp cleanup
    
game_over:
    ; Return evaluation score
    mov rax, rax
    jmp cleanup
    
evaluate_position:
    ; Simple evaluation function
    call evaluate_board
    jmp cleanup
    
cleanup:
    pop rdx
    pop rsi
    pop rdi
    pop rbp
    ret

; Check if game is over
check_game_over:
    ; Check rows
    mov r10, 0
    mov r11, 3
    
check_rows:
    mov r8, [rdi + r10]
    cmp r8, 0
    je check_next_row
    
    mov r9, [rdi + r10 + 1]
    cmp r9, r8
    jne check_next_row
    
    mov r12, [rdi + r10 + 2]
    cmp r12, r8
    jne check_next_row
    
    ; Win found
    cmp r8, 1
    je player_x_win
    mov rax, lose_score
    jmp return_score
    
player_x_win:
    mov rax, win_score
    jmp return_score
    
check_next_row:
    add r10, 3
    dec r11
    jg check_rows
    
    ; Check columns
    mov r10, 0
    mov r11, 3
    
check_cols:
    mov r8, [rdi + r10]
    cmp r8, 0
    je check_next_col
    
    mov r9, [rdi + r10 + 3]
    cmp r9, r8
    jne check_next_col
    
    mov r12, [rdi + r10 + 6]
    cmp r12, r8
    jne check_next_col
    
    ; Win found
    cmp r8, 1
    je player_x_win_col
    mov rax, lose_score
    jmp return_score
    
player_x_win_col:
    mov rax, win_score
    jmp return_score
    
check_next_col:
    inc r10
    dec r11
    jg check_cols
    
    ; Check diagonals
    ; Main diagonal
    mov r8, [rdi + 0]
    cmp r8, 0
    je check_anti_diag
    
    mov r9, [rdi + 4]
    cmp r9, r8
    jne check_anti_diag
    
    mov r12, [rdi + 8]
    cmp r12, r8
    jne check_anti_diag
    
    cmp r8, 1
    je player_x_win_diag
    mov rax, lose_score
    jmp return_score
    
player_x_win_diag:
    mov rax, win_score
    jmp return_score
    
check_anti_diag:
    ; Anti-diagonal
    mov r8, [rdi + 2]
    cmp r8, 0
    je check_draw
    
    mov r9, [rdi + 4]
    cmp r9, r8
    jne check_draw
    
    mov r12, [rdi + 6]
    cmp r12, r8
    jne check_draw
    
    cmp r8, 1
    je player_x_win_anti
    mov rax, lose_score
    jmp return_score
    
player_x_win_anti:
    mov rax, win_score
    jmp return_score
    
check_draw:
    ; Check if board is full (draw)
    mov r10, 0
    mov r11, 0
    
check_full_board:
    mov r8, [rdi + r10]
    cmp r8, 0
    jne is_not_full
    
    inc r11
    cmp r11, 9
    jl check_full_board
    
    ; Board full, it's a draw
    mov rax, draw_score
    jmp return_score
    
is_not_full:
    inc r10
    cmp r10, 9
    jl check_full_board
    
    ; Game not over yet
    mov rax, 1000       ; Not terminal state
    jmp return_score
    
return_score:
    ret

; Simple evaluation function for non-terminal states
evaluate_board:
    ; This is a simplified evaluation
    ; In practice, this would be more complex
    mov rax, 0
    
    ; Count possible winning lines for each player
    ; (simplified implementation)
    ret

; Main function to demonstrate minimax
main:
    ; Initialize board
    mov rdi, board
    mov rsi, 0
    mov rdx, 1          ; Start with maximizing player (X)
    
    ; Call minimax
    call minimax
    
    ; Result is in rax
    ; (In a real program, you'd output this result)
    
    ; Exit
    mov rax, 60         ; sys_exit
    mov rdi, 0          ; exit status
    syscall
```

## Key Features of This Implementation:

1. **Recursive Structure**: Implements the core minimax algorithm with recursive calls
2. **Game State Management**: Handles board representation and move validation
3. **Player Alternation**: Properly alternates between maximizing (X) and minimizing (O) players
4. **Terminal State Detection**: Identifies wins, losses, and draws
5. **Depth Limiting**: Includes depth parameter to prevent infinite recursion
6. **Score Evaluation**: Returns appropriate scores for different game states

## Algorithm Logic:

- **Maximizing Player**: Tries to maximize the score (chooses best move)
- **Minimizing Player**: Tries to minimize the score (chooses worst move for opponent)
- **Terminal States**: Win = +10, Loss = -10, Draw = 0
- **Recursion**: Explores all possible game trees to depth limit

This assembly implementation demonstrates the fundamental concepts of minimax in a low-level programming context, showing how the algorithm would be structured at the machine level.

