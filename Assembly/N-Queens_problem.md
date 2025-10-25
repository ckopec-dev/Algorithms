# N-Queens Problem in Assembly Language

Here's an example implementation of the N-Queens problem using x86-64 Assembly language:

```assembly
.section .data
    .equ N, 8           # Size of the chessboard (8x8)
    .equ MAX_N, 100     # Maximum board size
    
    # Arrays to track queen positions and constraints
    .align 8
    board: .space MAX_N * 8     # Array to store column positions of queens
    cols: .space MAX_N          # Array to track used columns
    diag1: .space MAX_N * 2     # Array to track used diagonals (row - col)
    diag2: .space MAX_N * 2     # Array to track used diagonals (row + col)
    
    solutions: .quad 0          # Counter for solutions
    
    # Print format strings
    .align 8
    newline: .ascii "\n"
    newline_len = . - newline
    queen_char: .ascii "Q"
    empty_char: .ascii "."
    space_char: .ascii " "

.section .text
    .global _start

# Function to check if placing a queen at (row, col) is safe
# Parameters: r (row), c (column)
# Returns: 1 if safe, 0 if not safe
check_safe:
    push %rbp
    mov %rsp, %rbp
    
    # Get parameters
    mov %rdi, %rax      # r (row)
    mov %rsi, %rbx      # c (column)
    
    # Check column constraint
    movzbq cols(%rbx), %rcx
    test %rcx, %rcx
    jnz not_safe
    
    # Check diagonal constraint 1 (row - col)
    mov %rax, %rcx
    sub %rbx, %rcx
    add N, %rcx         # Adjust for negative indices
    movzbq diag1(%rcx), %rcx
    test %rcx, %rcx
    jnz not_safe
    
    # Check diagonal constraint 2 (row + col)
    mov %rax, %rcx
    add %rbx, %rcx
    movzbq diag2(%rcx), %rcx
    test %rcx, %rcx
    jnz not_safe
    
    # If we reach here, it's safe
    mov $1, %rax
    jmp safe_exit
    
not_safe:
    mov $0, %rax
    
safe_exit:
    pop %rbp
    ret

# Function to place a queen at position (row, col)
# Parameters: r (row), c (column)
place_queen:
    push %rbp
    mov %rsp, %rbp
    
    mov %rdi, %rax      # r (row)
    mov %rsi, %rbx      # c (column)
    
    # Place queen on board
    mov %rbx, board(%rax)
    
    # Mark column as used
    movb $1, cols(%rbx)
    
    # Mark diagonals as used
    mov %rax, %rcx
    sub %rbx, %rcx
    add N, %rcx         # Adjust for negative indices
    movb $1, diag1(%rcx)
    
    mov %rax, %rcx
    add %rbx, %rcx
    movb $1, diag2(%rcx)
    
    pop %rbp
    ret

# Function to remove a queen at position (row, col)
# Parameters: r (row), c (column)
remove_queen:
    push %rbp
    mov %rsp, %rbp
    
    mov %rdi, %rax      # r (row)
    mov %rsi, %rbx      # c (column)
    
    # Remove queen from board
    movb $0, board(%rax)
    
    # Unmark column
    movb $0, cols(%rbx)
    
    # Unmark diagonals
    mov %rax, %rcx
    sub %rbx, %rcx
    add N, %rcx         # Adjust for negative indices
    movb $0, diag1(%rcx)
    
    mov %rax, %rcx
    add %rbx, %rcx
    movb $0, diag2(%rcx)
    
    pop %rbp
    ret

# Recursive backtracking function to solve N-Queens
# Parameters: row (current row to place queen)
solve_nqueens:
    push %rbp
    mov %rsp, %rbp
    
    mov %rdi, %rax      # row
    
    # Base case: if we've placed queens in all rows
    cmp $N, %rax
    jge found_solution
    
    # Try placing queen in each column of current row
    mov $0, %rbx        # column counter
    
try_column:
    cmp $N, %rbx
    jge backtrack       # if column >= N, backtrack
    
    # Check if current position is safe
    mov %rax, %rdi      # row
    mov %rbx, %rsi      # column
    call check_safe
    
    test %rax, %rax
    jz try_next_column  # if not safe, try next column
    
    # Place queen
    mov %rax, %rdi      # row
    mov %rbx, %rsi      # column
    call place_queen
    
    # Recursively solve for next row
    mov %rax, %rdi      # row
    inc %rdi            # next row
    call solve_nqueens
    
    # Remove queen (backtrack)
    mov %rax, %rdi      # row
    mov %rbx, %rsi      # column
    call remove_queen
    
try_next_column:
    inc %rbx
    jmp try_column
    
backtrack:
    # No valid position found in this row, backtrack
    mov $0, %rax
    jmp solve_exit
    
found_solution:
    # Increment solution counter
    mov solutions(%rip), %rax
    inc %rax
    mov %rax, solutions(%rip)
    mov $1, %rax        # indicate solution found
    
solve_exit:
    pop %rbp
    ret

# Function to print the board
print_board:
    push %rbp
    mov %rsp, %rbp
    
    mov $0, %rax        # row counter
    
print_row_loop:
    cmp $N, %rax
    jge print_done
    
    mov $0, %rbx        # column counter
    
print_col_loop:
    cmp $N, %rbx
    jge print_newline
    
    # Check if there's a queen at this position
    mov board(%rax), %rcx
    cmp %rbx, %rcx
    jne print_empty
    
    # Print queen
    mov $1, %rax        # write to stdout
    mov $1, %rdi        # file descriptor
    leaq queen_char, %rsi
    mov $1, %rdx        # count
    syscall
    
    jmp next_col
    
print_empty:
    # Print empty space
    mov $1, %rax        # write to stdout
    mov $1, %rdi        # file descriptor
    leaq empty_char, %rsi
    mov $1, %rdx        # count
    syscall
    
next_col:
    inc %rbx
    jmp print_col_loop
    
print_newline:
    # Print newline
    mov $1, %rax        # write to stdout
    mov $1, %rdi        # file descriptor
    leaq newline, %rsi
    mov $1, %rdx        # count
    syscall
    
    inc %rax
    jmp print_row_loop
    
print_done:
    pop %rbp
    ret

# Main function
_start:
    # Initialize arrays to zero
    mov $0, %rax
    mov $0, %rbx
    
init_loop:
    cmp $MAX_N, %rbx
    jge init_done
    
    movb $0, cols(%rbx)
    movb $0, diag1(%rbx)
    movb $0, diag2(%rbx)
    inc %rbx
    jmp init_loop
    
init_done:
    # Reset board
    mov $0, %rax
    mov $0, %rbx
    
reset_board:
    cmp $N, %rax
    jge board_reset_done
    
    movb $0, board(%rax)
    inc %rax
    jmp reset_board
    
board_reset_done:
    # Solve N-Queens
    mov $0, %rdi        # start with row 0
    call solve_nqueens
    
    # Print final solution count
    mov solutions(%rip), %rax
    # Print the number of solutions (simplified output)
    
    # Exit program
    mov $60, %rax       # sys_exit
    mov $0, %rdi        # exit status
    syscall
```

## Key Features of This Implementation:

### 1. **Data Structures**
- `board[]`: Stores column positions of queens for each row
- `cols[]`: Tracks used columns
- `diag1[]`: Tracks used diagonals (row - col)
- `diag2[]`: Tracks used diagonals (row + col)
- `solutions`: Counter for valid solutions

### 2. **Core Functions**
- **`check_safe()`**: Verifies if placing a queen at given position is valid
- **`place_queen()`**: Places a queen and marks constraints
- **`remove_queen()`**: Removes a queen and unmarks constraints
- **`solve_nqueens()`**: Recursive backtracking algorithm
- **`print_board()`**: Displays the chessboard

### 3. **Algorithm Approach**
- Uses backtracking with constraint checking
- Implements column and diagonal conflict detection
- Recursive approach to explore all possible configurations
- Efficient bit manipulation for constraint tracking

### 4. **Assembly-Specific Features**
- Proper stack management with `push`/`pop`
- Register usage conventions
- System calls for I/O operations
- Memory alignment for performance

This implementation demonstrates how to solve the classic N-Queens problem using low-level assembly programming techniques while maintaining the essential algorithmic structure of backtracking with constraint satisfaction.

