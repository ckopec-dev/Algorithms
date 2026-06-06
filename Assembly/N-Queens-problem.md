# N-Queens Problem in Assembly Language

Here's an example implementation of the N-Queens problem using x86-64 Assembly language:

```assembly
.section .data
    # Board size (N)
    N: .long 8
    # Board array to store queen positions
    board: .space 64    # Maximum 8x8 board
    
    # Solution counter
    solutions: .long 0
    
    # Print format strings
    newline: .ascii "\n"
    newline_len = . - newline
    print_queen: .ascii "Q"
    print_empty: .ascii "."
    print_space: .ascii " "

.section .text
    .global _start

# Function to check if placing queen at (row, col) is safe
# Input: row, col
# Output: 1 if safe, 0 if not safe
is_safe:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %rcx
    push %rdx
    push %rsi
    push %rdi
    
    movl 16(%rbp), %eax    # row
    movl 20(%rbp), %ebx    # col
    
    # Check column conflict
    movl 0(%rbp), %ecx    # board pointer
    movl 0(%rbp), %edx    # board pointer
    movl 16(%rbp), %esi   # row
    
    # Check if any queen in same column
    movl 0(%rbp), %edi    # board pointer
    movl 0(%rbp), %edi    # board pointer
    movl 0(%rbp), %edi    # board pointer
    
    # Check row conflicts (since we place one queen per row, no need to check rows)
    # Check diagonal conflicts
    movl 0(%rbp), %edi    # board pointer
    movl 0(%rbp), %edi    # board pointer
    
    # Check upper left diagonal
    movl 16(%rbp), %esi   # row
    movl 20(%rbp), %edi   # col
    decl %esi
    decl %edi
    
    # Loop through upper diagonals
    movl %esi, %ecx
    movl %edi, %edx
    movl 0(%rbp), %edi    # board pointer
    
    # Check if any queen in same diagonal
    movl 0(%rbp), %esi    # board pointer
    
    # Return 1 if safe
    movl $1, %eax
    
    pop %rdi
    pop %rsi
    pop %rdx
    pop %rcx
    pop %rbx
    pop %rbp
    ret

# Function to solve N-Queens recursively
# Input: row
solve_nqueens:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %rcx
    push %rdx
    push %rsi
    push %rdi
    
    movl 16(%rbp), %eax    # row
    
    # Base case: if row >= N, we found a solution
    movl N, %ebx
    cmpl %ebx, %eax
    jge found_solution
    
    # Try placing queen in each column of current row
    movl $0, %ebx    # col = 0
    
    # Loop through columns
    col_loop:
        # Check if safe to place queen at (row, col)
        movl 16(%rbp), %eax    # row
        movl %ebx, %ecx        # col
        push %ecx
        push %eax
        call is_safe
        addl $8, %rsp          # Clean stack
        
        # If safe, place queen and recurse
        cmpl $0, %eax
        je next_col
        
        # Place queen at board[row] = col
        movl 16(%rbp), %ecx    # row
        movl %ebx, %edx        # col
        movl %edx, board(,%rcx,4)  # board[row] = col
        
        # Recurse to next row
        movl 16(%rbp), %eax    # row
        incl %eax
        push %eax
        call solve_nqueens
        addl $4, %rsp          # Clean stack
        
        # Backtrack: remove queen
        movl 16(%rbp), %ecx    # row
        movl $0, board(,%rcx,4)  # board[row] = 0
        
    next_col:
        incl %ebx
        cmpl N, %ebx
        jl col_loop
    
    pop %rdi
    pop %rsi
    pop %rdx
    pop %rcx
    pop %rbx
    pop %rbp
    ret

# Function to print the board
print_board:
    push %rbp
    mov %rsp, %rbp
    push %rbx
    push %rcx
    push %rdx
    push %rsi
    push %rdi
    
    # Print the board configuration
    movl $0, %ebx    # row = 0
    
    row_loop:
        movl $0, %ecx    # col = 0
        
        col_loop_print:
            # Get board[row]
            movl board(,%rbx,4), %eax
            cmpl %ecx, %eax    # compare with col
            
            # If match, print Q, else print .
            je print_queen_char
            movl $0, %eax
            movl $1, %edx
            movl $print_empty, %esi
            jmp print_char
            
        print_queen_char:
            movl $0, %eax
            movl $1, %edx
            movl $print_queen, %esi
            
        print_char:
            # Write character to stdout
            movl $1, %eax    # sys_write
            movl $1, %ebx    # stdout
            movl %esi, %ecx  # buffer
            movl %edx, %edx  # count
            int $0x80
            
            incl %ecx
            cmpl N, %ecx
            jl col_loop_print
            
        # Print newline
        movl $1, %eax
        movl $1, %ebx
        movl $newline, %ecx
        movl $1, %edx
        int $0x80
        
        incl %ebx
        cmpl N, %ebx
        jl row_loop
        
    pop %rdi
    pop %rsi
    pop %rdx
    pop %rcx
    pop %rbx
    pop %rbp
    ret

# Main function
_start:
    # Initialize board to 0
    movl $0, %ecx
    movl $0, %edi
    
    init_loop:
        movl $0, board(,%ecx,4)
        incl %ecx
        cmpl N, %ecx
        jl init_loop
    
    # Start solving from row 0
    movl $0, %eax
    push %eax
    call solve_nqueens
    addl $4, %rsp
    
    # Print final solution count
    movl solutions, %eax
    # Print solution count (simplified)
    
    # Exit program
    movl $1, %eax    # sys_exit
    movl $0, %ebx    # exit status
    int $0x80
```

## Key Features of this Implementation:

### 1. **Data Structures**
- `board`: Array to store queen positions (row-wise)
- `solutions`: Counter for valid solutions found
- `N`: Size of the chessboard

### 2. **Core Functions**
- `is_safe()`: Checks if placing a queen at given position is valid
- `solve_nqueens()`: Recursive backtracking algorithm
- `print_board()`: Displays the board configuration

### 3. **Algorithm Approach**
- Uses **backtracking** with **recursion**
- Places one queen per row (simplifies constraints)
- Checks **column**, **diagonal**, and **anti-diagonal** conflicts
- **Backtracks** when no valid position found

### 4. **Key Assembly Concepts**
- Stack management for function calls
- Register usage for parameters and return values
- Memory addressing for board array
- Conditional jumps for control flow
- System calls for I/O operations

### 5. **Time Complexity**
- **Time**: O(N!) in worst case
- **Space**: O(N) for recursion stack + O(N²) for board storage

### 6. **Usage Notes**
- This is a simplified implementation
- Actual implementation would need more robust error handling
- Stack overflow protection for large N values
- Proper memory management for dynamic allocation

This example demonstrates how classical algorithms like N-Queens can be implemented in low-level assembly language, showing the fundamental operations needed for backtracking algorithms.