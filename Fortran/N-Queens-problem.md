# N-Queens Problem in Fortran

Here's a complete implementation of the N-Queens problem solver in Fortran:

```fortran
program n_queens
    implicit none
    integer, parameter :: MAX_N = 10
    integer :: n, i, j
    integer :: board(MAX_N)
    integer :: solutions = 0
    
    ! Read N from user
    write(*,*) 'Enter the size of the chessboard (N):'
    read(*,*) n
    
    ! Validate input
    if (n <= 0 .or. n > MAX_N) then
        write(*,*) 'Invalid board size. Please enter a value between 1 and', MAX_N
        stop
    end if
    
    ! Initialize board
    do i = 1, n
        board(i) = 0
    end do
    
    ! Solve N-Queens problem
    call solve_n_queens(board, 1, n)
    
    write(*,*) 'Total number of solutions for ', n, '-Queens problem:', solutions

contains

    recursive subroutine solve_n_queens(board, row, n)
        implicit none
        integer, intent(inout) :: board(:)
        integer, intent(in) :: row, n
        integer :: col
        
        ! Base case: if we've placed all queens
        if (row > n) then
            solutions = solutions + 1
            write(*,*) 'Solution ', solutions, ':'
            call print_board(board, n)
            return
        end if
        
        ! Try placing queen in each column of current row
        do col = 1, n
            if (is_safe(board, row, col, n)) then
                board(row) = col
                call solve_n_queens(board, row + 1, n)
            end if
        end do
    end subroutine solve_n_queens
    
    logical function is_safe(board, row, col, n)
        implicit none
        integer, intent(in) :: board(:), row, col, n
        integer :: i
        
        is_safe = .true.
        
        ! Check if queen can be placed at (row, col)
        do i = 1, row - 1
            ! Check column conflict
            if (board(i) == col) then
                is_safe = .false.
                return
            end if
            
            ! Check diagonal conflict
            if (abs(board(i) - col) == abs(i - row)) then
                is_safe = .false.
                return
            end if
        end do
    end function is_safe
    
    subroutine print_board(board, n)
        implicit none
        integer, intent(in) :: board(:), n
        integer :: i, j
        
        do i = 1, n
            do j = 1, n
                if (board(i) == j) then
                    write(*,'(A)', advance='no') 'Q '
                else
                    write(*,'(A)', advance='no') '. '
                end if
            end do
            write(*,*) ''
        end do
        write(*,*) ''
    end subroutine print_board

end program n_queens
```

## How the Algorithm Works

### Key Components:

1. **Main Program**: 
   - Reads the board size N
   - Calls the recursive solver function
   - Displays the total number of solutions

2. **solve_n_queens subroutine**:
   - Uses backtracking algorithm
   - Places queens row by row
   - Recursively explores all valid positions

3. **is_safe function**:
   - Checks if a queen can be safely placed
   - Validates column and diagonal conflicts
   - Returns .true. if position is safe

4. **print_board subroutine**:
   - Displays the chessboard with queens
   - Shows Q for queens and . for empty spaces

## Sample Output

For N=4, the output would be:
```
Enter the size of the chessboard (N):
4
Solution 1:
. Q . .
. . . Q
Q . . .
. . Q .

Solution 2:
. . Q .
Q . . .
. . . Q
. Q . .

Total number of solutions for  4 -Queens problem: 2
```

## Algorithm Complexity

- **Time Complexity**: O(N!) in worst case
- **Space Complexity**: O(N) for recursion stack
- Uses backtracking to efficiently prune invalid solutions
- Implements constraint checking for column and diagonal conflicts

This implementation solves the classic N-Queens problem by systematically trying all possible placements and backtracking when conflicts are detected.

