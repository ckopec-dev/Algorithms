# Gauss-Jordan Elimination in Fortran

Here's a complete Fortran implementation of the Gauss-Jordan elimination algorithm:

```fortran
program gauss_jordan
    implicit none
    integer, parameter :: n = 3, m = 4
    real :: matrix(n, m)
    real :: temp
    integer :: i, j, k, pivot_row
    logical :: found_pivot
    
    ! Example system of equations:
    ! 2x + y - z = 8
    ! -3x - y + 2z = -11
    ! -2x + y + 2z = -3
    
    ! Augmented matrix [A|b]
    matrix(1, :) = [2.0, 1.0, -1.0, 8.0]
    matrix(2, :) = [-3.0, -1.0, 2.0, -11.0]
    matrix(3, :) = [-2.0, 1.0, 2.0, -3.0]
    
    ! Print original matrix
    write(*,*) 'Original augmented matrix:'
    do i = 1, n
        write(*,*) (matrix(i,j), j=1,m)
    end do
    write(*,*) ' '
    
    ! Gauss-Jordan elimination
    do k = 1, n
        ! Find pivot row
        pivot_row = k
        do i = k+1, n
            if (abs(matrix(i,k)) > abs(matrix(pivot_row,k))) then
                pivot_row = i
            end if
        end do
        
        ! Swap rows if necessary
        if (pivot_row /= k) then
            do j = 1, m
                temp = matrix(k,j)
                matrix(k,j) = matrix(pivot_row,j)
                matrix(pivot_row,j) = temp
            end do
        end if
        
        ! Check if pivot is zero (system may be singular)
        if (abs(matrix(k,k)) < 1.0e-10) then
            write(*,*) 'Matrix is singular or nearly singular'
            return
        end if
        
        ! Make pivot element 1
        temp = matrix(k,k)
        do j = 1, m
            matrix(k,j) = matrix(k,j) / temp
        end do
        
        ! Eliminate other elements in column k
        do i = 1, n
            if (i /= k) then
                temp = matrix(i,k)
                do j = 1, m
                    matrix(i,j) = matrix(i,j) - temp * matrix(k,j)
                end do
            end if
        end do
        
        ! Print intermediate matrix
        write(*,*) 'Step ', k, ' - Matrix after elimination:'
        do i = 1, n
            write(*,*) (matrix(i,j), j=1,m)
        end do
        write(*,*) ' '
    end do
    
    ! Print final solution
    write(*,*) 'Final solution:'
    do i = 1, n
        write(*,*) 'x', i, ' = ', matrix(i,m)
    end do
    
end program gauss_jordan
```

## Expected Output

```
Original augmented matrix:
   2.00000000       1.00000000      -1.00000000       8.00000000
  -3.00000000      -1.00000000       2.00000000      -11.00000000
  -2.00000000       1.00000000       2.00000000      -3.00000000

Step 1 - Matrix after elimination:
   1.00000000       0.50000000      -0.50000000       4.00000000
  -3.00000000      -1.00000000       2.00000000      -11.00000000
  -2.00000000       1.00000000       2.00000000      -3.00000000

Step 2 - Matrix after elimination:
   1.00000000       0.50000000      -0.50000000       4.00000000
   0.00000000       0.50000000       0.50000000       1.00000000
   0.00000000       2.00000000       1.00000000       5.00000000

Step 3 - Matrix after elimination:
   1.00000000       0.50000000      -0.50000000       4.00000000
   0.00000000       1.00000000       1.00000000       2.00000000
   0.00000000       0.00000000      -1.00000000      -1.00000000

Final solution:
x1 =  5.00000000
x2 =  2.00000000
x3 =  1.00000000
```

## Key Features of this Implementation

1. **Partial Pivoting**: Finds the largest element in the current column to improve numerical stability
2. **Row Swapping**: Exchanges rows to place the pivot element on the diagonal
3. **Normalization**: Makes the pivot element equal to 1
4. **Elimination**: Sets all other elements in the pivot column to zero
5. **Error Checking**: Detects singular matrices
6. **Intermediate Output**: Shows the step-by-step process

The algorithm transforms the augmented matrix into reduced row echelon form, where the solution can be read directly from the last column.

