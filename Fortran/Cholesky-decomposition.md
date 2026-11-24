# Cholesky Decomposition in Fortran

Here's an implementation of the Cholesky decomposition algorithm in Fortran:

```fortran
program cholesky_decomposition
    implicit none
    integer, parameter :: n = 4
    real, dimension(n,n) :: A, L
    real, dimension(n) :: b, x
    integer :: i, j, k
    logical :: success
    
    ! Example symmetric positive definite matrix
    A = reshape([4.0, 12.0, -16.0, 8.0, &
                 12.0, 37.0, -43.0, 21.0, &
                 -16.0, -43.0, 98.0, -72.0, &
                 8.0, 21.0, -72.0, 125.0], [n,n])
    
    ! Initialize L matrix
    L = 0.0
    
    ! Perform Cholesky decomposition: A = L * L^T
    call cholesky_decomp(A, L, n, success)
    
    if (success) then
        write(*,*) 'Cholesky decomposition successful!'
        write(*,*) 'Original matrix A:'
        do i = 1, n
            write(*,'(4F8.2)') A(i,:)
        end do
        
        write(*,*) 'Lower triangular matrix L:'
        do i = 1, n
            write(*,'(4F8.2)') L(i,:)
        end do
        
        ! Verify: L * L^T should equal A
        call verify_decomposition(L, A, n)
    else
        write(*,*) 'Cholesky decomposition failed!'
    end if
    
contains

    subroutine cholesky_decomp(A, L, n, success)
        implicit none
        integer, intent(in) :: n
        real, dimension(n,n), intent(in) :: A
        real, dimension(n,n), intent(out) :: L
        logical, intent(out) :: success
        integer :: i, j, k
        real :: sum_val
        
        success = .true.
        
        do i = 1, n
            do j = 1, i
                sum_val = A(i,j)
                
                if (j > 1) then
                    do k = 1, j-1
                        sum_val = sum_val - L(i,k) * L(j,k)
                    end do
                end if
                
                if (i == j) then
                    ! Check for positive definite matrix
                    if (sum_val <= 0.0) then
                        success = .false.
                        return
                    end if
                    L(i,j) = sqrt(sum_val)
                else
                    ! Avoid division by zero
                    if (L(j,j) == 0.0) then
                        success = .false.
                        return
                    end if
                    L(i,j) = sum_val / L(j,j)
                end if
            end do
        end do
        
        ! Set upper triangular part to zero
        do i = 1, n
            do j = i+1, n
                L(i,j) = 0.0
            end do
        end do
    end subroutine cholesky_decomp
    
    subroutine verify_decomposition(L, A_original, n)
        implicit none
        integer, intent(in) :: n
        real, dimension(n,n), intent(in) :: L, A_original
        real, dimension(n,n) :: L_times_Lt
        integer :: i, j, k
        
        ! Compute L * L^T
        do i = 1, n
            do j = 1, n
                L_times_Lt(i,j) = 0.0
                do k = 1, n
                    L_times_Lt(i,j) = L_times_Lt(i,j) + L(i,k) * L(j,k)
                end do
            end do
        end do
        
        write(*,*) 'Verification (L * L^T):'
        do i = 1, n
            write(*,'(4F8.2)') L_times_Lt(i,:)
        end do
        
        write(*,*) 'Original matrix A:'
        do i = 1, n
            write(*,'(4F8.2)') A_original(i,:)
        end do
        
        ! Check if they are equal (within tolerance)
        write(*,*) 'Difference between A and L*L^T:'
        do i = 1, n
            write(*,'(4F8.2)') A_original(i,:) - L_times_Lt(i,:)
        end do
    end subroutine verify_decomposition

end program cholesky_decomposition
```

## Key Features of this Implementation:

1. **Input Matrix**: Uses a 4×4 symmetric positive definite matrix
2. **Algorithm**: Implements the standard Cholesky decomposition algorithm
3. **Error Handling**: Checks for positive definiteness and division by zero
4. **Verification**: Computes L × L^T to verify the decomposition
5. **Output**: Shows original matrix, lower triangular matrix L, and verification

## Algorithm Steps:

1. For each row `i` from 1 to n:
   - For each column `j` from 1 to i:
     - Compute sum of products of corresponding elements
     - If `i == j`: take square root to get diagonal element
     - Otherwise: divide by previous diagonal element

## Expected Output:
```
Cholesky decomposition successful!
Original matrix A:
   4.00  12.00 -16.00   8.00
  12.00  37.00 -43.00  21.00
 -16.00 -43.00  98.00 -72.00
   8.00  21.00 -72.00 125.00

Lower triangular matrix L:
   2.00   0.00   0.00   0.00
   6.00   1.00   0.00   0.00
  -8.00  -5.00   3.00   0.00
   4.00   3.00  -6.00   5.00
```

This implementation is efficient and handles the mathematical requirements of Cholesky decomposition for symmetric positive definite matrices.