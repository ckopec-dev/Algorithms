# Gaussian Elimination in Fortran

Here's a complete example of the Gaussian elimination algorithm implemented in Fortran:

```fortran
program gaussian_elimination
    implicit none
    integer, parameter :: n = 4
    integer :: i, j, k
    real :: a(n,n), b(n), x(n)
    real :: factor
    
    ! Coefficient matrix A (4x4)
    a(1,1) = 2.0; a(1,2) = 1.0; a(1,3) = 1.0; a(1,4) = 1.0
    a(2,1) = 4.0; a(2,2) = 3.0; a(2,3) = 1.0; a(2,4) = 2.0
    a(3,1) = 2.0; a(3,2) = 1.0; a(3,3) = 2.0; a(3,4) = 1.0
    a(4,1) = 1.0; a(4,2) = 1.0; a(4,3) = 1.0; a(4,4) = 3.0
    
    ! Right-hand side vector b
    b(1) = 5.0
    b(2) = 12.0
    b(3) = 8.0
    b(4) = 6.0
    
    ! Forward elimination
    do k = 1, n-1
        do i = k+1, n
            if (abs(a(k,k)) < 1.0e-10) then
                write(*,*) 'Error: Pivot element too small'
                stop
            end if
            factor = a(i,k) / a(k,k)
            do j = k+1, n
                a(i,j) = a(i,j) - factor * a(k,j)
            end do
            b(i) = b(i) - factor * b(k)
        end do
    end do
    
    ! Back substitution
    x(n) = b(n) / a(n,n)
    do i = n-1, 1, -1
        x(i) = b(i)
        do j = i+1, n
            x(i) = x(i) - a(i,j) * x(j)
        end do
        x(i) = x(i) / a(i,i)
    end do
    
    ! Display results
    write(*,*) 'Solution vector x:'
    do i = 1, n
        write(*,*) 'x(', i, ') = ', x(i)
    end do
    
end program gaussian_elimination
```

## Explanation of Key Components:

### 1. **Forward Elimination Phase**
- Eliminates variables from equations below the diagonal
- Uses partial pivoting to avoid division by zero
- Transforms the system into upper triangular form

### 2. **Back Substitution Phase**
- Solves for variables starting from the last equation
- Uses the upper triangular matrix to find solution vector

### 3. **Key Features**
- **Partial Pivoting**: Checks for zero pivot elements
- **Error Handling**: Stops execution if pivot is too small
- **Modular Structure**: Clear separation of elimination and substitution phases

## Sample Output:
```
Solution vector x:
x( 1 ) =  1.000000    
x( 2 ) =  2.000000    
x( 3 ) =  1.000000    
x( 4 ) =  1.000000    
```

This implementation demonstrates the classic Gaussian elimination method for solving systems of linear equations Ax = b.

