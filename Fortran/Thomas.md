# Thomas Algorithm in Fortran

The Thomas algorithm is a specialized method for solving tridiagonal systems of linear equations. Here's an implementation in Fortran:

```fortran
program thomas_algorithm
    implicit none
    integer, parameter :: n = 5
    integer :: i
    real :: a(n), b(n), c(n), d(n)
    real :: x(n)
    
    ! Initialize the tridiagonal system coefficients
    ! System: a[i]*x[i-1] + b[i]*x[i] + c[i]*x[i+1] = d[i]
    
    ! Coefficients for the tridiagonal matrix
    a(1) = 0.0    ! a[1] is not used (first row)
    b(1) = 2.0
    c(1) = 1.0
    d(1) = 5.0
    
    a(2) = 1.0
    b(2) = 4.0
    c(2) = 2.0
    d(2) = 10.0
    
    a(3) = 2.0
    b(3) = 5.0
    c(3) = 1.0
    d(3) = 15.0
    
    a(4) = 1.0
    b(4) = 3.0
    c(4) = 2.0
    d(4) = 12.0
    
    a(5) = 2.0    ! a[5] is not used (last row)
    b(5) = 6.0
    c(5) = 0.0    ! c[5] is not used (last row)
    d(5) = 20.0
    
    ! Solve using Thomas algorithm
    call thomas_solve(a, b, c, d, x, n)
    
    ! Display results
    write(*,*) 'Solution vector x:'
    do i = 1, n
        write(*,*) 'x(', i, ') = ', x(i)
    end do
    
end program thomas_algorithm

subroutine thomas_solve(a, b, c, d, x, n)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: a(n), b(n), c(n), d(n)
    real, intent(out) :: x(n)
    real :: c_prime(n), d_prime(n)
    integer :: i
    
    ! Forward elimination
    c_prime(1) = c(1) / b(1)
    d_prime(1) = d(1) / b(1)
    
    do i = 2, n-1
        c_prime(i) = c(i) / (b(i) - a(i) * c_prime(i-1))
        d_prime(i) = (d(i) - a(i) * d_prime(i-1)) / (b(i) - a(i) * c_prime(i-1))
    end do
    
    ! Back substitution
    x(n) = (d(n) - a(n) * d_prime(n-1)) / (b(n) - a(n) * c_prime(n-1))
    
    do i = n-1, 1, -1
        x(i) = d_prime(i) - c_prime(i) * x(i+1)
    end do
    
end subroutine thomas_solve
```

## Explanation of the Thomas Algorithm Implementation

### Key Components:

1. **Forward Elimination Phase**:
   - Transform the system into an upper triangular form
   - Compute `c_prime` and `d_prime` arrays
   - `c_prime(i) = c(i) / (b(i) - a(i) * c_prime(i-1))`
   - `d_prime(i) = (d(i) - a(i) * d_prime(i-1)) / (b(i) - a(i) * c_prime(i-1))`

2. **Back Substitution Phase**:
   - Solve for the last variable
   - `x(n) = (d(n) - a(n) * d_prime(n-1)) / (b(n) - a(n) * c_prime(n-1))`
   - Then work backwards: `x(i) = d_prime(i) - c_prime(i) * x(i+1)`

### Input Format:
The tridiagonal system is represented as:
```
b[1]*x[1] + c[1]*x[2] = d[1]
a[2]*x[1] + b[2]*x[2] + c[2]*x[3] = d[2]
a[3]*x[2] + b[3]*x[3] + c[3]*x[4] = d[3]
...
a[n]*x[n-1] + b[n]*x[n] = d[n]
```

### Time Complexity:
- **Forward elimination**: O(n)
- **Back substitution**: O(n)
- **Total**: O(n) - much faster than general Gaussian elimination O(n³)

This implementation efficiently solves tridiagonal systems that commonly arise in numerical methods for differential equations, finite difference methods, and other computational applications.

