# Conjugate Gradient Method in Fortran

Here's a complete implementation of the Conjugate Gradient method for solving linear systems in Fortran:

```fortran
program conjugate_gradient
    implicit none
    integer, parameter :: n = 4, max_iter = 100
    real, parameter :: tol = 1.0e-6
    integer :: i, j, iter
    real :: A(n,n), b(n), x(n), r(n), p(n), Ap(n)
    real :: alpha, beta, r_norm, r_norm_old, resid
    logical :: converged
    
    ! Define a test matrix A (symmetric positive definite)
    A(1,1) = 4.0; A(1,2) = 1.0; A(1,3) = 0.0; A(1,4) = 0.0
    A(2,1) = 1.0; A(2,2) = 4.0; A(2,3) = 1.0; A(2,4) = 0.0
    A(3,1) = 0.0; A(3,2) = 1.0; A(3,3) = 4.0; A(3,4) = 1.0
    A(4,1) = 0.0; A(4,2) = 0.0; A(4,3) = 1.0; A(4,4) = 4.0
    
    ! Define right-hand side vector b
    b(1) = 5.0
    b(2) = 6.0
    b(3) = 6.0
    b(4) = 5.0
    
    ! Initial guess
    x = 0.0
    
    ! Initialize residual r = b - Ax
    do i = 1, n
        r(i) = b(i)
        do j = 1, n
            r(i) = r(i) - A(i,j) * x(j)
        end do
    end do
    
    ! Initialize search direction p = r
    p = r
    
    ! Conjugate Gradient iterations
    converged = .false.
    do iter = 1, max_iter
        ! Compute Ap = A * p
        do i = 1, n
            Ap(i) = 0.0
            do j = 1, n
                Ap(i) = Ap(i) + A(i,j) * p(j)
            end do
        end do
        
        ! Compute alpha = (r^T * r) / (p^T * Ap)
        r_norm = 0.0
        do i = 1, n
            r_norm = r_norm + r(i) * r(i)
        end do
        
        alpha = r_norm / (dot_product(p, Ap))
        
        ! Update solution: x = x + alpha * p
        do i = 1, n
            x(i) = x(i) + alpha * p(i)
        end do
        
        ! Update residual: r = r - alpha * Ap
        do i = 1, n
            r(i) = r(i) - alpha * Ap(i)
        end do
        
        ! Compute new norm of residual
        r_norm_old = r_norm
        r_norm = 0.0
        do i = 1, n
            r_norm = r_norm + r(i) * r(i)
        end do
        
        ! Check convergence
        resid = sqrt(r_norm)
        if (resid < tol) then
            converged = .true.
            print *, 'Convergence achieved after', iter, 'iterations'
            print *, 'Residual:', resid
            exit
        end if
        
        ! Compute beta = (r_new^T * r_new) / (r_old^T * r_old)
        beta = r_norm / r_norm_old
        
        ! Update search direction: p = r + beta * p
        do i = 1, n
            p(i) = r(i) + beta * p(i)
        end do
    end do
    
    ! Print results
    print *, 'Solution vector x:'
    do i = 1, n
        print *, 'x(', i, ') = ', x(i)
    end do
    
    if (.not. converged) then
        print *, 'Warning: Maximum iterations reached'
        print *, 'Final residual:', resid
    end if
    
end program conjugate_gradient
```

## Key Features of this Implementation:

1. **Matrix A**: A 4×4 symmetric positive definite matrix
2. **Vector b**: Right-hand side vector
3. **Initial guess**: Zero vector
4. **Convergence check**: Based on residual norm
5. **Iteration limit**: Maximum 100 iterations
6. **Tolerance**: 1.0e-6

## Algorithm Steps:

1. **Initialization**: Set initial guess, compute initial residual
2. **Compute search direction**: p = r
3. **Compute step size**: α = r^T·r / p^T·Ap
4. **Update solution**: x = x + α·p
5. **Update residual**: r = r - α·Ap
6. **Compute new search direction**: p = r + β·p (where β = r_new^T·r_new / r_old^T·r_old)
7. **Check convergence**: Stop if residual is below tolerance

## Expected Output:
The program will output the solution vector x and indicate convergence after a few iterations. For this test case, the solution should be approximately x = [1.0, 1.0, 1.0, 1.0].

