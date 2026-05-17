# Karmarkar's Algorithm in Fortran

Here's an implementation of Karmarkar's algorithm for solving linear programming problems in Fortran:

```fortran
program karmarkar_algorithm
    implicit none
    integer, parameter :: n = 3, m = 2
    integer :: i, j, iter
    real :: epsilon = 1.0e-6
    real :: x(n), x_new(n)
    real :: A(m,n), b(m)
    real :: c(n)
    real :: alpha, beta, gamma
    real :: obj_val, obj_val_new
    
    ! Initialize problem data
    ! Minimize: c^T * x
    ! Subject to: A * x = b, x >= 0
    
    ! Coefficient matrix A (m x n)
    A(1,1) = 1.0; A(1,2) = 2.0; A(1,3) = 1.0
    A(2,1) = 2.0; A(2,2) = 1.0; A(2,3) = 1.0
    
    ! Right-hand side vector b (m x 1)
    b(1) = 4.0
    b(2) = 5.0
    
    ! Cost vector c (n x 1)
    c(1) = 1.0
    c(2) = 1.0
    c(3) = 2.0
    
    ! Initial feasible point (interior point)
    x(1) = 1.0
    x(2) = 1.0
    x(3) = 1.0
    
    ! Normalize initial point to satisfy constraints
    call normalize_point(x, A, b, n, m)
    
    ! Karmarkar's algorithm iterations
    do iter = 1, 1000
        ! Calculate objective value
        obj_val = dot_product(c, x)
        
        ! Calculate gradient of objective function
        call calculate_gradient(x, c, n, gamma)
        
        ! Calculate search direction
        call calculate_direction(x, A, b, c, gamma, x_new, n, m)
        
        ! Update solution
        alpha = 0.5  ! Step size
        do i = 1, n
            x_new(i) = x(i) + alpha * (x_new(i) - x(i))
        end do
        
        ! Check convergence
        if (abs(obj_val - obj_val_new) < epsilon) then
            print *, 'Convergence achieved after ', iter, ' iterations'
            exit
        end if
        
        ! Update current solution
        do i = 1, n
            x(i) = x_new(i)
        end do
        
        ! Ensure point remains feasible
        call project_to_feasible(x, A, b, n, m)
        
        ! Calculate new objective value
        obj_val_new = dot_product(c, x)
        
        if (mod(iter, 10) == 0) then
            print *, 'Iteration ', iter, ': Objective = ', obj_val_new
        end if
    end do
    
    ! Print final solution
    print *, 'Final solution:'
    do i = 1, n
        print *, 'x(', i, ') = ', x(i)
    end do
    print *, 'Optimal objective value = ', obj_val_new
    
end program karmarkar_algorithm

subroutine normalize_point(x, A, b, n, m)
    implicit none
    integer, intent(in) :: n, m
    real, intent(inout) :: x(n)
    real, intent(in) :: A(m,n), b(m)
    real :: temp, sum_x
    integer :: i, j
    
    ! Normalize point to satisfy constraints
    do i = 1, m
        temp = 0.0
        do j = 1, n
            temp = temp + A(i,j) * x(j)
        end do
        if (abs(temp - b(i)) > 1.0e-6) then
            ! Adjust x to satisfy constraint
            sum_x = 0.0
            do j = 1, n
                sum_x = sum_x + x(j)
            end do
            if (sum_x > 0.0) then
                do j = 1, n
                    x(j) = x(j) * b(i) / temp
                end do
            end if
        end if
    end do
end subroutine normalize_point

subroutine calculate_gradient(x, c, n, gamma)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: x(n), c(n)
    real, intent(out) :: gamma
    integer :: i
    
    ! Calculate gradient of objective function
    gamma = 0.0
    do i = 1, n
        gamma = gamma + c(i) * x(i)
    end do
end subroutine calculate_gradient

subroutine calculate_direction(x, A, b, c, gamma, direction, n, m)
    implicit none
    integer, intent(in) :: n, m
    real, intent(in) :: x(n), A(m,n), b(m), c(n), gamma
    real, intent(out) :: direction(n)
    integer :: i, j
    real :: temp
    
    ! Calculate search direction (simplified version)
    do i = 1, n
        direction(i) = 0.0
        do j = 1, m
            direction(i) = direction(i) + A(j,i) * (b(j) - dot_product(A(j,:), x))
        end do
        direction(i) = direction(i) - gamma * c(i)
    end do
end subroutine calculate_direction

subroutine project_to_feasible(x, A, b, n, m)
    implicit none
    integer, intent(in) :: n, m
    real, intent(inout) :: x(n)
    real, intent(in) :: A(m,n), b(m)
    integer :: i, j
    real :: temp
    
    ! Ensure all variables are positive
    do i = 1, n
        if (x(i) < 1.0e-6) then
            x(i) = 1.0e-6
        end if
    end do
    
    ! Project to satisfy constraints (simplified)
    do i = 1, m
        temp = 0.0
        do j = 1, n
            temp = temp + A(i,j) * x(j)
        end do
        if (abs(temp - b(i)) > 1.0e-6) then
            ! Simple projection - adjust variables
            do j = 1, n
                x(j) = x(j) * b(i) / temp
            end do
        end if
    end do
end subroutine project_to_feasible
```

## Key Features of this Implementation:

1. **Problem Setup**: Defines a linear programming problem with constraints
2. **Initialization**: Sets up an initial feasible interior point
3. **Core Algorithm**: Implements the iterative steps of Karmarkar's algorithm
4. **Convergence Check**: Monitors the objective function improvement
5. **Feasibility Maintenance**: Ensures the solution remains within constraints
6. **Output**: Displays iteration progress and final results

## Algorithm Steps:
1. Initialize interior feasible point
2. Calculate search direction using gradient information
3. Update solution with step size
4. Project to maintain feasibility
5. Check convergence criteria
6. Repeat until optimal solution found

This is a simplified version that demonstrates the core concepts of Karmarkar's algorithm. A full implementation would require more sophisticated handling of matrix operations and convergence criteria.

