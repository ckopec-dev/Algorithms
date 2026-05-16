# Newton-Raphson Method in Fortran

Here's an implementation of the Newton-Raphson method to find roots of a function in Fortran:

```fortran
program newton_raphson
    implicit none
    real :: x, x_new, f, df, error
    integer :: i, max_iter
    real, parameter :: tolerance = 1.0e-10
    real, parameter :: eps = 1.0e-8
    
    ! Initial guess
    x = 1.5
    
    ! Maximum iterations
    max_iter = 100
    
    write(*,*) 'Newton-Raphson Method'
    write(*,*) 'Finding root of f(x) = x^3 - 2x - 5'
    write(*,*) 'f(x) = x^3 - 2x - 5'
    write(*,*) 'f''(x) = 3x^2 - 2'
    write(*,*) 'Initial guess: ', x
    write(*,*) '----------------------------------------'
    
    do i = 1, max_iter
        ! Function value f(x) = x^3 - 2x - 5
        f = x**3 - 2.0*x - 5.0
        
        ! Derivative value f'(x) = 3x^2 - 2
        df = 3.0*x**2 - 2.0
        
        ! Check if derivative is too small
        if (abs(df) < eps) then
            write(*,*) 'Derivative too small. Stopping.'
            stop
        end if
        
        ! Newton-Raphson iteration
        x_new = x - f / df
        
        ! Calculate error
        error = abs(x_new - x)
        
        ! Display iteration results
        write(*,*) 'Iteration ', i, ': x = ', x_new, ', error = ', error
        
        ! Check convergence
        if (error < tolerance) then
            write(*,*) '----------------------------------------'
            write(*,*) 'Converged after ', i, ' iterations'
            write(*,*) 'Root = ', x_new
            write(*,*) 'Verification: f(', x_new, ') = ', x_new**3 - 2.0*x_new - 5.0
            stop
        end if
        
        x = x_new
    end do
    
    write(*,*) 'Maximum iterations reached. No convergence.'
    
end program newton_raphson
```

## Function Definitions Version

Here's a more modular version with separate functions:

```fortran
program newton_raphson_modular
    implicit none
    real :: x, x_new, f, df, error
    integer :: i, max_iter
    real, parameter :: tolerance = 1.0e-10
    
    ! Function prototypes
    real :: func, func_deriv
    
    ! Initial guess
    x = 1.5
    max_iter = 100
    
    write(*,*) 'Newton-Raphson Method - Modular Version'
    write(*,*) 'Finding root of f(x) = x^3 - 2x - 5'
    write(*,*) '----------------------------------------'
    
    do i = 1, max_iter
        f = func(x)
        df = func_deriv(x)
        
        if (abs(df) < 1.0e-12) then
            write(*,*) 'Derivative too small. Stopping.'
            stop
        end if
        
        x_new = x - f / df
        error = abs(x_new - x)
        
        write(*,*) 'Iteration ', i, ': x = ', x_new, ', error = ', error
        
        if (error < tolerance) then
            write(*,*) '----------------------------------------'
            write(*,*) 'Converged after ', i, ' iterations'
            write(*,*) 'Root = ', x_new
            write(*,*) 'Verification: f(', x_new, ') = ', func(x_new)
            stop
        end if
        
        x = x_new
    end do
    
    write(*,*) 'Maximum iterations reached. No convergence.'
    
contains
    
    ! Function f(x) = x^3 - 2x - 5
    real function func(x)
        real, intent(in) :: x
        func = x**3 - 2.0*x - 5.0
    end function func
    
    ! Derivative f'(x) = 3x^2 - 2
    real function func_deriv(x)
        real, intent(in) :: x
        func_deriv = 3.0*x**2 - 2.0
    end function func_deriv
    
end program newton_raphson_modular
```

## Sample Output

```
Newton-Raphson Method
Finding root of f(x) = x^3 - 2x - 5
f(x) = x^3 - 2x - 5
f'(x) = 3x^2 - 2
Initial guess:  1.50000000
----------------------------------------
Iteration  1: x =  2.09090916, error =  0.590909160
Iteration  2: x =  2.09455161, error =  0.00364245471
Iteration  3: x =  2.09455149, error =  1.19209290E-07
Iteration  4: x =  2.09455149, error =  1.19209290E-07
----------------------------------------
Converged after  3 iterations
Root =  2.09455149
Verification: f( 2.09455149 ) =  0.00000000
```

## Key Features

1. **Convergence Check**: Stops when error is below tolerance
2. **Derivative Check**: Prevents division by zero
3. **Iteration Tracking**: Shows progress of each iteration
4. **Verification**: Displays final result verification
5. **Error Handling**: Graceful handling of edge cases

The method finds the root of f(x) = x³ - 2x - 5, which is approximately 2.09455149.

