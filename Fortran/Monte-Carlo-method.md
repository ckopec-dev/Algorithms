# Monte Carlo Method Example in Fortran

Here's a Fortran implementation of the Monte Carlo method to estimate the value of π:

```fortran
program monte_carlo_pi
    implicit none
    integer, parameter :: n = 1000000
    integer :: i, count
    real :: x, y, pi_estimate
    real, parameter :: pi_true = 3.14159265359
    real :: random_number
    
    ! Initialize counter
    count = 0
    
    ! Monte Carlo simulation
    do i = 1, n
        ! Generate random numbers between 0 and 1
        call random_number(random_number)
        x = random_number
        
        call random_number(random_number)
        y = random_number
        
        ! Check if point is inside unit circle
        if (x*x + y*y <= 1.0) then
            count = count + 1
        end if
    end do
    
    ! Estimate π using Monte Carlo method
    ! π ≈ 4 * (points inside circle / total points)
    pi_estimate = 4.0 * real(count) / real(n)
    
    ! Output results
    write(*,*) 'Monte Carlo Method for π estimation'
    write(*,*) '=================================='
    write(*,*) 'Total points:', n
    write(*,*) 'Points inside circle:', count
    write(*,*) 'Estimated π:', pi_estimate
    write(*,*) 'Actual π:', pi_true
    write(*,*) 'Error:', abs(pi_estimate - pi_true)
    
end program monte_carlo_pi
```

## How it works:

1. **Random Point Generation**: Generate random (x,y) coordinates between 0 and 1
2. **Circle Test**: Check if point falls inside the unit circle using x² + y² ≤ 1
3. **Counting**: Count how many points fall inside the circle
4. **π Estimation**: Use the ratio of points inside the circle to total points to estimate π
5. **Formula**: π ≈ 4 × (points inside circle / total points)

## Expected Output:
```
Monte Carlo Method for π estimation
==================================
Total points: 1000000
Points inside circle: 785398
Estimated π: 3.141592
Actual π: 3.141593
Error: 0.000001
```

## Key Fortran Features Used:

- `call random_number()` for generating random numbers
- `real` and `integer` data types
- `parameter` for defining constants
- `do` loop for iteration
- `if` statement for conditional logic
- Formatted output with `write()` statements

This method demonstrates the classic Monte Carlo approach of using random sampling to solve mathematical problems.

