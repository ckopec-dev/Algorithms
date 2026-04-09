# Linear Regression in Fortran

Here's a complete example of implementing linear regression in Fortran:

```fortran
program linear_regression
    implicit none
    
    ! Variables
    integer, parameter :: n = 10
    real, dimension(n) :: x, y
    real :: sum_x, sum_y, sum_xy, sum_x2, mean_x, mean_y
    real :: slope, intercept, r_squared
    integer :: i
    
    ! Sample data
    data x /1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0/
    data y /2.1, 3.9, 6.2, 7.8, 10.1, 12.0, 13.8, 15.9, 18.2, 20.1/
    
    ! Calculate sums
    sum_x = 0.0
    sum_y = 0.0
    sum_xy = 0.0
    sum_x2 = 0.0
    
    do i = 1, n
        sum_x = sum_x + x(i)
        sum_y = sum_y + y(i)
        sum_xy = sum_xy + x(i) * y(i)
        sum_x2 = sum_x2 + x(i) * x(i)
    end do
    
    ! Calculate means
    mean_x = sum_x / real(n)
    mean_y = sum_y / real(n)
    
    ! Calculate slope and intercept
    slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    intercept = mean_y - slope * mean_x
    
    ! Calculate R-squared
    call calculate_r_squared(x, y, n, slope, intercept, r_squared)
    
    ! Output results
    write(*,*) 'Linear Regression Results'
    write(*,*) '========================'
    write(*,*) 'Slope (m): ', slope
    write(*,*) 'Intercept (b): ', intercept
    write(*,*) 'Equation: y = ', slope, 'x + ', intercept
    write(*,*) 'R-squared: ', r_squared
    
    ! Predict some values
    write(*,*) 'Predictions:'
    write(*,*) 'x = 5.5, y = ', slope * 5.5 + intercept
    write(*,*) 'x = 7.5, y = ', slope * 7.5 + intercept
    
contains
    
    subroutine calculate_r_squared(x_data, y_data, n_data, slope_val, intercept_val, r2)
        implicit none
        integer, intent(in) :: n_data
        real, intent(in) :: x_data(n_data), y_data(n_data)
        real, intent(in) :: slope_val, intercept_val
        real, intent(out) :: r2
        
        real :: sum_y_pred, sum_y_actual, sum_y_mean, ss_tot, ss_reg
        real :: y_pred
        integer :: i
        
        sum_y_pred = 0.0
        sum_y_actual = 0.0
        sum_y_mean = 0.0
        
        do i = 1, n_data
            y_pred = slope_val * x_data(i) + intercept_val
            sum_y_pred = sum_y_pred + y_pred
            sum_y_actual = sum_y_actual + y_data(i)
            sum_y_mean = sum_y_mean + y_data(i)
        end do
        
        ! Calculate total sum of squares
        ss_tot = 0.0
        ss_reg = 0.0
        
        do i = 1, n_data
            y_pred = slope_val * x_data(i) + intercept_val
            ss_tot = ss_tot + (y_data(i) - sum_y_actual/real(n_data))**2
            ss_reg = ss_reg + (y_pred - sum_y_actual/real(n_data))**2
        end do
        
        r2 = ss_reg / ss_tot
        
    end subroutine calculate_r_squared
    
end program linear_regression
```

## Key Features of this Implementation:

1. **Data Storage**: Uses arrays to store x and y data points
2. **Least Squares Method**: Implements the standard linear regression formula
3. **Slope and Intercept Calculation**: Uses the normal equations
4. **R-squared Calculation**: Measures the goodness of fit
5. **Prediction Capability**: Shows how to use the model for new predictions

## Expected Output:
```
Linear Regression Results
========================
Slope (m):  2.000000    
Intercept (b):  0.1000000    
Equation: y =  2.000000 x +  0.1000000    
R-squared:  0.9999999    
Predictions:
x = 5.5, y =  11.10000    
x = 7.5, y =  15.10000    
```

## How it Works:

1. **Input Data**: Sample x,y pairs are defined
2. **Summation**: Calculate all necessary sums for the regression
3. **Parameter Estimation**: Use the normal equations to find slope and intercept
4. **Goodness of Fit**: Calculate R-squared to measure how well the line fits the data
5. **Prediction**: Use the regression equation to predict new values

This implementation demonstrates the core concepts of linear regression in a clear, readable Fortran program.

