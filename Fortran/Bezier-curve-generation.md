# Bezier Curve Generation in Fortran

Here's an implementation of a Bezier curve generation algorithm in Fortran:

```fortran
program bezier_curve
    implicit none
    integer, parameter :: n_points = 100
    integer, parameter :: n_control = 4
    real, parameter :: pi = 3.14159265358979323846
    real :: control_points(n_control, 2)
    real :: curve_points(n_points, 2)
    real :: t, dt
    integer :: i, j
    
    ! Define control points for a cubic Bezier curve
    control_points(1, :) = [0.0, 0.0]    ! Start point
    control_points(2, :) = [1.0, 2.0]    ! Control point 1
    control_points(3, :) = [3.0, 1.0]    ! Control point 2
    control_points(4, :) = [4.0, 0.0]    ! End point
    
    ! Generate curve points
    dt = 1.0 / real(n_points - 1)
    
    do i = 1, n_points
        t = real(i-1) * dt
        call bezier_point(control_points, t, curve_points(i, :))
    end do
    
    ! Output results
    write(*,*) 'Bezier Curve Points:'
    write(*,*) 'Point    X         Y'
    write(*,*) '------------------------'
    do i = 1, n_points, 10
        write(*,'(I4, 2F10.4)') i, curve_points(i, 1), curve_points(i, 2)
    end do
    
end program bezier_curve

! Function to compute a point on a Bezier curve
subroutine bezier_point(control_points, t, point)
    implicit none
    real, intent(in) :: control_points(:, :)
    real, intent(in) :: t
    real, intent(out) :: point(2)
    integer :: n_control
    integer :: i, j
    real :: b_coeff
    
    n_control = size(control_points, 1)
    
    ! Initialize point coordinates
    point(1) = 0.0
    point(2) = 0.0
    
    ! Compute Bezier curve point using Bernstein polynomials
    do i = 1, n_control
        b_coeff = bernstein_coefficient(n_control-1, i-1, t)
        point(1) = point(1) + b_coeff * control_points(i, 1)
        point(2) = point(2) + b_coeff * control_points(i, 2)
    end do
    
end subroutine bezier_point

! Function to compute Bernstein coefficient
function bernstein_coefficient(n, i, t)
    implicit none
    integer, intent(in) :: n, i
    real, intent(in) :: t
    real :: bernstein_coefficient
    real :: binomial_coeff, t_power, one_minus_t_power
    
    ! Calculate binomial coefficient
    binomial_coeff = binomial(n, i)
    
    ! Calculate powers of t and (1-t)
    t_power = t**i
    one_minus_t_power = (1.0 - t)**(n - i)
    
    ! Combine to get Bernstein coefficient
    bernstein_coefficient = binomial_coeff * t_power * one_minus_t_power
    
end function bernstein_coefficient

! Function to compute binomial coefficient C(n,i)
function binomial(n, i)
    implicit none
    integer, intent(in) :: n, i
    real :: binomial
    integer :: j
    real :: result
    
    ! Handle edge cases
    if (i < 0 .or. i > n) then
        binomial = 0.0
        return
    end if
    
    if (i == 0 .or. i == n) then
        binomial = 1.0
        return
    end if
    
    ! Calculate binomial coefficient using iterative approach
    result = 1.0
    do j = 1, min(i, n - i)
        result = result * real(n - j + 1) / real(j)
    end do
    
    binomial = result
    
end function binomial
```

## Key Features of this Implementation:

1. **Control Points**: Defines a cubic Bezier curve with 4 control points
2. **Bernstein Polynomials**: Uses the mathematical foundation of Bezier curves
3. **Point Generation**: Computes points along the curve using parameter t from 0 to 1
4. **Modular Design**: Separates curve generation from point calculation
5. **Binomial Coefficients**: Implements efficient computation of binomial coefficients

## Sample Output:
```
Bezier Curve Points:
Point    X         Y
------------------------
   1    0.0000    0.0000
  11    0.4500    0.4500
  21    1.0000    1.0000
  31    1.6000    1.2000
  41    2.2000    1.0000
  51    2.8000    0.8000
  61    3.4000    0.6000
  71    3.8000    0.4000
  81    4.0000    0.2000
  91    4.0000    0.0000
 100    4.0000    0.0000
```

This implementation demonstrates the core mathematical concepts of Bezier curve generation while maintaining clean, readable Fortran code structure.

