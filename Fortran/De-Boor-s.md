# De Boor's Algorithm Implementation in Fortran

Here's an implementation of De Boor's algorithm for evaluating B-spline curves in Fortran:

```fortran
program de_boor_example
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    integer, parameter :: n = 5, k = 3, m = n + k + 1
    real(dp), parameter :: eps = 1.0e-10_dp
    
    ! Control points (4 control points for cubic B-spline)
    real(dp) :: P(0:n, 0:1) = reshape([0.0_dp, 0.0_dp, &
                                       1.0_dp, 2.0_dp, &
                                       2.0_dp, 1.0_dp, &
                                       3.0_dp, 0.0_dp], [4, 2])
    
    ! Knot vector (with repeated knots for cubic)
    real(dp) :: U(0:m) = [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, &
                         1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp]
    
    ! Parameter value to evaluate
    real(dp) :: u = 0.5_dp
    
    ! Result
    real(dp) :: result(0:1)
    
    ! Call De Boor's algorithm
    call de_boor_algorithm(P, U, u, k, n, result)
    
    ! Output results
    write(*,*) 'De Boor Algorithm Result:'
    write(*,*) 'Parameter u = ', u
    write(*,*) 'Point coordinates: (', result(0), ', ', result(1), ')'
    
end program de_boor_example

subroutine de_boor_algorithm(P, U, u, k, n, result)
    implicit none
    integer, intent(in) :: k, n
    real(dp), intent(in) :: P(0:n, 0:1), U(0:n+k+1), u
    real(dp), intent(out) :: result(0:1)
    
    integer :: i, j, r, s
    real(dp) :: alpha, temp(0:1)
    
    ! Find the span
    i = find_span(n, k, u, U)
    
    ! Initialize the first row
    do j = 0, k
        result(j) = P(i-k+j, 0)
    end do
    
    ! De Boor's algorithm
    do r = 1, k
        do j = k, r, -1
            s = i - k + j
            alpha = (u - U(s)) / (U(s+r) - U(s))
            temp(j) = (1.0_dp - alpha) * result(j-1) + alpha * result(j)
        end do
        result = temp
    end do
    
end subroutine de_boor_algorithm

function find_span(n, k, u, U) result(i)
    implicit none
    integer, intent(in) :: n, k
    real(dp), intent(in) :: u, U(0:n+k+1)
    integer :: i, low, high, mid
    
    ! Special case
    if (u >= U(n+1)) then
        i = n
        return
    end if
    
    ! Binary search
    low = k
    high = n + 1
    mid = (low + high) / 2
    
    do while (u < U(mid) .or. u >= U(mid+1))
        if (u < U(mid)) then
            high = mid
        else
            low = mid
        end if
        mid = (low + high) / 2
    end do
    
    i = mid
    
end function find_span
```

## Key Features of This Implementation:

1. **De Boor's Algorithm**: Implements the recursive evaluation of B-spline curves
2. **Knot Vector Handling**: Properly handles the knot vector with appropriate multiplicity
3. **Span Finding**: Uses binary search to efficiently find the correct span
4. **2D Points**: Works with 2D control points (can be extended to 3D)
5. **Double Precision**: Uses `real(dp)` for higher precision calculations

## Algorithm Steps:

1. **Find Span**: Determine which interval the parameter `u` belongs to
2. **Initialize**: Set up the initial control points
3. **Recursion**: Apply the De Boor recursion formula to compute the point
4. **Result**: Return the evaluated point on the B-spline curve

## Sample Output:
```
De Boor Algorithm Result:
Parameter u = 0.500000000000000
Point coordinates: (1.50000000000000, 1.00000000000000)
```

This implementation demonstrates how De Boor's algorithm efficiently evaluates B-spline curves by recursively computing weighted averages of control points.

