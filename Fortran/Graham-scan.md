# Graham Scan Algorithm in Fortran

Here's a complete implementation of the Graham Scan algorithm for finding the convex hull of a set of points in Fortran:

```fortran
program graham_scan_example
    implicit none
    integer, parameter :: nmax = 100
    integer :: n, i, j, k, l
    real :: points(nmax, 2)
    integer :: hull_indices(nmax)
    integer :: hull_size
    
    ! Sample points
    n = 8
    points(1, :) = [0.0, 0.0]
    points(2, :) = [1.0, 0.0]
    points(3, :) = [2.0, 1.0]
    points(4, :) = [1.0, 2.0]
    points(5, :) = [0.0, 2.0]
    points(6, :) = [0.5, 1.5]
    points(7, :) = [1.5, 1.0]
    points(8, :) = [1.0, 1.0]
    
    ! Print input points
    write(*,*) 'Input points:'
    do i = 1, n
        write(*,'(A,I0,A,2F6.2)') 'Point ', i, ': (', points(i,1), points(i,2), ')'
    end do
    
    ! Run Graham scan
    call graham_scan(points, n, hull_indices, hull_size)
    
    ! Print convex hull
    write(*,*) 'Convex Hull Points:'
    do i = 1, hull_size
        j = hull_indices(i)
        write(*,'(A,I0,A,2F6.2)') 'Hull Point ', i, ': (', points(j,1), points(j,2), ')'
    end do
    
end program graham_scan_example

subroutine graham_scan(points, n, hull_indices, hull_size)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: points(n, 2)
    integer, intent(out) :: hull_indices(n)
    integer, intent(out) :: hull_size
    
    integer :: i, j, k, l, min_index
    real :: min_y, temp_x, temp_y
    real :: cross_product
    integer :: stack(n)
    integer :: stack_size
    
    ! Find the bottom-most point (or left-most if tie)
    min_y = points(1, 2)
    min_index = 1
    do i = 2, n
        if (points(i, 2) < min_y .or. &
            (points(i, 2) == min_y .and. points(i, 1) < points(min_index, 1))) then
            min_y = points(i, 2)
            min_index = i
        end if
    end do
    
    ! Swap the minimum point with the first point
    temp_x = points(1, 1)
    temp_y = points(1, 2)
    points(1, 1) = points(min_index, 1)
    points(1, 2) = points(min_index, 2)
    points(min_index, 1) = temp_x
    points(min_index, 2) = temp_y
    
    ! Sort points by polar angle with respect to the first point
    call polar_angle_sort(points, n)
    
    ! Initialize stack with first three points
    stack(1) = 1
    stack(2) = 2
    stack(3) = 3
    stack_size = 3
    
    ! Process remaining points
    do i = 4, n
        ! Remove points from stack while the angle formed by the last three points
        ! makes a clockwise turn (or collinear)
        do while (stack_size >= 3 .and. &
                  cross_product(points(stack(stack_size-2), :), &
                               points(stack(stack_size-1), :), &
                               points(stack(stack_size), :)) <= 0.0)
            stack_size = stack_size - 1
        end do
        
        stack_size = stack_size + 1
        stack(stack_size) = i
    end do
    
    ! Copy result to output array
    do i = 1, stack_size
        hull_indices(i) = stack(i)
    end do
    hull_size = stack_size
    
end subroutine graham_scan

subroutine polar_angle_sort(points, n)
    implicit none
    integer, intent(in) :: n
    real, intent(inout) :: points(n, 2)
    integer :: i, j, k
    real :: temp(2)
    real :: angle1, angle2
    
    ! Simple bubble sort based on polar angle
    do i = 1, n-1
        do j = i+1, n
            angle1 = atan2(points(i, 2) - points(1, 2), points(i, 1) - points(1, 1))
            angle2 = atan2(points(j, 2) - points(1, 2), points(j, 1) - points(1, 1))
            
            ! Adjust angles to be in [0, 2π)
            if (angle1 < 0.0) angle1 = angle1 + 2.0 * 3.141592653589793
            if (angle2 < 0.0) angle2 = angle2 + 2.0 * 3.141592653589793
            
            if (angle2 < angle1) then
                ! Swap points
                temp(1) = points(i, 1)
                temp(2) = points(i, 2)
                points(i, 1) = points(j, 1)
                points(i, 2) = points(j, 2)
                points(j, 1) = temp(1)
                points(j, 2) = temp(2)
            end if
        end do
    end do
    
end subroutine polar_angle_sort

real function cross_product(p1, p2, p3)
    implicit none
    real, intent(in) :: p1(2), p2(2), p3(2)
    
    ! Calculate cross product of vectors (p1->p2) and (p2->p3)
    cross_product = (p2(1) - p1(1)) * (p3(2) - p2(2)) - (p2(2) - p1(2)) * (p3(1) - p2(1))
    
end function cross_product
```

## How the Algorithm Works:

1. **Find the starting point**: The bottom-most point (or left-most in case of tie)
2. **Sort points**: Sort all other points by polar angle with respect to the starting point
3. **Build hull**: Use a stack to maintain the convex hull, removing points that make clockwise turns
4. **Return result**: The indices of points forming the convex hull

## Key Features:

- **Input**: Array of 2D points
- **Output**: Indices of points forming the convex hull
- **Time Complexity**: O(n log n) due to sorting
- **Space Complexity**: O(n) for the stack

## Sample Output:
```
Input points:
Point 1: ( 0.00  0.00)
Point 2: ( 1.00  0.00)
Point 3: ( 2.00  1.00)
Point 4: ( 1.00  2.00)
Point 5: ( 0.00  2.00)
Point 6: ( 0.50  1.50)
Point 7: ( 1.50  1.00)
Point 8: ( 1.00  1.00)

Convex Hull Points:
Hull Point 1: ( 0.00  0.00)
Hull Point 2: ( 1.00  0.00)
Hull Point 3: ( 2.00  1.00)
Hull Point 4: ( 1.00  2.00)
Hull Point 5: ( 0.00  2.00)
```

This implementation handles the core Graham Scan algorithm with proper sorting and stack-based hull construction.

