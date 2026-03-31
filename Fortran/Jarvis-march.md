# Jarvis March Algorithm in Fortran

Here's an implementation of the Jarvis March (Gift Wrapping) algorithm for computing the convex hull of a set of points in Fortran:

```fortran
program jarvis_march
    implicit none
    integer, parameter :: n = 10
    integer :: i, j, k, start, next_point
    real :: points(n, 2)
    integer :: hull(n)
    integer :: hull_count
    real :: cross_product
    
    ! Sample points
    points(1, :) = [0.0, 0.0]
    points(2, :) = [1.0, 0.0]
    points(3, :) = [2.0, 1.0]
    points(4, :) = [2.0, 2.0]
    points(5, :) = [1.0, 3.0]
    points(6, :) = [0.0, 2.0]
    points(7, :) = [0.5, 1.5]
    points(8, :) = [1.5, 1.0]
    points(9, :) = [1.0, 1.0]
    points(10, :) = [0.0, 1.0]
    
    ! Initialize hull array
    do i = 1, n
        hull(i) = 0
    end do
    
    ! Find the leftmost point (starting point)
    start = 1
    do i = 2, n
        if (points(i, 1) < points(start, 1)) then
            start = i
        end if
    end do
    
    ! Start building the hull
    k = 1
    hull(k) = start
    next_point = start
    
    do
        ! Find the next point on the hull
        do i = 1, n
            ! Skip if it's the current point
            if (i == next_point) cycle
            
            ! Calculate cross product to determine orientation
            cross_product = (points(i, 1) - points(next_point, 1)) * &
                           (points(hull(k), 2) - points(next_point, 2)) - &
                           (points(i, 2) - points(next_point, 2)) * &
                           (points(hull(k), 1) - points(next_point, 1))
            
            ! If this point is more counterclockwise than current, update
            if (k == 1 .or. cross_product > 0.0) then
                next_point = i
            end if
        end do
        
        ! If we've wrapped around to the starting point, we're done
        if (next_point == hull(1)) then
            exit
        end if
        
        ! Add the next point to hull
        k = k + 1
        hull(k) = next_point
    end do
    
    hull_count = k
    
    ! Print the convex hull points
    write(*,*) 'Convex Hull Points:'
    do i = 1, hull_count
        write(*,*) 'Point ', i, ': (', points(hull(i), 1), ',', points(hull(i), 2), ')'
    end do
    
end program jarvis_march
```

## Algorithm Explanation

The Jarvis March algorithm works as follows:

1. **Find Starting Point**: Identify the leftmost point (or bottom-most if ties exist)
2. **Build Hull**: Starting from the leftmost point, find the next point by checking all other points
3. **Orientation Test**: Use cross product to determine which point is more counterclockwise
4. **Continue**: Repeat until we return to the starting point

## Key Components

- **Cross Product**: Used to determine the orientation of three points
- **Point Structure**: 2D coordinates stored in a 2D array
- **Hull Tracking**: Array to store indices of points forming the hull

## Time Complexity
- **Best Case**: O(nh) where h is the number of points on the hull
- **Worst Case**: O(n²) when all points are on the hull

## Space Complexity
- O(n) for storing the hull points

This implementation demonstrates the classic gift wrapping approach for computing convex hulls in Fortran.

