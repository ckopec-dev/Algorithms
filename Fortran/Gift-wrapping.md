# Gift Wrapping Algorithm in Fortran

Here's an implementation of the Gift Wrapping (Jarvis March) algorithm for finding the convex hull of a set of points in Fortran:

```fortran
program gift_wrapping
    implicit none
    integer, parameter :: nmax = 100
    integer :: n, i, j, k, l, next_point
    real :: x(nmax), y(nmax)
    integer :: hull(nmax)
    logical :: on_hull(nmax)
    real :: cross_product, min_angle, angle
    
    ! Sample points
    n = 6
    x(1) = 0.0; y(1) = 0.0
    x(2) = 1.0; y(2) = 1.0
    x(3) = 2.0; y(3) = 0.0
    x(4) = 1.0; y(4) = 2.0
    x(5) = 3.0; y(5) = 1.0
    x(6) = 2.0; y(6) = 2.0
    
    ! Initialize arrays
    on_hull = .false.
    hull = 0
    
    ! Find the leftmost point (minimum x-coordinate)
    l = 1
    do i = 2, n
        if (x(i) < x(l)) then
            l = i
        end if
    end do
    
    ! Start with the leftmost point
    k = l
    i = 1
    
    do
        ! Add current point to hull
        hull(i) = k
        i = i + 1
        
        ! Find the next point on hull
        next_point = 1
        min_angle = 1000.0
        
        do j = 1, n
            if (j == k) cycle
            
            ! Calculate cross product to determine orientation
            cross_product = (x(j) - x(k)) * (y(next_point) - y(k)) - &
                           (y(j) - y(k)) * (x(next_point) - x(k))
            
            ! If cross product is positive, point j is to the left
            ! If cross product is negative, point j is to the right
            ! If cross product is zero, points are collinear
            
            if (cross_product > 0.0) then
                next_point = j
                min_angle = cross_product
            else if (cross_product == 0.0) then
                ! For collinear points, choose the one that makes the smallest angle
                angle = atan2(y(j) - y(k), x(j) - x(k))
                if (angle < min_angle) then
                    next_point = j
                    min_angle = angle
                end if
            end if
        end do
        
        k = next_point
        
        ! Stop when we return to the starting point
        if (k == l) then
            exit
        end if
    end do
    
    ! Print the convex hull points
    write(*,*) 'Convex Hull Points:'
    do i = 1, n
        if (hull(i) == 0) exit
        write(*,*) 'Point ', hull(i), ': (', x(hull(i)), ',', y(hull(i)), ')'
    end do
    
end program gift_wrapping
```

## Key Components of the Algorithm:

1. **Finding the Starting Point**: The leftmost point is selected as the starting point
2. **Angle Calculation**: Uses cross product to determine the orientation of three points
3. **Point Selection**: Iteratively selects the next point that makes the smallest angle
4. **Termination**: Stops when returning to the starting point

## How it Works:

1. Start with the leftmost point
2. For each point on the hull, find the next point by checking all other points
3. Use cross product to determine which point makes the smallest angle
4. Continue until returning to the starting point

## Time Complexity:
- **O(nh)** where n is the number of input points and h is the number of points on the hull
- In worst case: O(n²) when all points are on the hull

## Space Complexity:
- O(n) for storing the points and hull indices

This implementation demonstrates the core logic of the gift wrapping algorithm for computing convex hulls in Fortran.

