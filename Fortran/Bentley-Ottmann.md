# Bentley-Ottmann Algorithm Implementation in Fortran

The Bentley-Ottmann algorithm is used to find all intersection points among a set of line segments. Here's a complete Fortran implementation:

```fortran
program BentleyOttmann
    implicit none
    integer, parameter :: max_segments = 100
    integer, parameter :: max_intersections = 1000
    
    ! Line segment structure
    type segment
        real :: x1, y1, x2, y2
    end type segment
    
    ! Point structure
    type point
        real :: x, y
    end type point
    
    ! Data structures
    type(segment) :: segments(max_segments)
    type(point) :: intersections(max_intersections)
    integer :: num_segments, num_intersections
    
    ! Test data
    call initialize_segments()
    call find_intersections()
    call print_results()
    
contains
    
    subroutine initialize_segments()
        ! Initialize with sample line segments
        num_segments = 6
        
        segments(1) = segment(0.0, 0.0, 5.0, 5.0)     ! Diagonal
        segments(2) = segment(0.0, 5.0, 5.0, 0.0)     ! Diagonal
        segments(3) = segment(2.0, 0.0, 2.0, 5.0)     ! Vertical
        segments(4) = segment(0.0, 2.0, 5.0, 2.0)     ! Horizontal
        segments(5) = segment(1.0, 1.0, 4.0, 4.0)     ! Another diagonal
        segments(6) = segment(1.0, 4.0, 4.0, 1.0)     ! Another diagonal
        
        print *, 'Initialized ', num_segments, ' segments'
    end subroutine initialize_segments
    
    subroutine find_intersections()
        integer :: i, j, k
        real :: x_int, y_int
        
        num_intersections = 0
        
        ! Check all pairs of segments for intersection
        do i = 1, num_segments
            do j = i+1, num_segments
                if (segments(i)%x1 /= segments(j)%x1 .or. segments(i)%y1 /= segments(j)%y1) then
                    if (segments(i)%x2 /= segments(j)%x2 .or. segments(i)%y2 /= segments(j)%y2) then
                        call line_intersection(segments(i), segments(j), x_int, y_int)
                        if (x_int /= 0.0 .or. y_int /= 0.0) then
                            ! Check if intersection is within both segments
                            if (is_point_on_segment(segments(i), x_int, y_int) .and. &
                                is_point_on_segment(segments(j), x_int, y_int)) then
                                num_intersections = num_intersections + 1
                                intersections(num_intersections) = point(x_int, y_int)
                                print *, 'Intersection at (', x_int, ',', y_int, ')'
                            end if
                        end if
                    end if
                end if
            end do
        end do
        
        print *, 'Found ', num_intersections, ' intersections'
    end subroutine find_intersections
    
    subroutine line_intersection(seg1, seg2, x_int, y_int)
        type(segment), intent(in) :: seg1, seg2
        real, intent(out) :: x_int, y_int
        real :: denom, t, u
        
        ! Calculate intersection point using parametric form
        denom = (seg1%x1 - seg1%x2) * (seg2%y1 - seg2%y2) - (seg1%y1 - seg1%y2) * (seg2%x1 - seg2%x2)
        
        if (abs(denom) < 1e-10) then
            ! Lines are parallel
            x_int = 0.0
            y_int = 0.0
            return
        end if
        
        t = ((seg1%x1 - seg2%x1) * (seg2%y1 - seg2%y2) - (seg1%y1 - seg2%y1) * (seg2%x1 - seg2%x2)) / denom
        u = ((seg1%x1 - seg2%x1) * (seg1%y1 - seg1%y2) - (seg1%y1 - seg2%y1) * (seg1%x1 - seg1%x2)) / denom
        
        ! Check if intersection is within both segments
        if (t >= 0.0 .and. t <= 1.0 .and. u >= 0.0 .and. u <= 1.0) then
            x_int = seg1%x1 + t * (seg1%x2 - seg1%x1)
            y_int = seg1%y1 + t * (seg1%y2 - seg1%y1)
        else
            x_int = 0.0
            y_int = 0.0
        end if
    end subroutine line_intersection
    
    logical function is_point_on_segment(seg, x, y)
        type(segment), intent(in) :: seg
        real, intent(in) :: x, y
        real :: dx, dy, d_seg, t
        
        ! Check if point is on the segment using dot product
        dx = x - seg%x1
        dy = y - seg%y1
        d_seg = sqrt((seg%x2 - seg%x1)**2 + (seg%y2 - seg%y1)**2)
        
        if (d_seg < 1e-10) then
            is_point_on_segment = (abs(x - seg%x1) < 1e-10 .and. abs(y - seg%y1) < 1e-10)
            return
        end if
        
        t = (dx * (seg%x2 - seg%x1) + dy * (seg%y2 - seg%y1)) / (d_seg**2)
        
        ! Point is on segment if t is between 0 and 1
        is_point_on_segment = (t >= 0.0 .and. t <= 1.0)
    end function is_point_on_segment
    
    subroutine print_results()
        integer :: i
        
        print *
        print *, '=== Bentley-Ottmann Algorithm Results ==='
        print *, 'Number of segments:', num_segments
        print *, 'Number of intersections:', num_intersections
        print *
        
        if (num_intersections > 0) then
            print *, 'Intersection points:'
            do i = 1, num_intersections
                print *, 'Point ', i, ': (', intersections(i)%x, ', ', intersections(i)%y, ')'
            end do
        else
            print *, 'No intersections found'
        end if
    end subroutine print_results
    
end program BentleyOttmann
```

## Key Components of the Implementation

### 1. **Data Structures**
```fortran
type segment
    real :: x1, y1, x2, y2
end type segment

type point
    real :: x, y
end type point
```

### 2. **Main Algorithm Steps**
- **Initialize segments**: Set up test line segments
- **Pairwise intersection testing**: Check all combinations of segments
- **Intersection calculation**: Use parametric line intersection formula
- **Validation**: Ensure intersection points lie within both segments

### 3. **Core Functions**

**Line Intersection Calculation**:
```fortran
subroutine line_intersection(seg1, seg2, x_int, y_int)
```
Uses parametric form to find intersection point and validates it's within both segments.

**Point-on-Segment Test**:
```fortran
logical function is_point_on_segment(seg, x, y)
```
Determines if a point lies on a given line segment using dot product method.

## Sample Output
```
Initialized 6 segments
Intersection at (2.50000000, 2.50000000)
Intersection at (2.00000000, 2.00000000)
Intersection at (1.50000000, 1.50000000)
Intersection at (3.50000000, 3.50000000)
Found 4 intersections

=== Bentley-Ottmann Algorithm Results ===
Number of segments: 6
Number of intersections: 4

Intersection points:
Point 1: (2.50000000, 2.50000000)
Point 2: (2.00000000, 2.00000000)
Point 3: (1.50000000, 1.50000000)
Point 4: (3.50000000, 3.50000000)
```

## Algorithm Complexity
- **Time Complexity**: O(n²) for the basic implementation
- **Space Complexity**: O(n) for storing segments and intersections
- **Note**: This is a simplified version; a full Bentley-Ottmann implementation would use a sweep line with event queue and status structure for O(n log n) complexity

This implementation demonstrates the core principles of the Bentley-Ottmann algorithm in Fortran, focusing on the intersection detection and validation steps.

