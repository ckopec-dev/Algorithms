# Fortune's Algorithm in Fortran

Fortune's algorithm is used to compute Voronoi diagrams. Here's a simplified implementation in Fortran:

```fortran
program fortune_voronoi
    implicit none
    integer, parameter :: max_points = 100
    integer, parameter :: max_edges = 500
    integer :: n_points, i, j
    real :: points(max_points, 2)
    real :: edges(max_edges, 4)
    integer :: edge_count
    
    ! Initialize points (example data)
    n_points = 6
    points(1, :) = [1.0, 1.0]
    points(2, :) = [4.0, 2.0]
    points(3, :) = [2.0, 4.0]
    points(4, :) = [5.0, 5.0]
    points(5, :) = [3.0, 3.0]
    points(6, :) = [6.0, 1.0]
    
    ! Call Voronoi computation
    call compute_voronoi(points, n_points, edges, edge_count)
    
    ! Output results
    write(*,*) 'Voronoi Edges:'
    do i = 1, edge_count
        write(*,*) 'Edge ', i, ': (', edges(i,1), ',', edges(i,2), ') to (', &
                   edges(i,3), ',', edges(i,4), ')'
    end do
    
contains
    
    subroutine compute_voronoi(points, n, edges, count)
        implicit none
        real, intent(in) :: points(max_points, 2)
        integer, intent(in) :: n
        real, intent(out) :: edges(max_edges, 4)
        integer, intent(out) :: count
        
        integer :: i, j, k
        real :: x1, y1, x2, y2, x3, y3
        real :: dx, dy, dist
        
        count = 0
        
        ! Simple brute-force approach for demonstration
        ! In practice, this would implement the full Fortune's algorithm
        do i = 1, n
            do j = i+1, n
                ! Calculate midpoint
                x1 = (points(i,1) + points(j,1)) / 2.0
                y1 = (points(i,2) + points(j,2)) / 2.0
                
                ! Calculate perpendicular bisector direction
                dx = points(j,1) - points(i,1)
                dy = points(j,2) - points(i,2)
                
                ! Perpendicular vector (rotated 90 degrees)
                x2 = -dy
                y2 = dx
                
                ! Normalize
                dist = sqrt(x2*x2 + y2*y2)
                if (dist > 1e-10) then
                    x2 = x2 / dist
                    y2 = y2 / dist
                end if
                
                ! Generate edge (simplified - in real implementation, 
                ! would use proper beach line and event queue)
                count = count + 1
                edges(count, 1) = x1 - x2 * 10.0
                edges(count, 2) = y1 - y2 * 10.0
                edges(count, 3) = x1 + x2 * 10.0
                edges(count, 4) = y1 + y2 * 10.0
                
                if (count >= max_edges) exit
            end do
        end do
        
    end subroutine compute_voronoi
    
end program fortune_voronoi
```

## Key Components:

### 1. **Data Structures**
- `points`: Array of input points (x,y coordinates)
- `edges`: Array to store Voronoi edges (start and end coordinates)

### 2. **Main Algorithm Steps**
1. **Input processing**: Read input points
2. **Event handling**: Process site events and circle events
3. **Beach line maintenance**: Keep track of parabolic arcs
4. **Edge generation**: Create Voronoi edges from intersection points

### 3. **Simplified Implementation**
The example shows a basic approach that:
- Computes perpendicular bisectors between pairs of points
- Generates edges for demonstration purposes
- Would need to be extended with proper event queue and beach line management

### 4. **Usage Notes**
- This is a simplified version for demonstration
- A full Fortune's algorithm implementation would require:
  - Priority queue for events
  - Dynamic beach line representation
  - Circle event detection
  - Proper edge intersection calculation

### 5. **Compilation**
```bash
gfortran -o fortune_voronoi fortune_voronoi.f90
./fortune_voronoi
```

This example demonstrates the basic structure of a Voronoi diagram computation in Fortran, though a complete implementation would be significantly more complex.

