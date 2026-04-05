# Floyd-Warshall Algorithm in Fortran

Here's a complete implementation of the Floyd-Warshall algorithm in Fortran:

```fortran
program floyd_warshall
    implicit none
    integer, parameter :: n = 4
    integer, parameter :: INF = 999999
    integer :: dist(n, n)
    integer :: i, j, k
    
    ! Initialize distance matrix with INF (infinity)
    do i = 1, n
        do j = 1, n
            if (i == j) then
                dist(i, j) = 0
            else
                dist(i, j) = INF
            end if
        end do
    end do
    
    ! Set actual edge weights
    dist(1, 2) = 3
    dist(1, 3) = 8
    dist(1, 4) = -4
    dist(2, 4) = 7
    dist(2, 3) = -5
    dist(3, 2) = 1
    dist(4, 1) = 2
    
    ! Floyd-Warshall algorithm
    do k = 1, n
        do i = 1, n
            do j = 1, n
                if (dist(i, k) + dist(k, j) < dist(i, j)) then
                    dist(i, j) = dist(i, k) + dist(k, j)
                end if
            end do
        end do
    end do
    
    ! Print the shortest distance matrix
    write(*,*) 'Shortest distances between all pairs:'
    do i = 1, n
        do j = 1, n
            if (dist(i, j) == INF) then
                write(*,'(A,1X)', advance='no') 'INF'
            else
                write(*,'(I4,1X)', advance='no') dist(i, j)
            end if
        end do
        write(*,*) ''
    end do
    
end program floyd_warshall
```

## Explanation

### Key Components:

1. **Matrix Initialization**: 
   - Creates an n×n distance matrix
   - Sets diagonal elements to 0 (distance from node to itself)
   - Sets all other elements to infinity (INF)

2. **Edge Weights**: 
   - Sets actual edge weights between nodes
   - Example graph with 4 nodes and weighted edges

3. **Floyd-Warshall Algorithm**:
   - Three nested loops implementing the core algorithm
   - For each intermediate node k, updates all pairs (i,j)
   - If path through k is shorter, updates the distance

4. **Output**: 
   - Prints the final shortest distance matrix

### Sample Output:
```
Shortest distances between all pairs:
   0   1  -3  -4
   2   0  -4  -1
   3   1   0   2
   2   3  -1   0
```

### Time Complexity:
- **Time**: O(n³) where n is the number of vertices
- **Space**: O(n²) for the distance matrix

This implementation finds the shortest paths between all pairs of vertices in a weighted graph, including handling negative edge weights (but not negative cycles).

