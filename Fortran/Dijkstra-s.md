# Dijkstra's Algorithm in Fortran

Here's an implementation of Dijkstra's algorithm in Fortran to find the shortest paths from a source vertex to all other vertices in a weighted graph:

```fortran
program dijkstra_example
    implicit none
    integer, parameter :: n = 6, inf = 1000000
    integer :: graph(n, n)
    integer :: dist(n)
    logical :: visited(n)
    integer :: source, i, j, min_dist, min_vertex
    integer :: current
    
    ! Initialize the graph with edge weights
    ! Using a 6x6 adjacency matrix
    do i = 1, n
        do j = 1, n
            if (i == j) then
                graph(i, j) = 0
            else
                graph(i, j) = inf
            end if
        end do
    end do
    
    ! Define edges with weights
    graph(1, 2) = 4
    graph(1, 3) = 2
    graph(2, 3) = 1
    graph(2, 4) = 5
    graph(3, 4) = 8
    graph(3, 5) = 10
    graph(4, 5) = 2
    graph(4, 6) = 6
    graph(5, 6) = 3
    
    ! Initialize distances and visited array
    source = 1
    do i = 1, n
        dist(i) = inf
        visited(i) = .false.
    end do
    
    ! Distance to source is 0
    dist(source) = 0
    
    ! Main Dijkstra algorithm
    do i = 1, n
        ! Find vertex with minimum distance that hasn't been visited
        min_dist = inf
        min_vertex = 0
        
        do j = 1, n
            if (.not. visited(j) .and. dist(j) < min_dist) then
                min_dist = dist(j)
                min_vertex = j
            end if
        end do
        
        ! If no reachable vertex found, break
        if (min_vertex == 0) then
            exit
        end if
        
        ! Mark vertex as visited
        visited(min_vertex) = .true.
        
        ! Update distances of adjacent vertices
        do j = 1, n
            if (.not. visited(j) .and. graph(min_vertex, j) /= inf) then
                current = dist(min_vertex) + graph(min_vertex, j)
                if (current < dist(j)) then
                    dist(j) = current
                end if
            end if
        end do
    end do
    
    ! Print results
    write(*,*) 'Shortest distances from vertex ', source
    write(*,*) 'Vertex  Distance'
    write(*,*) '------  --------'
    do i = 1, n
        if (dist(i) == inf) then
            write(*,*) i, '       INF'
        else
            write(*,*) i, '       ', dist(i)
        end if
    end do
    
end program dijkstra_example
```

## Explanation of the Code

### Key Components:

1. **Graph Representation**: Uses an adjacency matrix `graph(n,n)` where `graph(i,j)` represents the weight of edge from vertex `i` to vertex `j`

2. **Distance Array**: `dist(n)` stores the shortest distance from source to each vertex

3. **Visited Array**: `visited(n)` tracks which vertices have been processed

4. **Algorithm Steps**:
   - Initialize all distances to infinity except source (0)
   - Find vertex with minimum distance that hasn't been visited
   - Mark it as visited
   - Update distances of adjacent vertices
   - Repeat until all vertices are processed

### Sample Output:
```
Shortest distances from vertex  1
Vertex  Distance
------  --------
1        0
2        4
3        2
4        9
5        11
6        14
```

This implementation finds the shortest paths from vertex 1 to all other vertices in the graph, with the distances being: 1→2:4, 1→3:2, 1→4:9, 1→5:11, 1→6:14.

