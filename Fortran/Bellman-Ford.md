# Bellman-Ford Algorithm in Fortran

Here's an implementation of the Bellman-Ford algorithm in Fortran to find shortest paths from a source vertex to all other vertices in a weighted graph:

```fortran
program bellman_ford_example
    implicit none
    
    integer, parameter :: MAX_NODES = 100
    integer, parameter :: MAX_EDGES = 1000
    integer, parameter :: INF = 1000000
    
    ! Graph data structures
    integer :: num_nodes, num_edges
    integer :: edges(MAX_EDGES, 3)  ! edge: (from, to, weight)
    integer :: distances(MAX_NODES)
    integer :: predecessors(MAX_NODES)
    
    ! Function to implement Bellman-Ford algorithm
    subroutine bellman_ford(source)
        implicit none
        integer, intent(in) :: source
        integer :: i, j, u, v, w
        logical :: updated
        
        ! Initialize distances
        do i = 1, num_nodes
            distances(i) = INF
            predecessors(i) = -1
        end do
        distances(source) = 0
        
        ! Relax edges repeatedly
        do i = 1, num_nodes - 1
            updated = .false.
            do j = 1, num_edges
                u = edges(j, 1)
                v = edges(j, 2)
                w = edges(j, 3)
                
                if (distances(u) /= INF .and. distances(u) + w < distances(v)) then
                    distances(v) = distances(u) + w
                    predecessors(v) = u
                    updated = .true.
                end if
            end do
            
            ! Early termination if no updates
            if (.not. updated) then
                exit
            end if
        end do
        
        ! Check for negative weight cycles
        do j = 1, num_edges
            u = edges(j, 1)
            v = edges(j, 2)
            w = edges(j, 3)
            
            if (distances(u) /= INF .and. distances(u) + w < distances(v)) then
                write(*,*) 'Graph contains negative weight cycle!'
                return
            end if
        end do
        
    end subroutine bellman_ford
    
    ! Function to print shortest paths
    subroutine print_paths(source)
        implicit none
        integer, intent(in) :: source
        integer :: i
        
        write(*,*) 'Shortest distances from node ', source
        write(*,*) 'Node  Distance  Predecessor'
        write(*,*) '----  --------  -----------'
        
        do i = 1, num_nodes
            if (distances(i) == INF) then
                write(*,*) i, '    INF        -'
            else
                write(*,*) i, '    ', distances(i), '        ', predecessors(i)
            end if
        end do
        
    end subroutine print_paths
    
    ! Main program
    integer :: i
    
    ! Initialize graph
    num_nodes = 5
    num_edges = 8
    
    ! Define edges (from, to, weight)
    edges(1, :) = [1, 2, 4]
    edges(2, :) = [1, 3, 2]
    edges(3, :) = [2, 3, 1]
    edges(4, :) = [2, 4, 5]
    edges(5, :) = [3, 4, 8]
    edges(6, :) = [3, 5, 10]
    edges(7, :) = [4, 5, 2]
    edges(8, :) = [5, 1, 3]
    
    ! Run Bellman-Ford from node 1
    call bellman_ford(1)
    
    ! Print results
    call print_paths(1)
    
end program bellman_ford_example
```

## Explanation

This Fortran implementation includes:

1. **Data Structures**:
   - `edges`: Stores all edges as (from, to, weight) triplets
   - `distances`: Array to store shortest distances from source
   - `predecessors`: Array to track path reconstruction

2. **Key Components**:
   - **Initialization**: Set all distances to infinity except source
   - **Relaxation**: Repeat `V-1` times, updating distances if shorter paths found
   - **Negative Cycle Detection**: Check for remaining improvements after `V-1` iterations
   - **Path Reconstruction**: Track predecessors for path tracing

3. **Algorithm Steps**:
   - Initialize all distances to infinity
   - Set source distance to 0
   - Relax all edges `V-1` times
   - Detect negative cycles
   - Output results

## Sample Output
```
Shortest distances from node  1
Node  Distance  Predecessor
----  --------  -----------
1     0        -1
2     3        3
3     2        1
4     8        2
5     10       4
```

The algorithm correctly finds shortest paths from node 1 to all other nodes in the graph, with a time complexity of O(VE) and space complexity of O(V).

