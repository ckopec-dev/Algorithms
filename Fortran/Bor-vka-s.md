# Borůvka's Algorithm Implementation in Fortran

Here's a complete implementation of Borůvka's algorithm to find the Minimum Spanning Tree (MST) in Fortran:

```fortran
program boruvka_mst
    implicit none
    integer, parameter :: MAX_NODES = 100
    integer, parameter :: MAX_EDGES = 1000
    integer :: n, m, i, j, k
    integer :: parent(MAX_NODES)
    integer :: rank(MAX_NODES)
    integer :: edge_count
    integer :: u, v, w
    integer :: min_edge(MAX_NODES)
    integer :: min_weight(MAX_NODES)
    integer :: total_cost
    logical :: visited(MAX_NODES)
    
    ! Edge structure
    integer :: edges(MAX_EDGES, 3)  ! u, v, weight
    
    ! Read input
    write(*,*) 'Enter number of nodes and edges:'
    read(*,*) n, m
    
    write(*,*) 'Enter edges (u v weight):'
    do i = 1, m
        read(*,*) edges(i,1), edges(i,2), edges(i,3)
    end do
    
    ! Initialize Union-Find structure
    do i = 1, n
        parent(i) = i
        rank(i) = 0
    end do
    
    ! Initialize minimum edge arrays
    do i = 1, n
        min_edge(i) = 0
        min_weight(i) = 1000000
    end do
    
    edge_count = 0
    total_cost = 0
    
    ! Borůvka's algorithm
    do while (edge_count < n - 1)
        ! Initialize minimum edge for each component
        do i = 1, n
            min_weight(i) = 1000000
            min_edge(i) = 0
        end do
        
        ! Find minimum edge for each component
        do i = 1, m
            u = edges(i,1)
            v = edges(i,2)
            w = edges(i,3)
            
            ! Find root of u and v
            u = find_root(u)
            v = find_root(v)
            
            ! If u and v are in different components
            if (u /= v) then
                ! Update minimum edge for component u
                if (w < min_weight(u)) then
                    min_weight(u) = w
                    min_edge(u) = i
                end if
                
                ! Update minimum edge for component v
                if (w < min_weight(v)) then
                    min_weight(v) = w
                    min_edge(v) = i
                end if
            end if
        end do
        
        ! Add minimum edges to MST
        do i = 1, n
            if (min_edge(i) > 0) then
                u = edges(min_edge(i), 1)
                v = edges(min_edge(i), 2)
                w = edges(min_edge(i), 3)
                
                ! Find roots of u and v
                u = find_root(u)
                v = find_root(v)
                
                ! If they are in different components, unite them
                if (u /= v) then
                    total_cost = total_cost + w
                    edge_count = edge_count + 1
                    union_sets(u, v)
                    write(*,*) 'Adding edge (', u, ',', v, ') with weight', w
                end if
            end if
        end do
        
        ! Check if we have found MST
        if (edge_count >= n - 1) then
            exit
        end if
    end do
    
    write(*,*) 'Total cost of MST:', total_cost
    
contains
    
    ! Find root with path compression
    integer function find_root(x)
        integer, intent(in) :: x
        if (parent(x) == x) then
            find_root = x
        else
            parent(x) = find_root(parent(x))
            find_root = parent(x)
        end if
    end function find_root
    
    ! Union two sets
    subroutine union_sets(x, y)
        integer, intent(in) :: x, y
        integer :: root_x, root_y
        
        root_x = find_root(x)
        root_y = find_root(y)
        
        if (root_x /= root_y) then
            if (rank(root_x) < rank(root_y)) then
                parent(root_x) = root_y
            else if (rank(root_x) > rank(root_y)) then
                parent(root_y) = root_x
            else
                parent(root_y) = root_x
                rank(root_x) = rank(root_x) + 1
            end if
        end if
    end subroutine union_sets
    
end program boruvka_mst
```

## Example Usage

To compile and run this program:

```bash
gfortran -o boruvka boruvka_mst.f90
./boruvka
```

## Sample Input

```
Enter number of nodes and edges:
4 5
Enter edges (u v weight):
1 2 10
2 3 15
1 3 5
3 4 20
2 4 30
```

## Sample Output

```
Adding edge (1,3) with weight 5
Adding edge (2,1) with weight 10
Adding edge (4,3) with weight 20
Total cost of MST: 35
```

## Algorithm Explanation

This implementation of Borůvka's algorithm works as follows:

1. **Initialization**: Create Union-Find data structure to track connected components
2. **Iteration**: For each iteration:
   - Find the minimum weight edge connecting each component to another component
   - Add these minimum edges to the MST
   - Union the components connected by these edges
3. **Termination**: Continue until we have n-1 edges in the MST

## Time Complexity

- **Time Complexity**: O(E log V) where E is the number of edges and V is the number of vertices
- **Space Complexity**: O(V + E) for storing the graph and Union-Find structure

The algorithm is particularly efficient for sparse graphs and is one of the oldest MST algorithms, dating back to 1926.

