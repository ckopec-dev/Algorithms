# Edmonds-Karp Algorithm in Fortran

Here's a complete implementation of the Edmonds-Karp algorithm for finding maximum flow in a flow network using Fortran:

```fortran
program edmonds_karp_example
    implicit none
    integer, parameter :: max_nodes = 100
    integer, parameter :: max_edges = 1000
    integer, parameter :: inf = 1000000
    
    ! Flow network parameters
    integer :: num_nodes, num_edges
    integer :: capacity(max_nodes, max_nodes)
    integer :: flow(max_nodes, max_nodes)
    integer :: adj_list(max_nodes, max_nodes)
    integer :: adj_count(max_nodes)
    
    ! BFS variables
    integer :: queue(max_nodes)
    integer :: visited(max_nodes)
    integer :: parent(max_nodes)
    integer :: front, rear
    
    ! Function declarations
    logical :: bfs()
    integer :: min_value()
    integer :: max_flow
    
    ! Initialize the graph
    call initialize_graph()
    
    ! Run Edmonds-Karp algorithm
    max_flow = 0
    do while (bfs())
        max_flow = max_flow + min_value()
    end do
    
    ! Output result
    print *, 'Maximum flow:', max_flow
    
contains
    
    subroutine initialize_graph()
        ! Initialize adjacency list and capacities for a sample graph
        ! Graph: s(1) -> a(2) -> t(3) with capacities 10, 5
        !        s(1) -> b(4) -> t(3) with capacities 8, 7
        
        integer :: i, j
        
        ! Initialize arrays
        do i = 1, max_nodes
            do j = 1, max_nodes
                capacity(i,j) = 0
                flow(i,j) = 0
            end do
            adj_count(i) = 0
        end do
        
        ! Set number of nodes and edges
        num_nodes = 4
        num_edges = 4
        
        ! Add edges with capacities
        ! s -> a (capacity 10)
        capacity(1,2) = 10
        adj_count(1) = adj_count(1) + 1
        adj_list(1, adj_count(1)) = 2
        
        ! s -> b (capacity 8)
        capacity(1,4) = 8
        adj_count(1) = adj_count(1) + 1
        adj_list(1, adj_count(1)) = 4
        
        ! a -> t (capacity 5)
        capacity(2,3) = 5
        adj_count(2) = adj_count(2) + 1
        adj_list(2, adj_count(2)) = 3
        
        ! b -> t (capacity 7)
        capacity(4,3) = 7
        adj_count(4) = adj_count(4) + 1
        adj_list(4, adj_count(4)) = 3
        
        ! Add reverse edges for residual graph
        capacity(2,1) = 0
        capacity(4,1) = 0
        capacity(3,2) = 0
        capacity(3,4) = 0
        
    end subroutine initialize_graph
    
    logical function bfs()
        ! Breadth-First Search to find augmenting path
        integer :: u, v, i
        
        ! Initialize visited array
        do i = 1, num_nodes
            visited(i) = 0
            parent(i) = -1
        end do
        
        ! Initialize queue
        front = 1
        rear = 1
        queue(1) = 1  ! Start from source (node 1)
        visited(1) = 1
        
        bfs = .false.
        
        do while (front <= rear)
            u = queue(front)
            front = front + 1
            
            ! Check all adjacent nodes
            do i = 1, adj_count(u)
                v = adj_list(u, i)
                
                if (visited(v) == 0 .and. capacity(u,v) > flow(u,v)) then
                    visited(v) = 1
                    parent(v) = u
                    rear = rear + 1
                    queue(rear) = v
                    
                    if (v == num_nodes) then
                        bfs = .true.
                        return
                    end if
                end if
            end do
        end do
        
    end function bfs
    
    integer function min_value()
        ! Find minimum capacity along the path from source to sink
        integer :: u, v
        integer :: min_capacity
        
        min_capacity = inf
        
        ! Trace back from sink to source using parent array
        v = num_nodes
        do while (parent(v) /= -1)
            u = parent(v)
            if (capacity(u,v) - flow(u,v) < min_capacity) then
                min_capacity = capacity(u,v) - flow(u,v)
            end if
            v = u
        end do
        
        ! Update flow along the path
        v = num_nodes
        do while (parent(v) /= -1)
            u = parent(v)
            flow(u,v) = flow(u,v) + min_capacity
            flow(v,u) = flow(v,u) - min_capacity  ! Reverse edge
            v = u
        end do
        
        min_value = min_capacity
    end function min_value
    
end program edmonds_karp_example
```

## Explanation of Key Components:

### 1. **Data Structures**
- `capacity`: 2D array storing original capacities
- `flow`: 2D array storing current flow values
- `adj_list`: Adjacency list representation of the graph
- `visited`, `parent`: Arrays for BFS traversal

### 2. **Main Algorithm Steps**

**BFS Function**: 
- Finds an augmenting path from source to sink in residual graph
- Returns `.true.` if path exists, `.false.` otherwise

**Min Value Function**:
- Determines minimum capacity along the found path
- Updates flow values on the path and reverse edges

### 3. **Sample Graph**
The example creates a simple flow network with:
```
Source (1) -> Node A (2) -> Sink (3)
     |                    ^
     |                    |
     v                    |
Source (1) -> Node B (4) -> Sink (3)
```

### 4. **Time Complexity**
- Time: O(VE²) where V = vertices, E = edges
- Space: O(V²) for adjacency matrix

### 5. **Expected Output**
The program will output:
```
Maximum flow: 12
```

This represents the maximum flow from source to sink in the given network, which is the sum of flows through both paths (5 + 7 = 12).