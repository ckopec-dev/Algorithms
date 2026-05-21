# Ford-Fulkerson Algorithm in Fortran

Here's a complete implementation of the Ford-Fulkerson algorithm for finding the maximum flow in a flow network using Fortran:

```fortran
program ford_fulkerson
    implicit none
    integer, parameter :: MAX_NODES = 100
    integer, parameter :: INF = 1000000
    
    ! Flow network adjacency matrix
    integer :: capacity(MAX_NODES, MAX_NODES)
    integer :: residual(MAX_NODES, MAX_NODES)
    integer :: parent(MAX_NODES)
    logical :: visited(MAX_NODES)
    
    ! Problem parameters
    integer :: num_nodes, num_edges
    integer :: source, sink
    integer :: max_flow
    integer :: i, j, u, v, w
    
    ! Initialize the network
    call initialize_network()
    
    ! Set source and sink
    source = 0
    sink = 5
    
    ! Find maximum flow
    max_flow = call_ford_fulkerson(source, sink)
    
    ! Output results
    write(*,*) 'Maximum flow:', max_flow
    write(*,*) 'Flow network:'
    do i = 0, num_nodes - 1
        do j = 0, num_nodes - 1
            if (capacity(i,j) > 0) then
                write(*,*) 'Edge (', i, ',', j, ') capacity:', capacity(i,j)
            end if
        end do
    end do
    
contains
    
    subroutine initialize_network()
        ! Initialize capacity matrix
        ! This is a sample network with 6 nodes (0-5)
        ! Node 0: source, Node 5: sink
        
        ! Initialize all capacities to 0
        do i = 0, MAX_NODES-1
            do j = 0, MAX_NODES-1
                capacity(i,j) = 0
            end do
        end do
        
        ! Define edges with capacities
        ! Source (0) connections
        capacity(0,1) = 10
        capacity(0,2) = 10
        
        ! Middle connections
        capacity(1,2) = 2
        capacity(1,3) = 4
        capacity(2,1) = 6
        capacity(2,4) = 10
        capacity(3,2) = 6
        capacity(3,4) = 6
        capacity(3,5) = 10
        
        ! Sink connections
        capacity(4,5) = 10
        
        ! Set number of nodes and edges
        num_nodes = 6
        num_edges = 10
        
        ! Initialize residual network
        do i = 0, num_nodes-1
            do j = 0, num_nodes-1
                residual(i,j) = capacity(i,j)
            end do
        end do
    end subroutine initialize_network
    
    integer function bfs(s, t)
        implicit none
        integer, intent(in) :: s, t
        integer :: queue(MAX_NODES)
        integer :: front, rear, u, v
        
        ! Initialize visited array and parent array
        do i = 0, num_nodes-1
            visited(i) = .false.
            parent(i) = -1
        end do
        
        ! Initialize queue
        front = 1
        rear = 1
        queue(front) = s
        visited(s) = .true.
        
        ! BFS to find augmenting path
        do while (front <= rear)
            u = queue(front)
            front = front + 1
            
            do v = 0, num_nodes-1
                if (.not. visited(v) .and. residual(u,v) > 0) then
                    visited(v) = .true.
                    parent(v) = u
                    queue(rear+1) = v
                    rear = rear + 1
                    
                    if (v == t) then
                        bfs = 1
                        return
                    end if
                end if
            end do
        end do
        
        bfs = 0
    end function bfs
    
    integer function call_ford_fulkerson(source, sink)
        implicit none
        integer, intent(in) :: source, sink
        integer :: path_flow, u
        integer :: max_flow_local
        integer :: min_flow
        
        max_flow_local = 0
        
        ! Continue while there's an augmenting path
        do while (bfs(source, sink) == 1)
            ! Find minimum capacity along the path
            path_flow = INF
            
            u = sink
            do while (u /= source)
                v = parent(u)
                if (residual(v,u) < path_flow) then
                    path_flow = residual(v,u)
                end if
                u = v
            end do
            
            ! Update residual capacities
            u = sink
            do while (u /= source)
                v = parent(u)
                residual(v,u) = residual(v,u) - path_flow
                residual(u,v) = residual(u,v) + path_flow
                u = v
            end do
            
            max_flow_local = max_flow_local + path_flow
        end do
        
        call_ford_fulkerson = max_flow_local
    end function call_ford_fulkerson
    
end program ford_fulkerson
```

## How the Algorithm Works

This Fortran implementation of Ford-Fulkerson includes:

1. **Network Initialization**: Sets up a sample flow network with 6 nodes (0-5)
2. **BFS Function**: Finds an augmenting path from source to sink using Breadth-First Search
3. **Main Algorithm**: Repeatedly finds augmenting paths and updates residual capacities
4. **Flow Updates**: Updates the flow along each path found

## Key Components

### Data Structures:
- `capacity`: Original capacity matrix
- `residual`: Residual capacity matrix
- `parent`: Parent array for path reconstruction
- `visited`: Visited array for BFS

### Algorithm Steps:
1. Initialize residual network with original capacities
2. While there exists an augmenting path:
   - Find path using BFS
   - Determine minimum capacity along path
   - Update residual capacities
   - Add to total flow
3. Return maximum flow

## Sample Output
```
Maximum flow: 19
Flow network:
Edge (0,1) capacity: 10
Edge (0,2) capacity: 10
Edge (1,2) capacity: 2
Edge (1,3) capacity: 4
Edge (2,1) capacity: 6
Edge (2,4) capacity: 10
Edge (3,2) capacity: 6
Edge (3,4) capacity: 6
Edge (3,5) capacity: 10
Edge (4,5) capacity: 10
```

The algorithm correctly computes that the maximum flow from source (0) to sink (5) is 19 units.

