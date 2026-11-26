# Dinic's Algorithm Implementation in Fortran

Here's a complete implementation of Dinic's algorithm for finding maximum flow in a flow network using Fortran:

```fortran
program dinic_algorithm
    implicit none
    integer, parameter :: MAXN = 1000, INF = 1000000000
    integer :: n, m, s, t
    integer :: head(MAXN), to(MAXN*2), next_edge(MAXN*2), capacity(MAXN*2)
    integer :: level(MAXN), work(MAXN)
    integer :: edge_count
    
    ! Function declarations
    integer :: bfs(), dfs()
    
    ! Initialize graph
    call init_graph()
    
    ! Add edges (example network)
    call add_edge(1, 2, 10)
    call add_edge(1, 3, 10)
    call add_edge(2, 4, 4)
    call add_edge(2, 3, 2)
    call add_edge(3, 4, 6)
    call add_edge(3, 5, 10)
    call add_edge(4, 5, 10)
    call add_edge(4, 6, 10)
    call add_edge(5, 6, 10)
    
    s = 1
    t = 6
    
    ! Run Dinic's algorithm
    write(*,*) 'Maximum flow from node ', s, ' to node ', t, ':'
    write(*,*) 'Result:', dinic()
    
contains

    subroutine init_graph()
        integer :: i
        edge_count = 0
        do i = 1, MAXN
            head(i) = 0
        end do
    end subroutine init_graph
    
    subroutine add_edge(u, v, cap)
        integer, intent(in) :: u, v, cap
        
        ! Forward edge
        edge_count = edge_count + 1
        to(edge_count) = v
        capacity(edge_count) = cap
        next_edge(edge_count) = head(u)
        head(u) = edge_count
        
        ! Backward edge (for residual network)
        edge_count = edge_count + 1
        to(edge_count) = u
        capacity(edge_count) = 0  ! Backward edge has 0 capacity initially
        next_edge(edge_count) = head(v)
        head(v) = edge_count
    end subroutine add_edge
    
    integer function bfs()
        integer :: i, u, v, q(MAXN), front, rear
        logical :: visited(MAXN)
        
        ! Initialize
        do i = 1, n
            level(i) = -1
            visited(i) = .false.
        end do
        
        front = 1
        rear = 0
        level(s) = 0
        visited(s) = .true.
        rear = rear + 1
        q(rear) = s
        
        ! BFS to find level of each node
        do while (front <= rear)
            u = q(front)
            front = front + 1
            
            i = head(u)
            do while (i /= 0)
                v = to(i)
                if (.not. visited(v) .and. capacity(i) > 0) then
                    visited(v) = .true.
                    level(v) = level(u) + 1
                    rear = rear + 1
                    q(rear) = v
                end if
                i = next_edge(i)
            end do
        end do
        
        bfs = level(t)
    end function bfs
    
    integer function dfs(u, flow)
        integer, intent(in) :: u, flow
        integer :: i, v, pushed, min_flow
        
        if (u == t) then
            dfs = flow
            return
        end if
        
        i = work(u)
        do while (i /= 0)
            v = to(i)
            if (level(v) == level(u) + 1 .and. capacity(i) > 0) then
                min_flow = min(flow, capacity(i))
                pushed = dfs(v, min_flow)
                
                if (pushed > 0) then
                    capacity(i) = capacity(i) - pushed
                    capacity(i+1) = capacity(i+1) + pushed
                    dfs = pushed
                    return
                end if
            end if
            i = next_edge(i)
            work(u) = i
        end do
        
        dfs = 0
    end function dfs
    
    integer function dinic()
        integer :: total_flow, flow
        
        total_flow = 0
        
        do while (bfs() /= -1)
            do i = 1, n
                work(i) = head(i)
            end do
            
            do while (.true.)
                flow = dfs(s, INF)
                if (flow == 0) then
                    exit
                end if
                total_flow = total_flow + flow
            end do
        end do
        
        dinic = total_flow
    end function dinic
    
end program dinic_algorithm
```

## Key Components Explained:

### 1. **Data Structures**
- `head()`: Adjacency list head pointers
- `to()`: Destination nodes of edges
- `next_edge()`: Next edge in adjacency list
- `capacity()`: Edge capacities (forward and backward edges)
- `level()`: BFS levels for layering
- `work()`: Current pointer for DFS traversal

### 2. **Algorithm Steps**
1. **BFS Phase**: Build level graph to determine layering
2. **DFS Phase**: Find augmenting paths using blocking flow
3. **Repeat**: Continue until no more augmenting paths exist

### 3. **Time Complexity**
- O(V²E) in worst case
- O(EV²) for sparse graphs
- More efficient than Ford-Fulkerson for dense networks

### 4. **Example Network**
```
1 → 2 (10) → 4 (4) → 6 (10)
 ↓    ↘         ↘ 
3 (10) → 5 (10) → 6 (10)
 ↓
4 (6) → 5 (10)
```

This implementation finds the maximum flow from node 1 to node 6 in the example network. The algorithm handles both forward and backward edges in the residual network for efficient flow updates.