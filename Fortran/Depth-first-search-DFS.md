# Depth-First Search (DFS) in Fortran

Here's an implementation of the Depth-First Search algorithm in Fortran:

```fortran
program dfs_example
    implicit none
    integer, parameter :: MAX_NODES = 100
    integer :: adj_matrix(MAX_NODES, MAX_NODES)
    logical :: visited(MAX_NODES)
    integer :: num_nodes, num_edges
    integer :: i, j, start_node
    
    ! Initialize adjacency matrix
    call initialize_adjacency_matrix()
    
    ! Read graph information
    write(*,*) 'Enter number of nodes:'
    read(*,*) num_nodes
    write(*,*) 'Enter number of edges:'
    read(*,*) num_edges
    
    ! Initialize visited array
    do i = 1, num_nodes
        visited(i) = .false.
    end do
    
    ! Read edges and build adjacency matrix
    write(*,*) 'Enter edges (node1 node2):'
    do i = 1, num_edges
        read(*,*) j, start_node
        adj_matrix(j, start_node) = 1
        adj_matrix(start_node, j) = 1  ! For undirected graph
    end do
    
    ! Read starting node
    write(*,*) 'Enter starting node:'
    read(*,*) start_node
    
    ! Perform DFS
    write(*,*) 'DFS traversal:'
    call dfs(start_node, num_nodes)
    
end program dfs_example

subroutine dfs(node, num_nodes)
    implicit none
    integer, intent(in) :: node, num_nodes
    integer :: i
    logical :: visited(MAX_NODES)
    
    ! Mark current node as visited
    visited(node) = .true.
    write(*,*) node, ' '
    
    ! Visit all adjacent nodes
    do i = 1, num_nodes
        if (adj_matrix(node, i) == 1 .and. .not. visited(i)) then
            call dfs(i, num_nodes)
        end if
    end do
end subroutine dfs

subroutine initialize_adjacency_matrix()
    implicit none
    integer :: i, j
    
    ! Initialize adjacency matrix with zeros
    do i = 1, MAX_NODES
        do j = 1, MAX_NODES
            adj_matrix(i, j) = 0
        end do
    end do
end subroutine initialize_adjacency_matrix
```

## Alternative Implementation with Stack

```fortran
program dfs_stack
    implicit none
    integer, parameter :: MAX_NODES = 100
    integer, parameter :: MAX_STACK = 1000
    integer :: adj_matrix(MAX_NODES, MAX_NODES)
    logical :: visited(MAX_NODES)
    integer :: stack(MAX_STACK)
    integer :: stack_top
    integer :: num_nodes, num_edges
    integer :: i, j, start_node
    
    ! Initialize
    stack_top = 0
    
    ! Read graph information
    write(*,*) 'Enter number of nodes:'
    read(*,*) num_nodes
    write(*,*) 'Enter number of edges:'
    read(*,*) num_edges
    
    ! Initialize adjacency matrix
    do i = 1, MAX_NODES
        do j = 1, MAX_NODES
            adj_matrix(i, j) = 0
        end do
    end do
    
    ! Initialize visited array
    do i = 1, num_nodes
        visited(i) = .false.
    end do
    
    ! Read edges
    write(*,*) 'Enter edges (node1 node2):'
    do i = 1, num_edges
        read(*,*) j, start_node
        adj_matrix(j, start_node) = 1
        adj_matrix(start_node, j) = 1
    end do
    
    ! Read starting node
    write(*,*) 'Enter starting node:'
    read(*,*) start_node
    
    ! Perform iterative DFS using stack
    write(*,*) 'DFS traversal:'
    call dfs_iterative(start_node, num_nodes)
    
end program dfs_stack

subroutine dfs_iterative(start_node, num_nodes)
    implicit none
    integer, intent(in) :: start_node, num_nodes
    integer :: current_node, i
    integer :: stack(MAX_STACK)
    integer :: stack_top
    
    stack_top = 0
    
    ! Push starting node
    stack_top = stack_top + 1
    stack(stack_top) = start_node
    
    do while (stack_top > 0)
        ! Pop from stack
        current_node = stack(stack_top)
        stack_top = stack_top - 1
        
        ! If not visited, process it
        if (.not. visited(current_node)) then
            visited(current_node) = .true.
            write(*,*) current_node, ' '
            
            ! Push all unvisited neighbors
            do i = num_nodes, 1, -1
                if (adj_matrix(current_node, i) == 1 .and. .not. visited(i)) then
                    stack_top = stack_top + 1
                    stack(stack_top) = i
                end if
            end do
        end if
    end do
end subroutine dfs_iterative
```

## Sample Input/Output

**Input:**
```
Enter number of nodes:
5
Enter number of edges:
6
Enter edges (node1 node2):
1 2
1 3
2 4
2 5
3 4
4 5
Enter starting node:
1
```

**Output:**
```
DFS traversal:
1 2 4 5 3 
```

## Key Features

- **Recursive DFS**: First implementation uses recursion
- **Iterative DFS**: Second implementation uses explicit stack
- **Undirected Graph**: Edges are bidirectional
- **Visited Tracking**: Prevents revisiting nodes
- **Adjacency Matrix**: Efficient representation for dense graphs

## Time Complexity
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V) for visited array and recursion stack

