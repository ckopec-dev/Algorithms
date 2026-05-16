# Kosaraju's Algorithm in Fortran

Here's a complete implementation of Kosaraju's algorithm for finding strongly connected components in a directed graph using Fortran:

```fortran
program kosaraju_algorithm
    implicit none
    integer, parameter :: MAX_NODES = 100
    integer :: num_nodes, num_edges
    integer :: adj_matrix(MAX_NODES, MAX_NODES)
    integer :: reversed_adj(MAX_NODES, MAX_NODES)
    integer :: visited(MAX_NODES)
    integer :: stack(MAX_NODES)
    integer :: stack_top
    integer :: component_id(MAX_NODES)
    integer :: component_count
    integer :: i, j, k
    
    ! Initialize arrays
    do i = 1, MAX_NODES
        do j = 1, MAX_NODES
            adj_matrix(i, j) = 0
            reversed_adj(i, j) = 0
        end do
        visited(i) = 0
        component_id(i) = 0
    end do
    
    ! Example graph input
    ! Graph with 5 nodes and 6 edges
    num_nodes = 5
    num_edges = 6
    
    ! Original adjacency matrix
    adj_matrix(1, 2) = 1  ! 1 -> 2
    adj_matrix(2, 3) = 1  ! 2 -> 3
    adj_matrix(3, 1) = 1  ! 3 -> 1
    adj_matrix(3, 4) = 1  ! 3 -> 4
    adj_matrix(4, 5) = 1  ! 4 -> 5
    adj_matrix(5, 4) = 1  ! 5 -> 4
    
    ! Create reversed graph
    do i = 1, num_nodes
        do j = 1, num_nodes
            reversed_adj(i, j) = adj_matrix(j, i)
        end do
    end do
    
    ! Clear visited array for first DFS
    do i = 1, num_nodes
        visited(i) = 0
    end do
    
    ! First DFS to fill stack
    stack_top = 0
    do i = 1, num_nodes
        if (visited(i) == 0) then
            call dfs_first(i, adj_matrix, visited, stack, stack_top, num_nodes)
        end if
    end do
    
    ! Clear visited array for second DFS
    do i = 1, num_nodes
        visited(i) = 0
    end do
    
    ! Second DFS on reversed graph
    component_count = 0
    do i = stack_top, 1, -1
        if (visited(stack(i)) == 0) then
            component_count = component_count + 1
            call dfs_second(stack(i), reversed_adj, visited, component_id, component_count, num_nodes)
        end if
    end do
    
    ! Output results
    write(*,*) 'Strongly Connected Components:'
    do i = 1, component_count
        write(*,*) 'Component ', i, ':'
        do j = 1, num_nodes
            if (component_id(j) == i) then
                write(*,*) '  Node ', j
            end if
        end do
    end do
    
end program kosaraju_algorithm

! First DFS to fill stack
subroutine dfs_first(node, adj_matrix, visited, stack, stack_top, num_nodes)
    implicit none
    integer, intent(in) :: node, num_nodes
    integer, intent(inout) :: adj_matrix(MAX_NODES, MAX_NODES)
    integer, intent(inout) :: visited(MAX_NODES)
    integer, intent(inout) :: stack(MAX_NODES)
    integer, intent(inout) :: stack_top
    
    integer :: i
    
    visited(node) = 1
    
    do i = 1, num_nodes
        if (adj_matrix(node, i) == 1 .and. visited(i) == 0) then
            call dfs_first(i, adj_matrix, visited, stack, stack_top, num_nodes)
        end if
    end do
    
    stack_top = stack_top + 1
    stack(stack_top) = node
end subroutine dfs_first

! Second DFS to find components
subroutine dfs_second(node, adj_matrix, visited, component_id, component_count, num_nodes)
    implicit none
    integer, intent(in) :: node, num_nodes, component_count
    integer, intent(inout) :: adj_matrix(MAX_NODES, MAX_NODES)
    integer, intent(inout) :: visited(MAX_NODES)
    integer, intent(inout) :: component_id(MAX_NODES)
    
    integer :: i
    
    visited(node) = 1
    component_id(node) = component_count
    
    do i = 1, num_nodes
        if (adj_matrix(node, i) == 1 .and. visited(i) == 0) then
            call dfs_second(i, adj_matrix, visited, component_id, component_count, num_nodes)
        end if
    end do
end subroutine dfs_second
```

## Algorithm Explanation

Kosaraju's algorithm works in three main steps:

1. **First DFS**: Perform depth-first search on the original graph and push nodes to a stack in the order of completion
2. **Reverse Graph**: Create the transpose of the original graph
3. **Second DFS**: Perform DFS on the reversed graph using nodes from the stack (in reverse order) to find strongly connected components

## Key Features

- Uses adjacency matrix representation for the graph
- Implements two separate DFS functions
- Maintains a stack to store the order of nodes
- Tracks component assignments for each node
- Handles up to 100 nodes with parameter MAX_NODES

## Sample Output

For the example graph with 5 nodes, the output would show:
```
Strongly Connected Components:
Component 1:
  Node 1
  Node 2
  Node 3
Component 2:
  Node 4
  Node 5
```

This indicates that nodes 1, 2, and 3 form one strongly connected component, while nodes 4 and 5 form another.

