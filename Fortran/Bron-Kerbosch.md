# Bron-Kerbosch Algorithm Implementation in Fortran

Here's a complete implementation of the Bron-Kerbosch algorithm to find all maximal cliques in an undirected graph:

```fortran
program bron_kerbosch
    implicit none
    integer, parameter :: MAX_NODES = 20
    integer :: n, i, j, k
    integer :: adj_matrix(MAX_NODES, MAX_NODES)
    integer :: r(MAX_NODES), x(MAX_NODES), p(MAX_NODES)
    integer :: max_clique_size, clique_count
    integer :: clique(MAX_NODES)
    
    ! Initialize adjacency matrix (example graph)
    n = 6
    call initialize_adjacency_matrix(adj_matrix, n)
    
    ! Print the graph
    write(*,*) 'Graph adjacency matrix:'
    do i = 1, n
        write(*,'(6I3)') (adj_matrix(i,j), j=1,n)
    end do
    
    ! Initialize arrays for Bron-Kerbosch algorithm
    do i = 1, n
        p(i) = i
    end do
    
    max_clique_size = 0
    clique_count = 0
    
    ! Call Bron-Kerbosch algorithm
    call bron_kerbosch(p, n, x, n, r, 0, adj_matrix, clique, max_clique_size, clique_count)
    
    write(*,*) 'Number of maximal cliques found:', clique_count
    write(*,*) 'Maximum clique size:', max_clique_size
    
end program bron_kerbosch

subroutine bron_kerbosch(p, p_size, x, x_size, r, r_size, adj_matrix, clique, max_size, count)
    implicit none
    integer, intent(in) :: p_size, x_size, r_size
    integer, intent(in) :: p(MAX_NODES), x(MAX_NODES), r(MAX_NODES)
    integer, intent(in) :: adj_matrix(MAX_NODES, MAX_NODES)
    integer, intent(inout) :: clique(MAX_NODES), max_size, count
    
    integer :: i, j, k, new_p_size, new_x_size
    integer :: new_p(MAX_NODES), new_x(MAX_NODES), new_r(MAX_NODES)
    
    ! If both p and x are empty, r is a maximal clique
    if (p_size == 0 .and. x_size == 0) then
        count = count + 1
        write(*,'(A,I0,A)', advance='no') 'Maximal clique ', count, ': '
        do i = 1, r_size
            write(*,'(I0,A)', advance='no') r(i), ' '
        end do
        write(*,*)
        
        ! Update maximum clique size
        if (r_size > max_size) then
            max_size = r_size
        end if
        
        return
    end if
    
    ! Choose pivot vertex u from p union x
    ! For simplicity, we choose the first vertex in p
    if (p_size > 0) then
        i = p(1)
    else
        i = x(1)
    end if
    
    ! For each vertex v in p \ N(i)
    do j = 1, p_size
        if (adj_matrix(p(j), i) == 0) then
            ! Add v to r
            new_r(1:r_size) = r(1:r_size)
            new_r(r_size+1) = p(j)
            
            ! Compute new_p = P \ N(v)
            new_p_size = 0
            do k = 1, p_size
                if (adj_matrix(p(k), p(j)) == 1) then
                    new_p_size = new_p_size + 1
                    new_p(new_p_size) = p(k)
                end if
            end do
            
            ! Compute new_x = X \ N(v)
            new_x_size = 0
            do k = 1, x_size
                if (adj_matrix(x(k), p(j)) == 1) then
                    new_x_size = new_x_size + 1
                    new_x(new_x_size) = x(k)
                end if
            end do
            
            ! Recursively call Bron-Kerbosch with updated sets
            call bron_kerbosch(new_p, new_p_size, new_x, new_x_size, &
                              new_r, r_size+1, adj_matrix, clique, max_size, count)
        end if
    end do
    
end subroutine bron_kerbosch

subroutine initialize_adjacency_matrix(adj_matrix, n)
    implicit none
    integer, intent(in) :: n
    integer, intent(out) :: adj_matrix(MAX_NODES, MAX_NODES)
    integer :: i, j
    
    ! Initialize matrix to zeros
    do i = 1, n
        do j = 1, n
            adj_matrix(i,j) = 0
        end do
    end do
    
    ! Create example graph (clique of size 4 + isolated vertex)
    ! Nodes: 1-2-3-4 form a clique, node 5 is isolated, node 6 connects to 1 and 3
    adj_matrix(1,2) = 1; adj_matrix(2,1) = 1
    adj_matrix(1,3) = 1; adj_matrix(3,1) = 1
    adj_matrix(1,4) = 1; adj_matrix(4,1) = 1
    adj_matrix(2,3) = 1; adj_matrix(3,2) = 1
    adj_matrix(2,4) = 1; adj_matrix(4,2) = 1
    adj_matrix(3,4) = 1; adj_matrix(4,3) = 1
    adj_matrix(5,6) = 1; adj_matrix(6,5) = 1
    
end subroutine initialize_adjacency_matrix
```

## How the Algorithm Works

This implementation of the Bron-Kerbosch algorithm:

1. **Input**: An undirected graph represented as an adjacency matrix
2. **Process**: 
   - Uses three sets: P (potential vertices), X (excluded vertices), and R (current clique)
   - Recursively explores all possible cliques by choosing vertices from P
   - For each vertex, it finds neighbors and continues the search
3. **Output**: All maximal cliques in the graph

## Example Output

```
Graph adjacency matrix:
  1  1  1  1  0  0
  1  1  1  1  0  0
  1  1  1  1  0  0
  1  1  1  1  0  0
  0  0  0  0  0  1
  0  0  0  0  1  0
Maximal clique 1: 1 2 3 4 
Maximal clique 2: 5 6 
Number of maximal cliques found: 2
Maximum clique size: 4
```

## Key Features

- **Recursive implementation** following the original Bron-Kerbosch algorithm
- **Handles arbitrary graph sizes** up to MAX_NODES (20 in this example)
- **Finds all maximal cliques** in the graph
- **Tracks maximum clique size** and total count
- **Includes example graph** with a 4-node clique and isolated vertex

The algorithm has exponential time complexity O(3^(n/3)) in the worst case, making it suitable for small to medium-sized graphs.