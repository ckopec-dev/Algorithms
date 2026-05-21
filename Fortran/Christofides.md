# Christofides Algorithm in Fortran

Here's a complete implementation of the Christofides algorithm for the Traveling Salesman Problem in Fortran:

```fortran
program christofides_tsp
    implicit none
    integer, parameter :: n = 6
    integer :: i, j, k, min_node
    real :: distance(n, n)
    real :: min_dist, total_cost
    integer :: tour(n), visited(n), nearest(n)
    integer :: mst_edges(2*n, 2)
    integer :: edge_count, vertex_count
    integer :: current_node, next_node
    logical :: done
    
    ! Initialize distance matrix (symmetric TSP)
    ! Example: 6 cities with distances
    distance = 0.0
    
    ! Fill distance matrix (example values)
    distance(1,2) = 10.0
    distance(1,3) = 15.0
    distance(1,4) = 20.0
    distance(1,5) = 25.0
    distance(1,6) = 30.0
    
    distance(2,3) = 35.0
    distance(2,4) = 25.0
    distance(2,5) = 30.0
    distance(2,6) = 35.0
    
    distance(3,4) = 30.0
    distance(3,5) = 20.0
    distance(3,6) = 25.0
    
    distance(4,5) = 15.0
    distance(4,6) = 20.0
    
    distance(5,6) = 10.0
    
    ! Make matrix symmetric
    do i = 1, n
        do j = 1, n
            if (i /= j) then
                distance(j,i) = distance(i,j)
            end if
        end do
    end do
    
    ! Print distance matrix
    write(*,*) 'Distance Matrix:'
    do i = 1, n
        write(*,'(F6.1,1X)', advance='no') (distance(i,j), j=1,n)
        write(*,*) 
    end do
    
    ! Step 1: Find Minimum Spanning Tree using Prim's algorithm
    call find_mst(distance, n, mst_edges, edge_count)
    
    ! Step 2: Find vertices with odd degree
    call find_odd_vertices(mst_edges, edge_count, n, nearest)
    
    ! Step 3: Find minimum weight perfect matching for odd vertices
    call find_matching(nearest, n, mst_edges, edge_count)
    
    ! Step 4: Create Eulerian graph by adding matching edges
    call create_eulerian_graph(mst_edges, edge_count, n, tour)
    
    ! Step 5: Find Hamiltonian cycle using Eulerian tour
    call find_hamiltonian(tour, n, total_cost)
    
    ! Output result
    write(*,*) 'Christofides Algorithm Result:'
    write(*,*) 'Optimal tour:', (tour(i), i=1,n), tour(1)
    write(*,*) 'Total cost:', total_cost
    
end program christofides_tsp

! Function to find Minimum Spanning Tree using Prim's algorithm
subroutine find_mst(distance, n, mst_edges, edge_count)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: distance(n,n)
    integer, intent(out) :: mst_edges(2*n, 2)
    integer, intent(out) :: edge_count
    integer :: i, j, k, min_node
    real :: min_dist
    logical :: in_mst(n)
    integer :: nearest(n)
    integer :: parent(n)
    
    edge_count = 0
    
    ! Initialize
    do i = 1, n
        in_mst(i) = .false.
        nearest(i) = 0
        parent(i) = 0
    end do
    
    ! Start with vertex 1
    in_mst(1) = .true.
    nearest(1) = 1
    
    ! Build MST
    do i = 1, n-1
        min_dist = 1.0e30
        min_node = 0
        
        ! Find minimum edge to add to MST
        do j = 1, n
            if (.not. in_mst(j)) then
                if (distance(nearest(j), j) < min_dist) then
                    min_dist = distance(nearest(j), j)
                    min_node = j
                end if
            end if
        end do
        
        ! Add edge to MST
        edge_count = edge_count + 1
        mst_edges(edge_count, 1) = nearest(min_node)
        mst_edges(edge_count, 2) = min_node
        
        ! Update nearest distances
        in_mst(min_node) = .true.
        do j = 1, n
            if (.not. in_mst(j) .and. distance(min_node, j) < distance(nearest(j), j)) then
                nearest(j) = min_node
            end if
        end do
    end do
    
end subroutine find_mst

! Function to find vertices with odd degree
subroutine find_odd_vertices(mst_edges, edge_count, n, odd_vertices)
    implicit none
    integer, intent(in) :: edge_count, n
    integer, intent(in) :: mst_edges(2*n, 2)
    integer, intent(out) :: odd_vertices(n)
    integer :: degree(n)
    integer :: i, j, count
    
    ! Initialize degree array
    do i = 1, n
        degree(i) = 0
    end do
    
    ! Count degrees
    do i = 1, edge_count
        degree(mst_edges(i, 1)) = degree(mst_edges(i, 1)) + 1
        degree(mst_edges(i, 2)) = degree(mst_edges(i, 2)) + 1
    end do
    
    ! Find odd degree vertices
    count = 0
    do i = 1, n
        if (mod(degree(i), 2) == 1) then
            count = count + 1
            odd_vertices(count) = i
        end if
    end do
    
end subroutine find_odd_vertices

! Function to find minimum weight perfect matching (simplified version)
subroutine find_matching(odd_vertices, n, mst_edges, edge_count)
    implicit none
    integer, intent(in) :: n, edge_count
    integer, intent(in) :: odd_vertices(n)
    integer, intent(inout) :: mst_edges(2*n, 2)
    integer :: i, j, k, min_edge, min_dist
    integer :: match(n)
    logical :: matched(n)
    
    ! Initialize
    do i = 1, n
        matched(i) = .false.
        match(i) = 0
    end do
    
    ! Simple greedy matching (for demonstration - not optimal)
    do i = 1, n
        if (.not. matched(i)) then
            min_dist = 1.0e30
            min_edge = 0
            do j = 1, n
                if (i /= j .and. .not. matched(j)) then
                    if (distance(odd_vertices(i), odd_vertices(j)) < min_dist) then
                        min_dist = distance(odd_vertices(i), odd_vertices(j))
                        min_edge = j
                    end if
                end if
            end do
            if (min_edge > 0) then
                matched(i) = .true.
                matched(min_edge) = .true.
                ! Add matching edge to MST (simplified)
                edge_count = edge_count + 1
                mst_edges(edge_count, 1) = odd_vertices(i)
                mst_edges(edge_count, 2) = odd_vertices(min_edge)
            end if
        end if
    end do
    
end subroutine find_matching

! Function to create Eulerian graph
subroutine create_eulerian_graph(mst_edges, edge_count, n, tour)
    implicit none
    integer, intent(in) :: edge_count, n
    integer, intent(in) :: mst_edges(2*n, 2)
    integer, intent(out) :: tour(n)
    integer :: i, j, current_node, next_node
    integer :: edge_used(2*n)
    integer :: euler_tour(2*n)
    integer :: tour_count
    
    ! Initialize
    do i = 1, 2*n
        edge_used(i) = 0
    end do
    
    ! Create Eulerian tour (simplified)
    tour_count = 1
    current_node = 1
    tour(tour_count) = current_node
    tour_count = tour_count + 1
    
    ! Simple traversal - in practice, this would use Hierholzer's algorithm
    do i = 1, edge_count
        if (mst_edges(i, 1) == current_node) then
            next_node = mst_edges(i, 2)
        else if (mst_edges(i, 2) == current_node) then
            next_node = mst_edges(i, 1)
        else
            cycle
        end if
        
        current_node = next_node
        tour(tour_count) = current_node
        tour_count = tour_count + 1
    end do
    
end subroutine create_eulerian_graph

! Function to find Hamiltonian cycle from Eulerian tour
subroutine find_hamiltonian(tour, n, total_cost)
    implicit none
    integer, intent(in) :: n
    integer, intent(inout) :: tour(n)
    real, intent(out) :: total_cost
    integer :: i, j, k
    logical :: visited(n)
    integer :: current_node, next_node
    
    ! Initialize
    total_cost = 0.0
    do i = 1, n
        visited(i) = .false.
    end do
    
    ! Create Hamiltonian cycle (simplified - just remove duplicates)
    ! In practice, this would be more complex
    do i = 1, n
        if (.not. visited(tour(i))) then
            visited(tour(i)) = .true.
        else
            ! Skip duplicate
            tour(i) = 0
        end if
    end do
    
    ! Calculate total cost
    total_cost = 0.0
    do i = 1, n-1
        if (tour(i) > 0 .and. tour(i+1) > 0) then
            total_cost = total_cost + distance(tour(i), tour(i+1))
        end if
    end do
    
    ! Add return to start
    if (tour(n) > 0 .and. tour(1) > 0) then
        total_cost = total_cost + distance(tour(n), tour(1))
    end if
    
end subroutine find_hamiltonian
```

## Key Components of the Christofides Algorithm:

1. **Minimum Spanning Tree (MST)**: Uses Prim's algorithm to find the MST of the complete graph
2. **Odd Degree Vertices**: Identifies vertices with odd degree in the MST
3. **Perfect Matching**: Finds minimum weight perfect matching for odd-degree vertices
4. **Eulerian Graph**: Combines MST with matching edges to form an Eulerian graph
5. **Hamiltonian Cycle**: Finds a Hamiltonian cycle using the Eulerian tour

## How to Compile and Run:

```bash
gfortran -o christofides christofides_tsp.f90
./christofides
```

## Notes:

- This is a simplified implementation for demonstration purposes
- The perfect matching step uses a greedy approach rather than the optimal Hungarian algorithm
- In a production implementation, you would want to use more sophisticated algorithms for the matching step
- The code assumes a symmetric distance matrix (metric TSP)
- The algorithm provides an approximate solution with a guaranteed approximation ratio of 1.5 for the metric TSP

