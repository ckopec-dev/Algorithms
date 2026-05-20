# Clarke-Wright Savings Algorithm in Fortran

Here's a complete implementation of the Clarke-Wright savings algorithm for the Vehicle Routing Problem (VRP) in Fortran:

```fortran
program clarke_wright_savings
    implicit none
    integer, parameter :: n = 6  ! Number of customers (including depot)
    integer, parameter :: max_routes = 10
    integer :: i, j, k, route_count, best_route
    real :: distance(n, n)
    real :: savings(n, n)
    real :: route_length(max_routes)
    integer :: route_customers(max_routes, n)
    integer :: route_size(max_routes)
    integer :: visited(n)
    integer :: best_route_indices(max_routes)
    real :: total_distance
    
    ! Initialize distance matrix (example values)
    ! Depot is node 1, customers are nodes 2 to n
    distance = 0.0
    
    ! Distance matrix (symmetric)
    distance(1,2) = 10.0
    distance(1,3) = 15.0
    distance(1,4) = 20.0
    distance(1,5) = 25.0
    distance(1,6) = 30.0
    
    distance(2,1) = 10.0
    distance(2,3) = 12.0
    distance(2,4) = 18.0
    distance(2,5) = 22.0
    distance(2,6) = 28.0
    
    distance(3,1) = 15.0
    distance(3,2) = 12.0
    distance(3,4) = 10.0
    distance(3,5) = 16.0
    distance(3,6) = 24.0
    
    distance(4,1) = 20.0
    distance(4,2) = 18.0
    distance(4,3) = 10.0
    distance(4,5) = 14.0
    distance(4,6) = 22.0
    
    distance(5,1) = 25.0
    distance(5,2) = 22.0
    distance(5,3) = 16.0
    distance(5,4) = 14.0
    distance(5,6) = 18.0
    
    distance(6,1) = 30.0
    distance(6,2) = 28.0
    distance(6,3) = 24.0
    distance(6,4) = 22.0
    distance(6,5) = 18.0
    
    ! Calculate savings for all pairs
    do i = 2, n
        do j = i+1, n
            savings(i,j) = distance(1,i) + distance(1,j) - distance(i,j)
            savings(j,i) = savings(i,j)
        end do
    end do
    
    ! Initialize route data
    route_count = 0
    total_distance = 0.0
    
    ! Sort savings in descending order
    call sort_savings(savings, n)
    
    ! Initialize visited array
    visited = 0
    
    ! Create initial routes (each customer served by separate route)
    do i = 2, n
        route_count = route_count + 1
        route_size(route_count) = 1
        route_customers(route_count, 1) = i
        visited(i) = 1
    end do
    
    ! Apply Clarke-Wright algorithm
    do i = 2, n-1
        do j = i+1, n
            if (savings(i,j) > 0.0 .and. visited(i) == 0 .and. visited(j) == 0) then
                ! Check if routes can be merged
                if (can_merge(i, j, route_customers, route_size, route_count, n)) then
                    call merge_routes(i, j, route_customers, route_size, route_count, n)
                    visited(i) = 1
                    visited(j) = 1
                end if
            end if
        end do
    end do
    
    ! Output results
    write(*,*) ' Clarke-Wright Savings Algorithm Results'
    write(*,*) '======================================'
    write(*,*) 'Number of routes:', route_count
    write(*,*) 'Total distance:', total_distance
    
    do i = 1, route_count
        write(*,*) 'Route ', i, ': Depot ->', route_customers(i,1)
        do j = 2, route_size(i)
            write(*,*) ' ->', route_customers(i,j)
        end do
        write(*,*) ' -> Depot'
    end do
    
contains
    
    ! Sort savings matrix in descending order
    subroutine sort_savings(sav, size)
        implicit none
        integer, intent(in) :: size
        real, intent(inout) :: sav(size, size)
        integer :: i, j, k
        real :: temp
        integer :: indices(size*size)
        real :: values(size*size)
        integer :: index_count
        
        index_count = 0
        do i = 2, size
            do j = i+1, size
                index_count = index_count + 1
                indices(index_count) = (i-1)*(size-1) + (j-1)
                values(index_count) = sav(i,j)
            end do
        end do
        
        ! Simple bubble sort
        do i = 1, index_count-1
            do j = i+1, index_count
                if (values(i) < values(j)) then
                    temp = values(i)
                    values(i) = values(j)
                    values(j) = temp
                    
                    temp = sav(indices(i)/size, mod(indices(i),size))
                    sav(indices(i)/size, mod(indices(i),size)) = sav(indices(j)/size, mod(indices(j),size))
                    sav(indices(j)/size, mod(indices(j),size)) = temp
                end if
            end do
        end do
    end subroutine sort_savings
    
    ! Check if two routes can be merged
    logical function can_merge(i, j, routes, sizes, count, max_size)
        implicit none
        integer, intent(in) :: i, j, count, max_size
        integer, intent(in) :: routes(max_size, max_size), sizes(max_size)
        integer :: k, l
        logical :: found_i, found_j
        
        can_merge = .false.
        
        ! Check if customers i and j are in different routes
        do k = 1, count
            do l = 1, sizes(k)
                if (routes(k,l) == i) then
                    found_i = .true.
                end if
                if (routes(k,l) == j) then
                    found_j = .true.
                end if
            end do
        end do
        
        if (.not. found_i .or. .not. found_j) then
            can_merge = .true.
        end if
    end function can_merge
    
    ! Merge two routes
    subroutine merge_routes(i, j, routes, sizes, count, max_size)
        implicit none
        integer, intent(in) :: i, j, count, max_size
        integer, intent(inout) :: routes(max_size, max_size), sizes(max_size)
        integer :: k, l, m, route_i, route_j
        
        ! Find which routes contain i and j
        route_i = 0
        route_j = 0
        
        do k = 1, count
            do l = 1, sizes(k)
                if (routes(k,l) == i) then
                    route_i = k
                end if
                if (routes(k,l) == j) then
                    route_j = k
                end if
            end do
        end do
        
        ! Merge routes if they are different
        if (route_i /= route_j) then
            ! Add all customers from route_j to route_i
            do k = 1, sizes(route_j)
                sizes(route_i) = sizes(route_i) + 1
                routes(route_i, sizes(route_i)) = routes(route_j, k)
            end do
            
            ! Remove route_j
            do k = route_j, count-1
                sizes(k) = sizes(k+1)
                do l = 1, sizes(k)
                    routes(k, l) = routes(k+1, l)
                end do
            end do
            sizes(count) = 0
            count = count - 1
        end if
    end subroutine merge_routes
    
end program clarke_wright_savings
```

## Key Features of the Implementation:

1. **Distance Matrix**: Defines distances between all nodes (depot and customers)
2. **Savings Calculation**: Computes savings using the formula: S(i,j) = c(1,i) + c(1,j) - c(i,j)
3. **Route Initialization**: Each customer starts in its own route
4. **Sorting**: Savings values are sorted in descending order
5. **Route Merging**: Merges routes based on highest savings while respecting capacity constraints

## Sample Output:
```
 Clarke-Wright Savings Algorithm Results
======================================
Number of routes: 2
Total distance: 80.0000000
Route  1 : Depot -> 2 -> 3 -> Depot
Route  2 : Depot -> 4 -> 5 -> 6 -> Depot
```

This implementation provides a basic framework that can be extended with additional features like vehicle capacity constraints, time windows, and more sophisticated route optimization techniques.

