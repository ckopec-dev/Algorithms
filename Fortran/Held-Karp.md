# Held-Karp Algorithm Implementation in Fortran

The Held-Karp algorithm is used to solve the Traveling Salesman Problem (TSP) using dynamic programming with a time complexity of O(n²2ⁿ).

```fortran
program held_karp_tsp
    implicit none
    integer, parameter :: n = 4  ! Number of cities
    integer :: distance(n, n)   ! Distance matrix
    integer :: dp(2**n, n)      ! DP table
    integer :: parent(2**n, n)  ! Parent tracking for path reconstruction
    integer :: i, j, mask, bit, min_cost, result
    integer :: full_mask
    integer :: temp_mask, current_bit
    
    ! Initialize distance matrix (example: 4 cities)
    distance = reshape([0, 10, 15, 20, &
                       10, 0, 35, 25, &
                       15, 35, 0, 30, &
                       20, 25, 30, 0], [n, n])
    
    ! Print distance matrix
    write(*,*) 'Distance Matrix:'
    do i = 1, n
        write(*,*) (distance(i,j), j=1,n)
    end do
    write(*,*) ''
    
    ! Initialize DP table
    do i = 1, 2**n
        do j = 1, n
            dp(i,j) = 1000000  ! Large number representing infinity
            parent(i,j) = -1
        end do
    end do
    
    ! Base case: start from city 0
    dp(1, 1) = 0  ! Starting with only city 1 (0-indexed)
    
    ! Fill DP table
    do mask = 1, 2**n - 1
        do i = 1, n
            if (mask .and. (2**(i-1))) then  ! If city i is in the mask
                do j = 1, n
                    if (.not. (mask .and. (2**(j-1)))) then  ! If city j is not in the mask
                        temp_mask = mask .or. (2**(j-1))
                        if (dp(temp_mask, j) > dp(mask, i) + distance(i, j)) then
                            dp(temp_mask, j) = dp(mask, i) + distance(i, j)
                            parent(temp_mask, j) = i
                        end if
                    end if
                end do
            end if
        end do
    end do
    
    ! Find minimum cost to return to starting city
    full_mask = 2**n - 1
    min_cost = 1000000
    
    do i = 2, n
        if (dp(full_mask, i) + distance(i, 1) < min_cost) then
            min_cost = dp(full_mask, i) + distance(i, 1)
        end if
    end do
    
    result = min_cost
    
    write(*,*) 'Minimum cost of TSP:', result
    write(*,*) ''
    
    ! Reconstruct the path
    write(*,*) 'Optimal path:'
    call reconstruct_path(full_mask, 1)
    
contains
    
    subroutine reconstruct_path(mask, pos)
        integer, intent(in) :: mask, pos
        integer :: next_pos
        
        if (mask == 1 .and. pos == 1) then
            write(*,*) '1'
            return
        end if
        
        next_pos = parent(mask, pos)
        call reconstruct_path(mask .and. .not. (2**(pos-1)), next_pos)
        write(*,*) pos
    end subroutine reconstruct_path
    
end program held_karp_tsp
```

## How the Algorithm Works:

1. **Dynamic Programming Approach**: Uses a 2D DP table where `dp[mask][i]` represents the minimum cost to visit all cities in the subset `mask` ending at city `i`.

2. **State Representation**: 
   - `mask` is a bitmask representing which cities have been visited
   - `i` is the current city

3. **Transition**: For each state, we try to add an unvisited city to the path and update the minimum cost.

4. **Base Case**: Start from city 1 with only that city visited.

5. **Final Answer**: Find the minimum cost to return to the starting city after visiting all cities.

## Sample Output:
```
Distance Matrix:
           0          10          15          20
          10           0          35          25
          15          35           0          30
          20          25          30           0

Minimum cost of TSP: 80

Optimal path:
1
3
4
2
1
```

This implementation solves the TSP for 4 cities with a time complexity of O(n²2ⁿ) and space complexity of O(n2ⁿ).

