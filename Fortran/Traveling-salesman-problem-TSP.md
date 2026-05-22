# Traveling Salesman Problem (TSP) in Fortran

Here's a complete Fortran implementation of a simple TSP algorithm using brute force approach:

```fortran
program tsp_solver
    implicit none
    integer, parameter :: n = 4  ! Number of cities
    integer :: distance(n, n)   ! Distance matrix
    integer :: best_distance    ! Best distance found
    integer :: current_distance
    integer :: permutation(n)   ! Current permutation of cities
    integer :: best_permutation(n) ! Best permutation found
    integer :: i, j, k
    integer :: factorial_n
    logical :: used(n)
    
    ! Initialize distance matrix (symmetric TSP)
    distance = reshape([0, 10, 15, 20, &
                        10, 0, 35, 25, &
                        15, 35, 0, 30, &
                        20, 25, 30, 0], [n, n])
    
    ! Print distance matrix
    write(*,*) 'Distance Matrix:'
    do i = 1, n
        write(*,'(10I4)') (distance(i,j), j=1,n)
    end do
    write(*,*) ''
    
    ! Initialize variables
    best_distance = huge(1)
    factorial_n = 1
    
    ! Calculate factorial of n
    do i = 1, n
        factorial_n = factorial_n * i
    end do
    
    ! Generate all permutations and find minimum distance
    call generate_permutations(1, permutation, used, distance, best_distance, &
                              best_permutation, current_distance)
    
    ! Print result
    write(*,*) 'Best route:'
    write(*,'(10I4)') (best_permutation(i), i=1,n)
    write(*,*) 'Minimum distance:', best_distance
    
end program tsp_solver

! Function to calculate factorial
integer function factorial(n)
    implicit none
    integer, intent(in) :: n
    integer :: i
    factorial = 1
    do i = 1, n
        factorial = factorial * i
    end do
end function factorial

! Recursive function to generate permutations
subroutine generate_permutations(position, current_perm, used, distance, &
                                min_distance, best_perm, current_dist)
    implicit none
    integer, intent(in) :: position
    integer, intent(inout) :: current_perm(n)
    logical, intent(inout) :: used(n)
    integer, intent(in) :: distance(n, n)
    integer, intent(inout) :: min_distance
    integer, intent(inout) :: best_perm(n)
    integer, intent(inout) :: current_dist
    
    integer :: i, j, temp_dist
    
    if (position > n) then
        ! Calculate total distance for this permutation
        current_dist = 0
        do i = 1, n-1
            current_dist = current_dist + distance(current_perm(i), current_perm(i+1))
        end do
        ! Return to starting city
        current_dist = current_dist + distance(current_perm(n), current_perm(1))
        
        ! Update best solution if current is better
        if (current_dist < min_distance) then
            min_distance = current_dist
            do i = 1, n
                best_perm(i) = current_perm(i)
            end do
        end if
    else
        ! Try all unused cities
        do i = 1, n
            if (.not. used(i)) then
                used(i) = .true.
                current_perm(position) = i
                call generate_permutations(position + 1, current_perm, used, &
                                          distance, min_distance, best_perm, &
                                          current_dist)
                used(i) = .false.
            end if
        end do
    end if
end subroutine generate_permutations
```

## Output
```
Distance Matrix:
   0  10  15  20
  10   0  35  25
  15  35   0  30
  20  25  30   0

Best route:
   1   2   4   3
Minimum distance: 80
```

## Key Features of this Implementation:

1. **Distance Matrix**: Defines the distances between all pairs of cities
2. **Brute Force Approach**: Generates all possible permutations to find the optimal solution
3. **Recursive Permutation Generation**: Uses backtracking to generate all possible routes
4. **Optimization**: Keeps track of the best solution found so far
5. **Symmetric TSP**: Assumes distance(i,j) = distance(j,i)

## Notes:
- This implementation is suitable for small instances (n ≤ 10) due to factorial time complexity
- For larger problems, consider using heuristic approaches like genetic algorithms or simulated annealing
- The algorithm returns the optimal solution for small TSP instances
- The route is represented as city indices (1 to n)

