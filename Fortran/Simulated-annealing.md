# Simulated Annealing Algorithm in Fortran

Here's a complete implementation of the Simulated Annealing algorithm in Fortran to solve the Traveling Salesman Problem (TSP):

```fortran
program simulated_annealing_tsp
    implicit none
    integer, parameter :: n_cities = 20
    integer, parameter :: max_iterations = 10000
    integer, parameter :: max_temperature = 1000
    real, parameter :: cooling_rate = 0.95
    real, parameter :: min_temperature = 0.01
    integer :: i, j, k, current_iteration
    real :: temperature, best_distance, current_distance, new_distance
    real :: delta, probability, random_val
    integer :: best_route(n_cities), current_route(n_cities), new_route(n_cities)
    real :: distance_matrix(n_cities, n_cities)
    real :: x_coords(n_cities), y_coords(n_cities)
    integer :: seed = 12345
    
    ! Initialize random number generator
    call random_seed()
    call random_seed put([seed])
    
    ! Generate random city coordinates
    do i = 1, n_cities
        call random_number(x_coords(i))
        call random_number(y_coords(i))
    end do
    
    ! Calculate distance matrix
    do i = 1, n_cities
        do j = 1, n_cities
            if (i == j) then
                distance_matrix(i,j) = 0.0
            else
                distance_matrix(i,j) = sqrt((x_coords(i)-x_coords(j))**2 + &
                                          (y_coords(i)-y_coords(j))**2)
            end if
        end do
    end do
    
    ! Initialize routes
    do i = 1, n_cities
        current_route(i) = i
        best_route(i) = i
    end do
    
    ! Calculate initial distance
    current_distance = calculate_total_distance(current_route, distance_matrix)
    best_distance = current_distance
    
    ! Main simulated annealing loop
    temperature = real(max_temperature)
    current_iteration = 0
    
    do while (temperature > min_temperature .and. current_iteration < max_iterations)
        ! Generate new candidate solution by swapping two random cities
        call generate_neighbor(current_route, new_route, n_cities)
        
        ! Calculate distance of new solution
        new_distance = calculate_total_distance(new_route, distance_matrix)
        
        ! Calculate change in distance
        delta = new_distance - current_distance
        
        ! Accept or reject the new solution
        if (delta < 0.0) then
            ! Always accept better solutions
            current_route = new_route
            current_distance = new_distance
            if (current_distance < best_distance) then
                best_distance = current_distance
                best_route = current_route
            end if
        else
            ! Accept worse solutions with probability
            call random_number(random_val)
            probability = exp(-delta / temperature)
            if (random_val < probability) then
                current_route = new_route
                current_distance = new_distance
            end if
        end if
        
        ! Cool down the temperature
        temperature = temperature * cooling_rate
        current_iteration = current_iteration + 1
    end do
    
    ! Output results
    write(*,*) 'Simulated Annealing Results:'
    write(*,*) 'Best distance found:', best_distance
    write(*,*) 'Number of iterations:', current_iteration
    write(*,*) 'Final route:'
    do i = 1, n_cities
        write(*,'(I0,1X)', advance='no') best_route(i)
    end do
    write(*,*) ''
    
    ! Calculate final distance for verification
    write(*,*) 'Verification of final distance:'
    write(*,*) 'Distance:', calculate_total_distance(best_route, distance_matrix)
    
contains
    
    ! Function to calculate total distance of a route
    real function calculate_total_distance(route, dist_matrix)
        integer, intent(in) :: route(n_cities)
        real, intent(in) :: dist_matrix(n_cities, n_cities)
        integer :: i
        real :: total
        total = 0.0
        do i = 1, n_cities-1
            total = total + dist_matrix(route(i), route(i+1))
        end do
        ! Return to starting city
        total = total + dist_matrix(route(n_cities), route(1))
        calculate_total_distance = total
    end function calculate_total_distance
    
    ! Procedure to generate neighbor solution by swapping two cities
    subroutine generate_neighbor(old_route, new_route, n)
        integer, intent(in) :: old_route(n)
        integer, intent(out) :: new_route(n)
        integer, intent(in) :: n
        integer :: i, j, temp
        ! Copy old route
        new_route = old_route
        ! Generate two random indices
        call random_number(i)
        i = int(i * real(n)) + 1
        call random_number(j)
        j = int(j * real(n)) + 1
        ! Swap two cities (avoid swapping same position)
        if (i /= j) then
            temp = new_route(i)
            new_route(i) = new_route(j)
            new_route(j) = temp
        end if
    end subroutine generate_neighbor
    
end program simulated_annealing_tsp
```

## Key Components Explained:

### 1. **Main Algorithm Structure**
- **Temperature Control**: Starts with high temperature and gradually decreases
- **Cooling Schedule**: Uses geometric cooling with rate of 0.95
- **Iteration Limit**: Maximum number of iterations to prevent infinite loops

### 2. **Solution Generation**
- **Neighbor Generation**: Swaps two random cities in the current route
- **Acceptance Criteria**: 
  - Always accept better solutions
  - Accept worse solutions with probability based on temperature and energy difference

### 3. **Key Functions**
- `calculate_total_distance()`: Computes total travel distance
- `generate_neighbor()`: Creates new candidate solution

### 4. **Parameters**
- `n_cities`: Number of cities in the TSP
- `max_iterations`: Maximum number of iterations
- `max_temperature`: Initial temperature
- `cooling_rate`: Rate at which temperature decreases
- `min_temperature`: Minimum temperature threshold

### 5. **Algorithm Steps**
1. Initialize random solution and temperature
2. Generate neighbor solution
3. Calculate energy difference (distance change)
4. Accept or reject based on Metropolis criterion
5. Cool down temperature
6. Repeat until stopping criteria met

This implementation demonstrates the core principles of simulated annealing: exploring the solution space while gradually focusing on better solutions, and allowing occasional uphill moves to escape local optima.

