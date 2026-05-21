# Artificial Bee Colony Algorithm in Fortran

Here's a complete implementation of the Artificial Bee Colony (ABC) algorithm in Fortran for solving optimization problems:

```fortran
program artificial_bee_colony
    implicit none
    integer, parameter :: n = 100          ! Number of bees
    integer, parameter :: dim = 10         ! Dimension of the problem
    integer, parameter :: max_iter = 1000  ! Maximum iterations
    integer, parameter :: limit = 100      ! Limit for abandonment
    real, parameter :: lb = -5.0           ! Lower bound
    real, parameter :: ub = 5.0            ! Upper bound
    real, parameter :: epsilon = 1.0e-6    ! Convergence threshold
    
    ! Variables
    real :: food_source(n, dim)           ! Food sources (solutions)
    real :: fitness(n)                    ! Fitness values
    real :: trial(n)                      ! Trial counts
    real :: best_solution(dim)            ! Best solution found
    real :: best_fitness                  ! Best fitness value
    real :: new_solution(dim)             ! New solution
    real :: obj_val                       ! Objective function value
    integer :: i, j, k, iter, limit_count
    real :: r, diff, best_val
    
    ! Initialize random number generator
    call random_seed()
    
    ! Initialize food sources
    call initialize_food_sources()
    
    ! Main ABC loop
    do iter = 1, max_iter
        ! Employed bee phase
        call employed_bee_phase()
        
        ! Onlooker bee phase
        call onlooker_bee_phase()
        
        ! Scout bee phase
        call scout_bee_phase()
        
        ! Update best solution
        call update_best_solution()
        
        ! Check convergence
        if (iter > 1) then
            diff = abs(best_fitness - best_val)
            if (diff < epsilon) then
                write(*,*) 'Converged at iteration ', iter
                exit
            end if
        end if
        best_val = best_fitness
        
        if (mod(iter, 100) == 0) then
            write(*,*) 'Iteration ', iter, ': Best fitness = ', best_fitness
        end if
    end do
    
    ! Output results
    write(*,*) 'Final Results:'
    write(*,*) 'Best fitness: ', best_fitness
    write(*,*) 'Best solution:'
    do i = 1, dim
        write(*,*) 'x(', i, ') = ', best_solution(i)
    end do
    
contains
    
    ! Initialize food sources
    subroutine initialize_food_sources()
        integer :: i, j
        do i = 1, n
            do j = 1, dim
                call random_number(r)
                food_source(i, j) = lb + r * (ub - lb)
            end do
            fitness(i) = calculate_fitness(food_source(i, :))
        end do
        
        ! Initialize trial counts
        trial(:) = 0
        
        ! Initialize best solution
        best_fitness = huge(1.0)
        do i = 1, n
            if (fitness(i) < best_fitness) then
                best_fitness = fitness(i)
                best_solution(:) = food_source(i, :)
            end if
        end do
    end subroutine initialize_food_sources
    
    ! Employed bee phase
    subroutine employed_bee_phase()
        integer :: i, j, k, param
        real :: phi
        
        do i = 1, n
            ! Select a random parameter
            call random_number(r)
            param = int(r * dim) + 1
            
            ! Select a random neighbor
            call random_number(r)
            j = int(r * n) + 1
            do while (j == i)
                call random_number(r)
                j = int(r * n) + 1
            end do
            
            ! Generate new solution
            call random_number(r)
            phi = -1.0 + 2.0 * r
            
            new_solution(:) = food_source(i, :)
            new_solution(param) = food_source(i, param) + phi * &
                                 (food_source(i, param) - food_source(j, param))
            
            ! Ensure bounds
            if (new_solution(param) < lb) new_solution(param) = lb
            if (new_solution(param) > ub) new_solution(param) = ub
            
            ! Evaluate new solution
            obj_val = calculate_fitness(new_solution(:))
            
            ! Apply greedy selection
            if (obj_val < fitness(i)) then
                food_source(i, :) = new_solution(:)
                fitness(i) = obj_val
                trial(i) = 0
            else
                trial(i) = trial(i) + 1
            end if
        end do
    end subroutine employed_bee_phase
    
    ! Onlooker bee phase
    subroutine onlooker_bee_phase()
        integer :: i, j, k, selected
        real :: total_fitness, prob, cumulative_prob, r
        real, allocatable :: probabilities(:)
        
        ! Calculate probabilities based on fitness
        allocate(probabilities(n))
        total_fitness = 0.0
        
        do i = 1, n
            total_fitness = total_fitness + 1.0 / (fitness(i) + 1.0e-10)
        end do
        
        cumulative_prob = 0.0
        do i = 1, n
            prob = (1.0 / (fitness(i) + 1.0e-10)) / total_fitness
            cumulative_prob = cumulative_prob + prob
            probabilities(i) = cumulative_prob
        end do
        
        ! Select food sources
        do i = 1, n
            call random_number(r)
            do j = 1, n
                if (r <= probabilities(j)) then
                    selected = j
                    exit
                end if
            end do
            
            ! Generate new solution using selected source
            call generate_new_solution(selected, new_solution)
            
            ! Evaluate new solution
            obj_val = calculate_fitness(new_solution(:))
            
            ! Apply greedy selection
            if (obj_val < fitness(selected)) then
                food_source(selected, :) = new_solution(:)
                fitness(selected) = obj_val
                trial(selected) = 0
            else
                trial(selected) = trial(selected) + 1
            end if
        end do
        
        deallocate(probabilities)
    end subroutine onlooker_bee_phase
    
    ! Scout bee phase
    subroutine scout_bee_phase()
        integer :: i, j
        
        do i = 1, n
            if (trial(i) >= limit) then
                ! Replace abandoned food source
                do j = 1, dim
                    call random_number(r)
                    food_source(i, j) = lb + r * (ub - lb)
                end do
                fitness(i) = calculate_fitness(food_source(i, :))
                trial(i) = 0
            end if
        end do
    end subroutine scout_bee_phase
    
    ! Generate new solution for onlooker bee
    subroutine generate_new_solution(source_index, new_sol)
        integer, intent(in) :: source_index
        real, intent(out) :: new_sol(dim)
        integer :: param, j
        real :: phi
        
        ! Select a random parameter
        call random_number(r)
        param = int(r * dim) + 1
        
        ! Select a random neighbor
        call random_number(r)
        j = int(r * n) + 1
        do while (j == source_index)
            call random_number(r)
            j = int(r * n) + 1
        end do
        
        ! Generate new solution
        call random_number(r)
        phi = -1.0 + 2.0 * r
        
        new_sol(:) = food_source(source_index, :)
        new_sol(param) = food_source(source_index, param) + phi * &
                         (food_source(source_index, param) - food_source(j, param))
        
        ! Ensure bounds
        if (new_sol(param) < lb) new_sol(param) = lb
        if (new_sol(param) > ub) new_sol(param) = ub
    end subroutine generate_new_solution
    
    ! Calculate fitness (example: minimize sphere function)
    real function calculate_fitness(solution)
        real, intent(in) :: solution(dim)
        real :: sum_val
        integer :: i
        
        sum_val = 0.0
        do i = 1, dim
            sum_val = sum_val + solution(i) * solution(i)
        end do
        calculate_fitness = sum_val
    end function calculate_fitness
    
    ! Update best solution
    subroutine update_best_solution()
        integer :: i
        do i = 1, n
            if (fitness(i) < best_fitness) then
                best_fitness = fitness(i)
                best_solution(:) = food_source(i, :)
            end if
        end do
    end subroutine update_best_solution
    
end program artificial_bee_colony
```

## Key Features of this Implementation:

### 1. **Core Components**:
- **Food Sources**: Population of candidate solutions
- **Employed Bees**: Explore neighborhood solutions
- **Onlooker Bees**: Select promising solutions probabilistically
- **Scout Bees**: Replace abandoned solutions

### 2. **Algorithm Phases**:
- **Employed Bee Phase**: Each bee explores its neighborhood
- **Onlooker Bee Phase**: Bees select solutions based on fitness
- **Scout Bee Phase**: Abandoned solutions are replaced

### 3. **Parameters**:
- `n`: Number of bees (population size)
- `dim`: Problem dimension
- `max_iter`: Maximum iterations
- `limit`: Maximum trials before abandonment

### 4. **Fitness Function**:
Uses the sphere function as an example (minimize sum of squares)

### 5. **Key Features**:
- Proper bounds checking
- Random number generation
- Convergence monitoring
- Detailed output during execution

## Compilation and Execution:

```bash
gfortran -o abc_algorithm artificial_bee_colony.f90
./abc_algorithm
```

This implementation demonstrates the core principles of the Artificial Bee Colony algorithm while maintaining clean, readable Fortran code structure.

