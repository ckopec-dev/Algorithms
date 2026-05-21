# Vogel's Approximation Method in Fortran

Here's a complete Fortran implementation of Vogel's Approximation Method for solving transportation problems:

```fortran
program vogel_approximation_method
    implicit none
    integer, parameter :: n = 4, m = 5  ! Number of supply and demand points
    integer :: supply(n), demand(m)
    real :: cost(n,m), allocation(n,m)
    real :: total_cost
    integer :: i, j, min_row, min_col, min_diff
    real :: row_diff(n), col_diff(m)
    real :: min_diff_val, min_cost
    logical :: supply_done(n), demand_done(m)
    
    ! Initialize arrays
    do i = 1, n
        do j = 1, m
            allocation(i,j) = 0.0
        end do
        supply_done(i) = .false.
    end do
    
    do j = 1, m
        demand_done(j) = .false.
    end do
    
    ! Define supply and demand values
    supply = [30, 20, 10, 20]  ! Supply quantities
    demand = [15, 15, 10, 15, 25]  ! Demand quantities
    
    ! Define cost matrix
    cost = reshape([8, 6, 10, 9, 12,
                    7, 5, 11, 8, 14,
                    9, 12, 13, 7, 10,
                    14, 9, 16, 11, 12], [n,m])
    
    ! Display initial data
    write(*,*) 'Transportation Problem'
    write(*,*) '======================'
    write(*,*) 'Supply:', supply
    write(*,*) 'Demand:', demand
    write(*,*) 'Cost Matrix:'
    do i = 1, n
        write(*,*) (cost(i,j), j=1,m)
    end do
    
    ! Main Vogel's Approximation Method loop
    do while (.not.(all(supply_done) .and. all(demand_done)))
        
        ! Calculate row differences
        do i = 1, n
            if (.not.supply_done(i)) then
                min_cost = 1.0e30
                do j = 1, m
                    if (.not.demand_done(j) .and. cost(i,j) < min_cost) then
                        min_cost = cost(i,j)
                    end if
                end do
                row_diff(i) = min_cost
            else
                row_diff(i) = 0.0
            end if
        end do
        
        ! Calculate column differences
        do j = 1, m
            if (.not.demand_done(j)) then
                min_cost = 1.0e30
                do i = 1, n
                    if (.not.supply_done(i) .and. cost(i,j) < min_cost) then
                        min_cost = cost(i,j)
                    end if
                end do
                col_diff(j) = min_cost
            else
                col_diff(j) = 0.0
            end if
        end do
        
        ! Find maximum difference
        min_diff_val = -1.0e30
        min_row = 0
        min_col = 0
        
        do i = 1, n
            if (.not.supply_done(i)) then
                do j = 1, m
                    if (.not.demand_done(j)) then
                        if (row_diff(i) > min_diff_val) then
                            min_diff_val = row_diff(i)
                            min_row = i
                            min_col = j
                        end if
                    end if
                end do
            end if
        end do
        
        do j = 1, m
            if (.not.demand_done(j)) then
                do i = 1, n
                    if (.not.supply_done(i)) then
                        if (col_diff(j) > min_diff_val) then
                            min_diff_val = col_diff(j)
                            min_row = i
                            min_col = j
                        end if
                    end if
                end do
            end if
        end do
        
        ! Make allocation
        if (supply(min_row) >= demand(min_col)) then
            allocation(min_row, min_col) = demand(min_col)
            supply(min_row) = supply(min_row) - demand(min_col)
            demand(min_col) = 0
            demand_done(min_col) = .true.
        else
            allocation(min_row, min_col) = supply(min_row)
            demand(min_col) = demand(min_col) - supply(min_row)
            supply(min_row) = 0
            supply_done(min_row) = .true.
        end if
        
        write(*,*) 'Allocated ', allocation(min_row,min_col), ' units from supply ', min_row, &
                   ' to demand ', min_col
        
    end do
    
    ! Calculate total cost
    total_cost = 0.0
    do i = 1, n
        do j = 1, m
            total_cost = total_cost + allocation(i,j) * cost(i,j)
        end do
    end do
    
    ! Display results
    write(*,*) '======================'
    write(*,*) 'Final Allocation Matrix:'
    do i = 1, n
        write(*,*) (allocation(i,j), j=1,m)
    end do
    write(*,*) 'Total Cost:', total_cost
    
end program vogel_approximation_method
```

## How the Algorithm Works:

1. **Initialization**: Set up supply, demand, and cost matrices
2. **Calculate Differences**: For each row and column, find the difference between the two smallest costs
3. **Find Maximum Difference**: Identify the cell with the largest difference
4. **Make Allocation**: Allocate as much as possible to the cell with maximum difference
5. **Update**: Adjust supply and demand values, mark cells as completed
6. **Repeat**: Continue until all supplies and demands are satisfied

## Key Features:

- **Input Data**: Supply quantities, demand quantities, and cost matrix
- **Difference Calculation**: Computes row and column penalties
- **Optimal Selection**: Chooses cell with maximum penalty
- **Allocation**: Makes efficient allocations to minimize cost
- **Result Display**: Shows final allocation matrix and total cost

This implementation demonstrates the core logic of Vogel's Approximation Method, which is an efficient heuristic for solving transportation problems that typically provides solutions very close to optimal.

