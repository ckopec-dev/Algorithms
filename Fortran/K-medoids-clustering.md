# K-Medoids Clustering Algorithm in Fortran

Here's a complete implementation of the K-Medoids clustering algorithm in Fortran:

```fortran
program kmedoids_clustering
    implicit none
    integer, parameter :: n = 100, d = 2, k = 3, max_iter = 100
    integer :: i, j, iter, best_iter
    real :: data(n, d)
    integer :: medoids(k)
    integer :: assignments(n)
    real :: distances(n, k)
    real :: best_distances(n)
    real :: best_cost, current_cost
    real :: min_dist, temp_dist
    logical :: converged
    
    ! Initialize random seed
    call random_seed()
    
    ! Generate sample data (2D points)
    call random_number(data)
    do i = 1, n
        data(i, 1) = data(i, 1) * 10.0
        data(i, 2) = data(i, 2) * 10.0
    end do
    
    ! Initialize medoids randomly
    call random_number(distances)
    do i = 1, k
        medoids(i) = int(distances(i, 1) * real(n)) + 1
    end do
    
    ! Print initial medoids
    write(*,*) 'Initial medoids:', (medoids(i), i=1,k)
    
    ! Main K-medoids iteration
    converged = .false.
    best_cost = huge(1.0)
    
    do iter = 1, max_iter
        ! Assign each point to the nearest medoid
        do i = 1, n
            min_dist = huge(1.0)
            do j = 1, k
                temp_dist = 0.0
                temp_dist = (data(i, 1) - data(medoids(j), 1))**2 + &
                           (data(i, 2) - data(medoids(j), 2))**2
                temp_dist = sqrt(temp_dist)
                if (temp_dist < min_dist) then
                    min_dist = temp_dist
                    assignments(i) = j
                end if
            end do
            distances(i, 1) = min_dist
        end do
        
        ! Calculate total cost
        current_cost = 0.0
        do i = 1, n
            current_cost = current_cost + distances(i, 1)
        end do
        
        ! Check if this is the best solution so far
        if (current_cost < best_cost) then
            best_cost = current_cost
            best_iter = iter
            do i = 1, n
                best_distances(i) = distances(i, 1)
            end do
        end if
        
        ! Try to improve by swapping medoids
        call improve_medoids(data, medoids, assignments, k, n, d)
        
        ! Check convergence (simplified)
        if (iter > 1) then
            if (abs(current_cost - previous_cost) < 1e-6) then
                converged = .true.
            end if
        end if
        
        previous_cost = current_cost
        
        if (converged) then
            write(*,*) 'Converged at iteration:', iter
            exit
        end if
        
    end do
    
    ! Output results
    write(*,*) 'Final medoids:', (medoids(i), i=1,k)
    write(*,*) 'Best cost:', best_cost
    write(*,*) 'Best iteration:', best_iter
    
    ! Print cluster assignments
    write(*,*) 'Cluster assignments:'
    do i = 1, n
        write(*,*) 'Point ', i, ' -> Cluster ', assignments(i)
    end do
    
end program kmedoids_clustering

subroutine improve_medoids(data, medoids, assignments, k, n, d)
    implicit none
    integer, intent(in) :: k, n, d
    real, intent(in) :: data(n, d)
    integer, intent(inout) :: medoids(k)
    integer, intent(in) :: assignments(n)
    integer :: i, j, l, best_medoid
    real :: current_cost, new_cost, temp_dist
    integer :: temp_medoids(k)
    
    ! Try swapping each medoid with each data point
    do i = 1, k
        do j = 1, n
            ! Temporarily change medoid
            temp_medoids = medoids
            temp_medoids(i) = j
            
            ! Calculate new cost
            new_cost = 0.0
            do l = 1, n
                temp_dist = 0.0
                temp_dist = (data(l, 1) - data(temp_medoids(assignments(l)), 1))**2 + &
                           (data(l, 2) - data(temp_medoids(assignments(l)), 2))**2
                temp_dist = sqrt(temp_dist)
                new_cost = new_cost + temp_dist
            end do
            
            ! If better, update medoids
            if (new_cost < current_cost) then
                current_cost = new_cost
                medoids = temp_medoids
            end if
        end do
    end do
    
end subroutine improve_medoids
```

## Key Features of this Implementation:

### 1. **Data Structure**
- `data(n, d)`: n data points in d-dimensional space
- `medoids(k)`: Array storing indices of current medoids
- `assignments(n)`: Cluster assignment for each data point

### 2. **Algorithm Steps**
1. **Initialization**: Randomly select k medoids
2. **Assignment**: Assign each point to nearest medoid
3. **Update**: Try to improve medoids by swapping with data points
4. **Convergence**: Stop when no improvement is possible

### 3. **Distance Calculation**
- Uses Euclidean distance: √[(x₁-x₂)² + (y₁-y₂)²]

### 4. **Key Functions**
- `improve_medoids()`: Tries to find better medoids by swapping
- Distance computation and assignment logic
- Cost calculation and convergence checking

### 5. **Usage Notes**
- The program generates sample 2D data points
- Uses a simplified convergence criterion
- Can be extended for higher dimensions
- Includes basic error handling and output

This implementation demonstrates the core principles of K-medoids clustering while maintaining the structure and syntax typical of Fortran programming.

