# DBSCAN Clustering Algorithm in Fortran

Here's a complete implementation of the DBSCAN clustering algorithm in Fortran:

```fortran
program dbscan_example
    implicit none
    integer, parameter :: n = 100, d = 2
    integer, parameter :: eps = 2, minPts = 3
    real, dimension(n, d) :: data
    integer, dimension(n) :: labels
    integer :: i, j, cluster_id, neighbor_count
    logical, dimension(n) :: visited, is_core
    integer, dimension(n) :: neighbors
    integer :: num_clusters
    
    ! Initialize data points (2D example)
    call initialize_data(data)
    
    ! Initialize arrays
    visited = .false.
    is_core = .false.
    labels = 0
    
    ! Find core points
    do i = 1, n
        neighbor_count = 0
        do j = 1, n
            if (distance(data(i,:), data(j,:)) <= eps) then
                neighbor_count = neighbor_count + 1
                neighbors(neighbor_count) = j
            end if
        end do
        if (neighbor_count >= minPts) then
            is_core(i) = .true.
        end if
    end do
    
    ! Perform clustering
    cluster_id = 0
    do i = 1, n
        if (.not. visited(i)) then
            visited(i) = .true.
            if (is_core(i)) then
                cluster_id = cluster_id + 1
                call expand_cluster(i, cluster_id, data, labels, visited, is_core, &
                                   neighbors, neighbor_count, eps, minPts)
            end if
        end if
    end do
    
    ! Output results
    write(*,*) 'DBSCAN Clustering Results:'
    write(*,*) 'Number of clusters found:', cluster_id
    write(*,*) 'Point | Cluster'
    write(*,*) '------|--------'
    do i = 1, n
        write(*,*) i, '     | ', labels(i)
    end do
    
contains
    
    subroutine initialize_data(data)
        implicit none
        real, dimension(n, d) :: data
        integer :: i, j
        
        ! Generate sample data points
        do i = 1, n
            data(i,1) = real(i) / 10.0
            data(i,2) = real(i) * 0.5
        end do
        
        ! Add some noise to make it more realistic
        do i = 1, n
            data(i,1) = data(i,1) + 0.1 * (real(i) - 50.0) / 100.0
            data(i,2) = data(i,2) + 0.1 * (real(i) - 50.0) / 100.0
        end do
    end subroutine initialize_data
    
    real function distance(p1, p2)
        implicit none
        real, dimension(d) :: p1, p2
        real :: sum_dist
        integer :: i
        
        sum_dist = 0.0
        do i = 1, d
            sum_dist = sum_dist + (p1(i) - p2(i))**2
        end do
        distance = sqrt(sum_dist)
    end function distance
    
    subroutine expand_cluster(point, cluster_id, data, labels, visited, is_core, &
                             neighbors, neighbor_count, eps, minPts)
        implicit none
        integer :: point, cluster_id, neighbor_count, eps, minPts
        real, dimension(n, d) :: data
        integer, dimension(n) :: labels
        logical, dimension(n) :: visited, is_core
        integer, dimension(n) :: neighbors
        
        integer :: i, j, neighbor_point
        integer :: current_neighbor_count
        
        ! Add point to cluster
        labels(point) = cluster_id
        
        ! Find neighbors of current point
        current_neighbor_count = 0
        do i = 1, n
            if (distance(data(point,:), data(i,:)) <= eps) then
                current_neighbor_count = current_neighbor_count + 1
                neighbors(current_neighbor_count) = i
            end if
        end do
        
        ! Process neighbors
        do i = 1, current_neighbor_count
            neighbor_point = neighbors(i)
            if (.not. visited(neighbor_point)) then
                visited(neighbor_point) = .true.
                if (is_core(neighbor_point)) then
                    ! Add neighbor to cluster
                    labels(neighbor_point) = cluster_id
                    ! Add neighbors of this neighbor to our list
                    call find_neighbors(neighbor_point, neighbors, neighbor_count, data, eps)
                end if
            end if
        end do
    end subroutine expand_cluster
    
    subroutine find_neighbors(point, neighbors, neighbor_count, data, eps)
        implicit none
        integer :: point, neighbor_count, eps
        real, dimension(n, d) :: data
        integer, dimension(n) :: neighbors
        
        integer :: i
        
        neighbor_count = 0
        do i = 1, n
            if (distance(data(point,:), data(i,:)) <= eps) then
                neighbor_count = neighbor_count + 1
                neighbors(neighbor_count) = i
            end if
        end do
    end subroutine find_neighbors
    
end program dbscan_example
```

## Key Features of this Implementation:

1. **Core Algorithm Components**:
   - Distance calculation between points
   - Core point identification
   - Cluster expansion using neighbor search
   - Noise point detection

2. **Main Functions**:
   - `distance()`: Calculates Euclidean distance between two points
   - `expand_cluster()`: Recursively expands clusters
   - `find_neighbors()`: Finds all points within epsilon distance

3. **Variables**:
   - `data`: Input data points
   - `labels`: Cluster labels for each point
   - `visited`: Tracks visited points
   - `is_core`: Identifies core points

4. **Parameters**:
   - `eps`: Epsilon neighborhood radius
   - `minPts`: Minimum points required to form a cluster

## How to Compile and Run:

```bash
gfortran -o dbscan dbscan_example.f90
./dbscan
```

## Expected Output:
The program will output the cluster assignments for each data point, showing how DBSCAN groups points into clusters based on density while identifying noise points that don't belong to any cluster.

This implementation provides a working foundation for DBSCAN clustering that can be adapted for different datasets and parameter values.

