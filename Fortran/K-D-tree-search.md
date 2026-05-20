# K-D Tree Search Algorithm in Fortran

Here's a complete implementation of a K-D tree search algorithm in Fortran:

```fortran
program kd_tree_example
    implicit none
    integer, parameter :: max_dim = 3
    integer, parameter :: max_points = 1000
    integer :: i, j, n_points
    real :: points(max_points, max_dim)
    real :: query_point(max_dim)
    integer :: nearest_index
    real :: nearest_distance
    
    ! Sample data points (3D)
    n_points = 10
    points(1, :) = [1.0, 2.0, 3.0]
    points(2, :) = [4.0, 5.0, 6.0]
    points(3, :) = [7.0, 8.0, 9.0]
    points(4, :) = [2.0, 3.0, 1.0]
    points(5, :) = [5.0, 6.0, 4.0]
    points(6, :) = [8.0, 9.0, 7.0]
    points(7, :) = [3.0, 1.0, 2.0]
    points(8, :) = [6.0, 4.0, 5.0]
    points(9, :) = [9.0, 7.0, 8.0]
    points(10, :) = [1.5, 2.5, 3.5]
    
    ! Query point
    query_point = [2.0, 3.0, 2.0]
    
    ! Perform k-d tree search
    call kd_tree_search(points, n_points, query_point, nearest_index, nearest_distance)
    
    ! Output results
    write(*,*) 'Query point: ', query_point
    write(*,*) 'Nearest point index: ', nearest_index
    write(*,*) 'Nearest point: ', points(nearest_index, :)
    write(*,*) 'Distance: ', nearest_distance
    
end program kd_tree_example

! K-D Tree node structure
type kd_node
    integer :: axis
    real :: threshold
    integer :: left_child
    integer :: right_child
    integer :: point_index
    logical :: is_leaf
end type kd_node

! K-D Tree structure
type kd_tree
    type(kd_node) :: nodes(max_points)
    integer :: root_index
    integer :: num_nodes
end type kd_tree

! Function to calculate Euclidean distance
real function euclidean_distance(point1, point2, dim)
    implicit none
    integer, intent(in) :: dim
    real, intent(in) :: point1(dim), point2(dim)
    integer :: i
    real :: sum_sq
    
    sum_sq = 0.0
    do i = 1, dim
        sum_sq = sum_sq + (point1(i) - point2(i))**2
    end do
    euclidean_distance = sqrt(sum_sq)
end function euclidean_distance

! Function to find median of a sorted array
integer function find_median(arr, start, end)
    implicit none
    integer, intent(in) :: start, end
    integer, intent(in) :: arr(end)
    find_median = (start + end) / 2
end function find_median

! Recursive function to build K-D tree
integer function build_kd_tree(points, indices, start, end, depth, tree)
    implicit none
    real, intent(in) :: points(:, :)
    integer, intent(in) :: indices(:), start, end, depth
    type(kd_tree), intent(inout) :: tree
    integer :: median, axis, i, j, temp_index
    integer :: left_start, left_end, right_start, right_end
    
    if (start > end) then
        build_kd_tree = -1
        return
    end if
    
    ! Determine splitting axis (cycle through dimensions)
    axis = mod(depth, size(points, 2))
    
    ! Sort indices by the current axis
    call quick_sort(indices, start, end, points, axis)
    
    ! Find median
    median = find_median(indices, start, end)
    
    ! Create new node
    tree%num_nodes = tree%num_nodes + 1
    build_kd_tree = tree%num_nodes
    
    tree%nodes(build_kd_tree)%axis = axis
    tree%nodes(build_kd_tree)%threshold = points(indices(median), axis)
    tree%nodes(build_kd_tree)%point_index = indices(median)
    tree%nodes(build_kd_tree)%is_leaf = .false.
    
    ! Recursively build left and right subtrees
    left_start = start
    left_end = median - 1
    right_start = median + 1
    right_end = end
    
    tree%nodes(build_kd_tree)%left_child = build_kd_tree(points, indices, left_start, left_end, depth + 1, tree)
    tree%nodes(build_kd_tree)%right_child = build_kd_tree(points, indices, right_start, right_end, depth + 1, tree)
    
    ! Mark as leaf if no children
    if (tree%nodes(build_kd_tree)%left_child == -1 .and. tree%nodes(build_kd_tree)%right_child == -1) then
        tree%nodes(build_kd_tree)%is_leaf = .true.
    end if
end function build_kd_tree

! Quick sort function for sorting by axis
subroutine quick_sort(indices, start, end, points, axis)
    implicit none
    integer, intent(in) :: start, end, axis
    integer, intent(inout) :: indices(:)
    real, intent(in) :: points(:, :)
    integer :: i, j, pivot
    real :: pivot_val
    
    if (start < end) then
        pivot = partition(indices, start, end, points, axis)
        call quick_sort(indices, start, pivot - 1, points, axis)
        call quick_sort(indices, pivot + 1, end, points, axis)
    end if
end subroutine quick_sort

! Partition function for quick sort
integer function partition(indices, start, end, points, axis)
    implicit none
    integer, intent(in) :: start, end, axis
    integer, intent(inout) :: indices(:)
    real, intent(in) :: points(:, :)
    integer :: i, j
    real :: pivot_val, temp_val
    
    pivot_val = points(indices(end), axis)
    i = start - 1
    
    do j = start, end - 1
        if (points(indices(j), axis) <= pivot_val) then
            i = i + 1
            call swap(indices(i), indices(j))
        end if
    end do
    
    call swap(indices(i + 1), indices(end))
    partition = i + 1
end function partition

! Swap two integers
subroutine swap(a, b)
    implicit none
    integer, intent(inout) :: a, b
    integer :: temp
    temp = a
    a = b
    b = temp
end subroutine swap

! K-D Tree search function
subroutine kd_tree_search(points, n_points, query_point, nearest_index, nearest_distance)
    implicit none
    real, intent(in) :: points(:, :)
    integer, intent(in) :: n_points
    real, intent(in) :: query_point(:)
    integer, intent(out) :: nearest_index
    real, intent(out) :: nearest_distance
    
    type(kd_tree) :: tree
    integer :: indices(max_points)
    integer :: i
    
    ! Initialize indices
    do i = 1, n_points
        indices(i) = i
    end do
    
    ! Build the tree
    tree%num_nodes = 0
    tree%root_index = build_kd_tree(points, indices, 1, n_points, 0, tree)
    
    ! Perform search
    call search_node(tree, points, query_point, tree%root_index, nearest_index, nearest_distance)
end subroutine kd_tree_search

! Recursive search in K-D tree
subroutine search_node(tree, points, query_point, node_index, nearest_index, nearest_distance)
    implicit none
    type(kd_tree), intent(in) :: tree
    real, intent(in) :: points(:, :), query_point(:)
    integer, intent(in) :: node_index
    integer, intent(out) :: nearest_index
    real, intent(out) :: nearest_distance
    
    integer :: axis, left_child, right_child
    real :: distance, temp_distance
    integer :: temp_index
    
    ! Base case - leaf node
    if (tree%nodes(node_index)%is_leaf) then
        nearest_index = tree%nodes(node_index)%point_index
        nearest_distance = euclidean_distance(query_point, points(nearest_index, :), size(query_point))
        return
    end if
    
    axis = tree%nodes(node_index)%axis
    left_child = tree%nodes(node_index)%left_child
    right_child = tree%nodes(node_index)%right_child
    
    ! Determine which subtree to search first
    if (query_point(axis) <= tree%nodes(node_index)%threshold) then
        call search_node(tree, points, query_point, left_child, nearest_index, nearest_distance)
        ! Check if we need to search the other subtree
        if (abs(query_point(axis) - tree%nodes(node_index)%threshold) < nearest_distance) then
            call search_node(tree, points, query_point, right_child, temp_index, temp_distance)
            if (temp_distance < nearest_distance) then
                nearest_index = temp_index
                nearest_distance = temp_distance
            end if
        end if
    else
        call search_node(tree, points, query_point, right_child, nearest_index, nearest_distance)
        ! Check if we need to search the other subtree
        if (abs(query_point(axis) - tree%nodes(node_index)%threshold) < nearest_distance) then
            call search_node(tree, points, query_point, left_child, temp_index, temp_distance)
            if (temp_distance < nearest_distance) then
                nearest_index = temp_index
                nearest_distance = temp_distance
            end if
        end if
    end if
end subroutine search_node
```

## Key Features of this Implementation:

1. **K-D Tree Structure**: Uses a node-based approach with splitting axes
2. **Build Function**: Recursively builds the tree by sorting points along different axes
3. **Search Function**: Performs efficient nearest neighbor search with pruning
4. **Distance Calculation**: Uses Euclidean distance metric
5. **Optimization**: Implements branch-and-bound pruning to avoid unnecessary searches

## How it Works:

1. **Tree Construction**: Points are recursively partitioned along different dimensions
2. **Search Process**: Starting from the root, the algorithm traverses the tree, choosing the appropriate subtree based on the query point's position
3. **Pruning**: When a subtree is found to be farther than the current best distance, it's pruned to improve efficiency
4. **Result**: Returns the index and distance of the nearest neighbor

This implementation demonstrates the core concepts of K-D tree search in Fortran, providing an efficient way to find nearest neighbors in multi-dimensional space.

