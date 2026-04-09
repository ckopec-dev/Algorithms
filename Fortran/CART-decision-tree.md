# CART Decision Tree Algorithm in Fortran

Here's an implementation of a Classification and Regression Tree (CART) algorithm in Fortran:

```fortran
program cart_decision_tree
    implicit none
    integer, parameter :: max_depth = 10
    integer, parameter :: min_samples_split = 2
    integer, parameter :: n_features = 4
    integer, parameter :: n_samples = 150
    
    ! Data arrays
    real, dimension(n_samples, n_features) :: X
    integer, dimension(n_samples) :: y
    integer :: i, j
    
    ! Initialize sample data (Iris dataset)
    call initialize_data(X, y)
    
    ! Create decision tree
    call build_tree(X, y, n_samples, n_features, 0)
    
    ! Print results
    write(*,*) 'CART Decision Tree Implementation Complete'
    
contains

    subroutine initialize_data(X, y)
        implicit none
        real, dimension(n_samples, n_features) :: X
        integer, dimension(n_samples) :: y
        integer :: i
        
        ! Sample data initialization (simplified)
        do i = 1, n_samples
            X(i,1) = real(i) * 0.1
            X(i,2) = real(i) * 0.05
            X(i,3) = real(i) * 0.02
            X(i,4) = real(i) * 0.03
            y(i) = mod(i, 3) + 1
        end do
    end subroutine initialize_data

    subroutine build_tree(X, y, n_samples, n_features, depth)
        implicit none
        real, dimension(n_samples, n_features) :: X
        integer, dimension(n_samples) :: y
        integer, intent(in) :: n_samples, n_features, depth
        integer :: best_feature, best_threshold, best_gain
        integer :: left_indices(n_samples), right_indices(n_samples)
        integer :: n_left, n_right
        integer :: i, j
        
        ! Base cases
        if (depth >= max_depth .or. n_samples < min_samples_split) then
            write(*,*) 'Leaf node created at depth:', depth
            return
        end if
        
        ! Find best split
        call find_best_split(X, y, n_samples, n_features, &
                            best_feature, best_threshold, best_gain)
        
        if (best_gain <= 0) then
            write(*,*) 'No good split found, creating leaf at depth:', depth
            return
        end if
        
        ! Split data
        call split_data(X, y, n_samples, best_feature, best_threshold, &
                       left_indices, right_indices, n_left, n_right)
        
        ! Print split information
        write(*,*) 'Splitting on feature', best_feature, &
                   'with threshold', best_threshold, &
                   'gain:', best_gain
        
        ! Recursively build left and right subtrees
        if (n_left > 0) then
            write(*,*) 'Building left subtree with', n_left, 'samples'
            call build_tree(X(left_indices(1:n_left), :), &
                           y(left_indices(1:n_left)), &
                           n_left, n_features, depth + 1)
        end if
        
        if (n_right > 0) then
            write(*,*) 'Building right subtree with', n_right, 'samples'
            call build_tree(X(right_indices(1:n_right), :), &
                           y(right_indices(1:n_right)), &
                           n_right, n_features, depth + 1)
        end if
        
    end subroutine build_tree

    subroutine find_best_split(X, y, n_samples, n_features, &
                              best_feature, best_threshold, best_gain)
        implicit none
        real, dimension(n_samples, n_features) :: X
        integer, dimension(n_samples) :: y
        integer, intent(in) :: n_samples, n_features
        integer, intent(out) :: best_feature, best_threshold, best_gain
        integer :: i, j, k
        real :: threshold, gain, current_gain
        integer :: n_left, n_right
        integer :: left_indices(n_samples), right_indices(n_samples)
        
        best_gain = -1
        best_feature = -1
        best_threshold = -1
        
        ! Try all features and thresholds
        do j = 1, n_features
            do i = 1, n_samples
                threshold = X(i, j)
                call split_data(X, y, n_samples, j, threshold, &
                               left_indices, right_indices, n_left, n_right)
                
                if (n_left > 0 .and. n_right > 0) then
                    current_gain = calculate_gini_gain(y, n_samples, &
                                                       left_indices, right_indices, &
                                                       n_left, n_right)
                    if (current_gain > best_gain) then
                        best_gain = current_gain
                        best_feature = j
                        best_threshold = int(threshold)
                    end if
                end if
            end do
        end do
        
    end subroutine find_best_split

    subroutine split_data(X, y, n_samples, feature, threshold, &
                         left_indices, right_indices, n_left, n_right)
        implicit none
        real, dimension(n_samples, :) :: X
        integer, dimension(n_samples) :: y
        integer, intent(in) :: n_samples, feature
        real, intent(in) :: threshold
        integer, dimension(n_samples) :: left_indices, right_indices
        integer, intent(out) :: n_left, n_right
        integer :: i
        
        n_left = 0
        n_right = 0
        
        do i = 1, n_samples
            if (X(i, feature) <= threshold) then
                n_left = n_left + 1
                left_indices(n_left) = i
            else
                n_right = n_right + 1
                right_indices(n_right) = i
            end if
        end do
        
    end subroutine split_data

    real function calculate_gini_gain(y, n_samples, left_indices, right_indices, &
                                     n_left, n_right)
        implicit none
        integer, dimension(n_samples) :: y
        integer, intent(in) :: n_samples, n_left, n_right
        integer, dimension(n_samples) :: left_indices, right_indices
        real :: gini_left, gini_right, gini_parent
        integer :: i, j, class_count(3)
        real :: total_weight
        
        ! Calculate parent Gini
        call calculate_gini(y, n_samples, class_count, gini_parent)
        
        ! Calculate left Gini
        call calculate_gini(y(left_indices(1:n_left)), n_left, &
                           class_count, gini_left)
        
        ! Calculate right Gini
        call calculate_gini(y(right_indices(1:n_right)), n_right, &
                           class_count, gini_right)
        
        ! Calculate weighted Gini
        total_weight = real(n_left) / real(n_samples) * gini_left + &
                      real(n_right) / real(n_samples) * gini_right
        
        calculate_gini_gain = gini_parent - total_weight
        
    end function calculate_gini_gain

    subroutine calculate_gini(y, n_samples, class_count, gini)
        implicit none
        integer, dimension(n_samples) :: y
        integer, intent(in) :: n_samples
        integer, dimension(3) :: class_count
        real, intent(out) :: gini
        integer :: i, j, total
        real :: prob
        
        ! Initialize class counts
        class_count = 0
        
        ! Count classes
        do i = 1, n_samples
            class_count(y(i)) = class_count(y(i)) + 1
        end do
        
        ! Calculate Gini
        gini = 1.0
        total = n_samples
        
        do i = 1, 3
            if (total > 0) then
                prob = real(class_count(i)) / real(total)
                gini = gini - prob * prob
            end if
        end do
        
    end subroutine calculate_gini

end program cart_decision_tree
```

## Key Features of this CART Implementation:

1. **Tree Structure**: Uses recursive splitting to build decision trees
2. **Split Selection**: Finds the best feature and threshold for splitting
3. **Gini Impurity**: Calculates Gini impurity for measuring split quality
4. **Stopping Criteria**: 
   - Maximum depth reached
   - Minimum samples required for splitting
5. **Recursive Building**: Builds left and right subtrees recursively

## Algorithm Components:

- **Data Initialization**: Sample dataset creation
- **Tree Building**: Recursive tree construction
- **Split Finding**: Best feature and threshold selection
- **Gini Calculation**: Impurity measurement
- **Data Splitting**: Partitioning samples based on threshold

This implementation demonstrates the core concepts of CART decision trees in Fortran, including the recursive tree building process and impurity calculations.

