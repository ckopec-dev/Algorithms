# Decision Tree Learning Algorithm in Fortran

Here's an implementation of a simple decision tree learning algorithm in Fortran:

```fortran
program decision_tree_learning
    implicit none
    integer, parameter :: max_features = 10
    integer, parameter :: max_examples = 100
    integer, parameter :: max_depth = 5
    
    ! Data structures for decision tree
    type node
        integer :: feature_index
        real :: threshold
        integer :: is_leaf
        integer :: class_label
        type(node), pointer :: left_child
        type(node), pointer :: right_child
        character(len=20) :: feature_name
    end type node
    
    ! Sample dataset
    real, dimension(max_examples, max_features) :: dataset
    integer, dimension(max_examples) :: labels
    integer :: num_examples, num_features
    
    ! Initialize sample data
    call initialize_dataset()
    
    ! Build decision tree
    call build_decision_tree()
    
    ! Print tree structure
    call print_tree()
    
contains
    
    subroutine initialize_dataset()
        ! Sample dataset: 4 features, 10 examples
        ! Features: [age, income, credit_score, education_level]
        ! Labels: 0 (no), 1 (yes)
        
        num_examples = 10
        num_features = 4
        
        ! Sample data
        dataset(1, :) = [25.0, 50000.0, 700.0, 1.0]  ! age, income, credit, education
        dataset(2, :) = [35.0, 80000.0, 750.0, 2.0]
        dataset(3, :) = [45.0, 120000.0, 800.0, 3.0]
        dataset(4, :) = [28.0, 45000.0, 650.0, 1.0]
        dataset(5, :) = [38.0, 90000.0, 780.0, 2.0]
        dataset(6, :) = [52.0, 150000.0, 820.0, 3.0]
        dataset(7, :) = [22.0, 35000.0, 600.0, 1.0]
        dataset(8, :) = [41.0, 100000.0, 770.0, 2.0]
        dataset(9, :) = [33.0, 70000.0, 720.0, 2.0]
        dataset(10, :) = [48.0, 130000.0, 810.0, 3.0]
        
        ! Labels (1 = approve loan, 0 = reject)
        labels = [1, 1, 1, 0, 1, 1, 0, 1, 0, 1]
    end subroutine initialize_dataset
    
    subroutine build_decision_tree()
        ! Main decision tree building function
        type(node), pointer :: root
        integer :: i
        
        ! Create root node
        allocate(root)
        root%feature_index = -1
        root%threshold = 0.0
        root%is_leaf = 0
        root%class_label = -1
        root%left_child => null()
        root%right_child => null()
        root%feature_name = 'root'
        
        ! Build tree recursively
        call build_tree_recursive(root, 1, num_examples, 0)
        
        ! Print tree structure
        write(*,*) 'Decision Tree Built Successfully'
        write(*,*) 'Root node: ', root%feature_name
    end subroutine build_decision_tree
    
    subroutine build_tree_recursive(current_node, start_idx, end_idx, depth)
        implicit none
        type(node), pointer :: current_node
        integer, intent(in) :: start_idx, end_idx, depth
        integer :: i, j, best_feature, best_threshold
        integer :: left_count, right_count
        real :: best_gain, gain
        integer :: left_labels(100), right_labels(100)
        integer :: left_label_count, right_label_count
        
        ! Check stopping conditions
        if (depth > max_depth) then
            call set_leaf_node(current_node, start_idx, end_idx)
            return
        end if
        
        ! Find best split
        call find_best_split(start_idx, end_idx, best_feature, best_threshold, best_gain)
        
        ! If no good split found, make leaf
        if (best_gain <= 0.0) then
            call set_leaf_node(current_node, start_idx, end_idx)
            return
        end if
        
        ! Set current node attributes
        current_node%feature_index = best_feature
        current_node%threshold = best_threshold
        current_node%feature_name = 'feature_' // trim(adjustl(char(best_feature+48)))
        
        ! Split data
        left_count = 0
        right_count = 0
        
        do i = start_idx, end_idx
            if (dataset(i, best_feature) <= best_threshold) then
                left_count = left_count + 1
                left_labels(left_count) = labels(i)
            else
                right_count = right_count + 1
                right_labels(right_count) = labels(i)
            end if
        end do
        
        ! Create left child
        allocate(current_node%left_child)
        current_node%left_child%feature_index = -1
        current_node%left_child%threshold = 0.0
        current_node%left_child%is_leaf = 0
        current_node%left_child%class_label = -1
        current_node%left_child%left_child => null()
        current_node%left_child%right_child => null()
        current_node%left_child%feature_name = 'left'
        
        ! Create right child
        allocate(current_node%right_child)
        current_node%right_child%feature_index = -1
        current_node%right_child%threshold = 0.0
        current_node%right_child%is_leaf = 0
        current_node%right_child%class_label = -1
        current_node%right_child%left_child => null()
        current_node%right_child%right_child => null()
        current_node%right_child%feature_name = 'right'
        
        ! Recursively build subtrees
        call build_tree_recursive(current_node%left_child, start_idx, start_idx + left_count - 1, depth + 1)
        call build_tree_recursive(current_node%right_child, start_idx + left_count, end_idx, depth + 1)
    end subroutine build_tree_recursive
    
    subroutine find_best_split(start_idx, end_idx, best_feature, best_threshold, best_gain)
        implicit none
        integer, intent(in) :: start_idx, end_idx
        integer, intent(out) :: best_feature, best_threshold
        real, intent(out) :: best_gain
        integer :: i, j, k
        real :: current_gain, max_gain
        real :: threshold_val
        integer :: left_count, right_count
        integer :: left_labels(100), right_labels(100)
        integer :: left_label_count, right_label_count
        
        best_gain = -1.0
        best_feature = -1
        best_threshold = 0.0
        
        ! Try each feature
        do i = 1, num_features
            ! Try each possible threshold for this feature
            do j = start_idx, end_idx
                threshold_val = dataset(j, i)
                left_count = 0
                right_count = 0
                
                ! Count samples on each side
                do k = start_idx, end_idx
                    if (dataset(k, i) <= threshold_val) then
                        left_count = left_count + 1
                    else
                        right_count = right_count + 1
                    end if
                end do
                
                ! Calculate information gain
                current_gain = calculate_information_gain(start_idx, end_idx, i, threshold_val)
                
                if (current_gain > best_gain) then
                    best_gain = current_gain
                    best_feature = i
                    best_threshold = threshold_val
                end if
            end do
        end do
    end subroutine find_best_split
    
    function calculate_information_gain(start_idx, end_idx, feature_idx, threshold) result(gain)
        implicit none
        integer, intent(in) :: start_idx, end_idx, feature_idx
        real, intent(in) :: threshold
        real :: gain
        integer :: i, left_count, right_count
        integer :: left_labels(100), right_labels(100)
        integer :: left_label_count, right_label_count
        real :: entropy_before, entropy_after, total_samples
        
        total_samples = real(end_idx - start_idx + 1)
        
        ! Calculate entropy before split
        entropy_before = calculate_entropy(start_idx, end_idx)
        
        ! Split data
        left_count = 0
        right_count = 0
        
        do i = start_idx, end_idx
            if (dataset(i, feature_idx) <= threshold) then
                left_count = left_count + 1
            else
                right_count = right_count + 1
            end if
        end do
        
        ! Calculate entropy after split
        entropy_after = 0.0
        
        if (left_count > 0) then
            entropy_after = entropy_after + (real(left_count) / total_samples) * calculate_entropy(start_idx, start_idx + left_count - 1)
        end if
        
        if (right_count > 0) then
            entropy_after = entropy_after + (real(right_count) / total_samples) * calculate_entropy(start_idx + left_count, end_idx)
        end if
        
        gain = entropy_before - entropy_after
    end function calculate_information_gain
    
    function calculate_entropy(start_idx, end_idx) result(entropy)
        implicit none
        integer, intent(in) :: start_idx, end_idx
        real :: entropy
        integer :: i, count_0, count_1
        real :: p0, p1, total_samples
        
        count_0 = 0
        count_1 = 0
        total_samples = real(end_idx - start_idx + 1)
        
        ! Count labels
        do i = start_idx, end_idx
            if (labels(i) == 0) then
                count_0 = count_0 + 1
            else
                count_1 = count_1 + 1
            end if
        end do
        
        ! Calculate entropy
        entropy = 0.0
        
        if (count_0 > 0) then
            p0 = real(count_0) / total_samples
            entropy = entropy - p0 * log2(p0)
        end if
        
        if (count_1 > 0) then
            p1 = real(count_1) / total_samples
            entropy = entropy - p1 * log2(p1)
        end if
    end function calculate_entropy
    
    subroutine set_leaf_node(node, start_idx, end_idx)
        implicit none
        type(node), pointer :: node
        integer, intent(in) :: start_idx, end_idx
        integer :: i, count_0, count_1
        integer :: majority_class
        
        node%is_leaf = 1
        
        ! Count labels
        count_0 = 0
        count_1 = 0
        
        do i = start_idx, end_idx
            if (labels(i) == 0) then
                count_0 = count_0 + 1
            else
                count_1 = count_1 + 1
            end if
        end do
        
        ! Set majority class
        if (count_0 > count_1) then
            node%class_label = 0
        else
            node%class_label = 1
        end if
    end subroutine set_leaf_node
    
    subroutine print_tree()
        write(*,*) 'Decision Tree Structure:'
        write(*,*) '========================'
        write(*,*) 'This is a simplified decision tree implementation'
        write(*,*) 'In a full implementation, this would recursively print the tree structure'
        write(*,*) 'with feature splits and leaf nodes containing class predictions'
    end subroutine print_tree
    
    function log2(x) result(res)
        implicit none
        real, intent(in) :: x
        real :: res
        res = log(x) / log(2.0)
    end function log2
    
end program decision_tree_learning
```

## Key Features of this Implementation

This Fortran decision tree implementation includes:

1. **Data Structures**:
   - `node` type for tree nodes with feature index, threshold, and child pointers
   - Sample dataset with 10 examples and 4 features

2. **Core Algorithms**:
   - Recursive tree building with stopping conditions
   - Best split selection using information gain
   - Entropy calculation for measuring impurity
   - Leaf node creation based on majority class

3. **Key Functions**:
   - `build_tree_recursive()` - Main recursive tree building function
   - `find_best_split()` - Finds optimal feature and threshold for splitting
   - `calculate_information_gain()` - Computes information gain for splits
   - `calculate_entropy()` - Calculates entropy for node impurity

4. **Features**:
   - Maximum depth control to prevent overfitting
   - Automatic leaf node creation when no good splits exist
   - Basic tree printing functionality

## Usage Notes

- This is a simplified implementation for educational purposes
- In practice, you would want to add more sophisticated features like:
  - Cross-validation for parameter tuning
  - Pruning to prevent overfitting
  - Support for continuous and categorical features
  - More robust entropy calculations
  - Better memory management for large datasets

The program demonstrates the core concepts of decision tree learning while maintaining the syntax and structure typical of Fortran programming.

