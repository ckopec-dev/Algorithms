# Random Forest Implementation in Fortran

Here's a simplified example of a Random Forest algorithm implemented in Fortran. This example demonstrates the core concepts of ensemble learning with decision trees.

```fortran
program random_forest_example
    implicit none
    integer, parameter :: n_samples = 1000
    integer, parameter :: n_features = 4
    integer, parameter :: n_trees = 10
    integer, parameter :: max_depth = 5
    integer, parameter :: min_samples_split = 2
    
    ! Data arrays
    real, dimension(n_samples, n_features) :: X
    integer, dimension(n_samples) :: y
    real, dimension(n_samples, n_features) :: X_train
    integer, dimension(n_samples) :: y_train
    integer, dimension(n_samples) :: X_indices
    
    ! Random Forest structure
    integer, dimension(n_trees) :: tree_predictions
    integer :: i, j, k, n_train
    
    ! Initialize random number generator
    call random_seed()
    
    ! Generate sample data (for demonstration)
    call generate_sample_data(X, y, n_samples, n_features)
    
    ! Split data into training and testing sets
    n_train = n_samples * 0.8
    call split_data(X, y, X_train, y_train, n_train, n_samples, n_features)
    
    ! Train Random Forest
    call train_random_forest(X_train, y_train, n_train, n_features, n_trees, &
                            max_depth, min_samples_split)
    
    ! Make predictions
    call predict_random_forest(X, y, n_samples, n_features, n_trees)
    
    write(*,*) 'Random Forest Training Complete'
    write(*,*) 'Number of trees:', n_trees
    write(*,*) 'Sample predictions completed'
    
contains
    
    subroutine generate_sample_data(X, y, n_samples, n_features)
        implicit none
        integer, intent(in) :: n_samples, n_features
        real, intent(out) :: X(n_samples, n_features)
        integer, intent(out) :: y(n_samples)
        integer :: i, j
        
        ! Generate random data
        do i = 1, n_samples
            do j = 1, n_features
                call random_number(X(i,j))
            end do
            ! Generate simple classification labels
            y(i) = mod(i, 3) + 1
        end do
    end subroutine generate_sample_data
    
    subroutine split_data(X, y, X_train, y_train, n_train, n_samples, n_features)
        implicit none
        integer, intent(in) :: n_train, n_samples, n_features
        real, intent(in) :: X(n_samples, n_features)
        integer, intent(in) :: y(n_samples)
        real, intent(out) :: X_train(n_train, n_features)
        integer, intent(out) :: y_train(n_train)
        integer :: i, j, idx
        
        ! Simple random sampling for training set
        do i = 1, n_train
            call random_number(idx)
            idx = int(idx * n_samples) + 1
            X_train(i, :) = X(idx, :)
            y_train(i) = y(idx)
        end do
    end subroutine split_data
    
    subroutine train_random_forest(X_train, y_train, n_train, n_features, &
                                  n_trees, max_depth, min_samples_split)
        implicit none
        integer, intent(in) :: n_train, n_features, n_trees, max_depth, &
                              min_samples_split
        real, intent(in) :: X_train(n_train, n_features)
        integer, intent(in) :: y_train(n_train)
        integer :: i, j, k
        
        ! Create multiple decision trees
        do i = 1, n_trees
            write(*,*) 'Training tree ', i
            ! In a full implementation, this would call tree construction
            ! For this example, we'll just simulate training
            call random_number(j)
            j = mod(j * 100, 3) + 1
        end do
    end subroutine train_random_forest
    
    subroutine predict_random_forest(X, y, n_samples, n_features, n_trees)
        implicit none
        integer, intent(in) :: n_samples, n_features, n_trees
        real, intent(in) :: X(n_samples, n_features)
        integer, intent(inout) :: y(n_samples)
        integer :: i, j, prediction
        
        ! Simple voting mechanism (simplified for example)
        do i = 1, n_samples
            call random_number(prediction)
            prediction = mod(prediction * 3, 3) + 1
            y(i) = prediction
        end do
    end subroutine predict_random_forest
    
end program random_forest_example
```

## Key Components Explained

### 1. **Data Structures**
- `X`: Feature matrix of size (n_samples × n_features)
- `y`: Target labels of size (n_samples)
- `X_train`, `y_train`: Training data subsets

### 2. **Main Algorithm Steps**
1. **Data Preparation**: Split data into training/testing sets
2. **Tree Construction**: Build multiple decision trees
3. **Ensemble Prediction**: Combine predictions from all trees

### 3. **Key Parameters**
- `n_trees`: Number of decision trees in the forest
- `max_depth`: Maximum depth of each tree
- `min_samples_split`: Minimum samples required to split a node

### 4. **Simplified Implementation Notes**
- This is a conceptual example showing the structure
- Actual implementation would include:
  - Decision tree construction algorithms (ID3, C4.5, CART)
  - Feature sampling for each tree
  - Bootstrap sampling for training
  - Voting mechanisms for predictions

### 5. **Compilation**
```bash
gfortran -o random_forest random_forest_example.f90
./random_forest
```

This example demonstrates the basic framework of Random Forest in Fortran, though a complete implementation would require additional subroutines for actual tree construction and more sophisticated ensemble methods.

