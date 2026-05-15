# Naïve Bayes Classifier in Fortran

Here's a complete implementation of a Naïve Bayes classifier in Fortran:

```fortran
program naive_bayes_classifier
    implicit none
    integer, parameter :: n_features = 4
    integer, parameter :: n_classes = 3
    integer, parameter :: n_training_samples = 150
    integer, parameter :: n_test_samples = 30
    
    ! Data arrays
    real, dimension(n_training_samples, n_features) :: X_train
    integer, dimension(n_training_samples) :: y_train
    real, dimension(n_test_samples, n_features) :: X_test
    integer, dimension(n_test_samples) :: y_test
    
    ! Model parameters
    real, dimension(n_classes) :: class_priors
    real, dimension(n_classes, n_features) :: feature_means
    real, dimension(n_classes, n_features) :: feature_stds
    integer, dimension(n_classes) :: class_counts
    
    ! Results
    integer, dimension(n_test_samples) :: predictions
    integer :: i, j, k, correct_predictions
    real :: probability, max_prob
    
    ! Training data (Iris dataset example)
    ! Features: sepal_length, sepal_width, petal_length, petal_width
    ! Classes: 0=Setosa, 1=Versicolor, 2=Virginica
    
    ! Training data
    data X_train / &
        5.1, 3.5, 1.4, 0.2, &
        4.9, 3.0, 1.4, 0.2, &
        4.7, 3.2, 1.3, 0.2, &
        4.6, 3.1, 1.5, 0.2, &
        5.0, 3.6, 1.4, 0.2, &
        5.4, 3.9, 1.7, 0.4, &
        4.6, 3.4, 1.4, 0.3, &
        5.0, 3.4, 1.5, 0.2, &
        4.4, 2.9, 1.4, 0.2, &
        4.9, 3.1, 1.5, 0.1, &
        5.4, 3.7, 1.5, 0.2, &
        4.8, 3.4, 1.6, 0.2, &
        4.8, 3.0, 1.4, 0.1, &
        4.3, 3.0, 1.1, 0.1, &
        5.0, 3.3, 1.4, 0.2, &
        7.0, 3.2, 4.7, 1.4, &
        6.4, 3.2, 4.5, 1.5, &
        6.9, 3.1, 4.9, 1.5, &
        5.5, 2.3, 4.0, 1.3, &
        6.5, 2.8, 4.6, 1.5, &
        5.7, 2.8, 4.5, 1.3, &
        6.3, 3.3, 4.7, 1.6, &
        4.9, 2.4, 3.3, 1.0, &
        6.6, 3.0, 4.4, 1.4, &
        5.2, 2.7, 3.9, 1.4, &
        5.0, 2.0, 3.5, 1.0, &
        5.9, 3.0, 5.1, 1.8, &
        6.0, 2.2, 5.0, 1.5, &
        6.1, 2.9, 4.7, 1.4, &
        5.6, 2.9, 3.6, 1.3, &
        6.7, 3.1, 4.4, 1.4, &
        5.6, 3.0, 4.5, 1.5, &
        5.8, 2.7, 4.1, 1.0, &
        6.2, 2.2, 4.5, 1.5, &
        5.6, 2.5, 3.9, 1.1, &
        5.9, 3.2, 4.8, 1.8, &
        6.1, 2.8, 4.0, 1.3, &
        6.3, 2.5, 4.9, 1.5, &
        6.1, 2.8, 4.7, 1.2, &
        6.4, 2.9, 4.3, 1.3, &
        6.6, 3.0, 4.4, 1.4, &
        6.8, 2.8, 4.8, 1.4, &
        6.7, 3.0, 5.0, 1.7, &
        6.0, 2.9, 4.5, 1.5, &
        5.7, 2.6, 3.5, 1.0, &
        5.5, 2.4, 3.8, 1.1, &
        5.5, 2.4, 3.7, 1.0, &
        5.8, 2.7, 3.9, 1.2, &
        6.0, 2.7, 5.1, 1.6, &
        5.4, 3.0, 4.5, 1.5, &
        6.0, 3.4, 4.5, 1.6, &
        6.7, 3.1, 4.7, 1.5, &
        6.3, 2.3, 4.4, 1.3, &
        5.6, 3.0, 4.1, 1.3, &
        5.5, 2.5, 4.0, 1.3, &
        5.5, 2.6, 4.4, 1.2, &
        6.1, 3.0, 4.6, 1.4, &
        5.8, 2.6, 4.0, 1.2, &
        5.0, 2.3, 3.3, 1.0, &
        5.6, 2.7, 4.2, 1.3, &
        5.7, 3.0, 4.2, 1.2, &
        5.7, 2.9, 4.2, 1.3, &
        6.2, 2.9, 4.3, 1.3, &
        5.1, 2.5, 3.0, 1.1, &
        5.7, 2.8, 4.1, 1.3, &
        6.3, 3.3, 6.0, 2.5, &
        5.8, 2.7, 5.1, 1.9, &
        7.1, 3.0, 5.9, 2.1, &
        6.3, 2.9, 5.6, 1.8, &
        6.5, 3.0, 5.8, 2.2, &
        7.6, 3.0, 6.6, 2.1, &
        4.9, 2.5, 4.5, 1.7, &
        7.3, 2.9, 6.3, 1.8, &
        6.7, 2.5, 5.8, 1.8, &
        7.2, 3.6, 6.1, 2.5, &
        6.5, 3.2, 5.1, 2.0, &
        6.4, 2.7, 5.3, 1.9, &
        6.8, 3.0, 5.5, 2.1, &
        5.7, 2.5, 5.0, 2.0, &
        5.8, 2.8, 5.1, 2.4, &
        6.4, 3.2, 5.3, 2.3, &
        6.5, 3.0, 5.5, 1.8, &
        7.7, 3.8, 6.7, 2.2, &
        7.7, 2.6, 6.9, 2.3, &
        6.0, 2.2, 5.0, 1.5, &
        6.9, 3.2, 5.7, 2.3, &
        5.6, 2.8, 4.9, 2.0, &
        7.7, 2.8, 6.7, 2.0, &
        6.3, 2.7, 4.9, 1.8, &
        6.7, 3.3, 5.7, 2.1, &
        7.2, 3.2, 6.0, 1.8, &
        6.2, 2.8, 4.8, 1.8, &
        6.1, 3.0, 4.9, 1.8, &
        6.4, 2.8, 5.6, 2.1, &
        7.2, 3.0, 5.8, 1.6, &
        7.4, 2.8, 6.1, 1.9, &
        7.9, 3.8, 6.4, 2.0, &
        6.4, 2.8, 5.6, 2.2, &
        6.3, 2.8, 5.1, 1.5, &
        6.1, 2.6, 5.6, 1.4, &
        7.7, 3.0, 6.1, 2.3, &
        6.3, 3.4, 5.6, 2.4, &
        6.4, 3.1, 5.5, 1.8, &
        6.0, 3.0, 4.8, 1.8, &
        6.9, 3.1, 5.4, 2.1, &
        6.7, 3.1, 5.6, 2.4, &
        6.9, 3.1, 5.1, 2.3, &
        5.8, 2.7, 5.1, 1.9, &
        6.8, 3.2, 5.9, 2.3, &
        6.7, 3.3, 5.7, 2.5, &
        6.7, 3.0, 5.2, 2.3, &
        6.3, 2.5, 5.0, 1.9, &
        6.5, 3.0, 5.2, 2.0, &
        6.2, 3.4, 5.4, 2.3, &
        5.9, 3.0, 5.1, 1.8 /
    
    ! Training labels (0=Setosa, 1=Versicolor, 2=Virginica)
    data y_train / &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
        0, 0, 0, 0, 0, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, &
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2 /
    
    ! Test data
    data X_test / &
        5.1, 3.5, 1.4, 0.2, &
        4.9, 3.0, 1.4, 0.2, &
        4.7, 3.2, 1.3, 0.2, &
        4.6, 3.1, 1.5, 0.2, &
        5.0, 3.6, 1.4, 0.2, &
        5.4, 3.9, 1.7, 0.4, &
        4.6, 3.4, 1.4, 0.3, &
        5.0, 3.4, 1.5, 0.2, &
        4.4, 2.9, 1.4, 0.2, &
        4.9, 3.1, 1.5, 0.1, &
        5.4, 3.7, 1.5, 0.2, &
        4.8, 3.4, 1.6, 0.2, &
        4.8, 3.0, 1.4, 0.1, &
        4.3, 3.0, 1.1, 0.1, &
        5.0, 3.3, 1.4, 0.2, &
        7.0, 3.2, 4.7, 1.4, &
        6.4, 3.2, 4.5, 1.5, &
        6.9, 3.1, 4.9, 1.5, &
        5.5, 2.3, 4.0, 1.3, &
        6.5, 2.8, 4.6, 1.5, &
        5.7, 2.8, 4.5, 1.3, &
        6.3, 3.3, 4.7, 1.6, &
        4.9, 2.4, 3.3, 1.0, &
        6.6, 3.0, 4.4, 1.4, &
        5.2, 2.7, 3.9, 1.4, &
        5.0, 2.0, 3.5, 1.0, &
        5.9, 3.0, 5.1, 1.8, &
        6.0, 2.2, 5.0, 1.5, &
        6.1, 2.9, 4.7, 1.4 /
    
    ! Test labels
    data y_test / &
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
        0, 0, 0, 0, 0, 1, 1, 1, 1, 1, &
        1, 1, 1, 1, 1, 2, 2, 2, 2, 2 /
    
    ! Initialize class counts
    class_counts = 0
    
    ! Calculate class priors and feature statistics
    call calculate_statistics(X_train, y_train, class_counts, &
                             class_priors, feature_means, feature_stds)
    
    ! Make predictions
    do i = 1, n_test_samples
        predictions(i) = predict(X_test(i,:), class_priors, &
                               feature_means, feature_stds, class_counts)
    end do
    
    ! Calculate accuracy
    correct_predictions = 0
    do i = 1, n_test_samples
        if (predictions(i) == y_test(i)) then
            correct_predictions = correct_predictions + 1
        end if
    end do
    
    ! Output results
    write(*,*) 'Naïve Bayes Classifier Results'
    write(*,*) '=============================='
    write(*,*) 'Training samples:', n_training_samples
    write(*,*) 'Test samples:', n_test_samples
    write(*,*) 'Accuracy:', real(correct_predictions)/real(n_test_samples)*100.0, '%'
    write(*,*) 'Predictions vs Actual:'
    
    do i = 1, n_test_samples
        write(*,*) 'Sample ', i, ': Predicted = ', predictions(i), &
                   ', Actual = ', y_test(i)
    end do
    
end program

subroutine calculate_statistics(X, y, class_counts, class_priors, &
                               feature_means, feature_stds)
    implicit none
    real, intent(in) :: X(n_training_samples, n_features)
    integer, intent(in) :: y(n_training_samples)
    integer, intent(inout) :: class_counts(n_classes)
    real, intent(out) :: class_priors(n_classes)
    real, intent(out) :: feature_means(n_classes, n_features)
    real, intent(out) :: feature_stds(n_classes, n_features)
    
    integer :: i, j, k
    real :: sum_mean(n_classes, n_features)
    real :: sum_var(n_classes, n_features)
    real :: temp
    
    ! Initialize arrays
    class_priors = 0.0
    sum_mean = 0.0
    sum_var = 0.0
    
    ! Count samples per class and calculate sums for means
    do i = 1, n_training_samples
        k = y(i) + 1  ! Convert to 1-based indexing
        class_counts(k) = class_counts(k) + 1
        do j = 1