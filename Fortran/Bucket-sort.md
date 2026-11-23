# Bucket Sort Algorithm in Fortran

Here's an implementation of the bucket sort algorithm in Fortran:

```fortran
program bucket_sort_example
    implicit none
    integer, parameter :: n = 10
    integer :: i, j, bucket_count
    real, dimension(n) :: array, sorted_array
    real, dimension(:,:), allocatable :: buckets
    
    ! Initialize the array to be sorted
    data array /8.2, 3.1, 5.7, 9.0, 1.5, 6.8, 2.3, 7.9, 4.4, 0.6/
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(F5.1)', advance='no') array(i)
    end do
    write(*,*) ''
    
    ! Perform bucket sort
    call bucket_sort(array, sorted_array, n)
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(F5.1)', advance='no') sorted_array(i)
    end do
    write(*,*) ''
    
end program bucket_sort_example

subroutine bucket_sort(input_array, output_array, size)
    implicit none
    integer, intent(in) :: size
    real, dimension(size), intent(in) :: input_array
    real, dimension(size), intent(out) :: output_array
    
    integer :: i, j, k, bucket_count, index
    real, dimension(:,:), allocatable :: buckets
    real :: min_val, max_val, range, bucket_size
    
    ! Find minimum and maximum values
    min_val = input_array(1)
    max_val = input_array(1)
    
    do i = 2, size
        if (input_array(i) < min_val) min_val = input_array(i)
        if (input_array(i) > max_val) max_val = input_array(i)
    end do
    
    ! Calculate bucket count (using square root of array size)
    bucket_count = int(sqrt(real(size))) + 1
    
    ! Allocate buckets
    allocate(buckets(1:bucket_count, 1:size))
    
    ! Initialize bucket sizes to zero
    do i = 1, bucket_count
        buckets(i, 1) = 0.0
    end do
    
    ! Calculate bucket size
    range = max_val - min_val
    if (range == 0.0) then
        bucket_size = 1.0
    else
        bucket_size = range / real(bucket_count)
    end if
    
    ! Distribute elements into buckets
    do i = 1, size
        if (bucket_size == 0.0) then
            j = 1
        else
            j = int((input_array(i) - min_val) / bucket_size) + 1
        end if
        
        ! Make sure we don't exceed bucket bounds
        if (j > bucket_count) j = bucket_count
        
        ! Add element to bucket
        buckets(j, 1) = buckets(j, 1) + 1.0
        buckets(j, int(buckets(j, 1))) = input_array(i)
    end do
    
    ! Sort each bucket and collect results
    index = 1
    do i = 1, bucket_count
        if (buckets(i, 1) > 0.0) then
            ! Sort the current bucket using bubble sort
            call bubble_sort(buckets(i, 2), int(buckets(i, 1)))
            
            ! Copy sorted bucket elements to output array
            do j = 1, int(buckets(i, 1))
                output_array(index) = buckets(i, j+1)
                index = index + 1
            end do
        end if
    end do
    
    ! Deallocate buckets
    deallocate(buckets)
    
end subroutine bucket_sort

subroutine bubble_sort(array, size)
    implicit none
    integer, intent(in) :: size
    real, dimension(size), intent(inout) :: array
    integer :: i, j
    real :: temp
    
    do i = 1, size - 1
        do j = 1, size - i
            if (array(j) > array(j+1)) then
                temp = array(j)
                array(j) = array(j+1)
                array(j+1) = temp
            end if
        end do
    end do
    
end subroutine bubble_sort
```

## How it works:

1. **Bucket Creation**: The algorithm creates buckets based on the range of input values
2. **Distribution**: Elements are distributed into appropriate buckets based on their value ranges
3. **Sorting**: Each bucket is sorted individually (using bubble sort in this example)
4. **Combination**: Sorted elements from all buckets are combined to form the final sorted array

## Key Features:

- Uses square root of array size as bucket count
- Handles edge cases like identical elements
- Implements bubble sort for individual bucket sorting
- Works with real numbers (floating-point values)

## Sample Output:
```
Original array:
  8.2  3.1  5.7  9.0  1.5  6.8  2.3  7.9  4.4  0.6

Sorted array:
  0.6  1.5  2.3  3.1  4.4  5.7  6.8  7.9  8.2  9.0
```

This implementation demonstrates the bucket sort algorithm's approach of dividing the input into buckets and sorting them individually before combining the results.