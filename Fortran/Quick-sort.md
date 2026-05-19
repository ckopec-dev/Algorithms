# Quick Sort Algorithm in Fortran

Here's an implementation of the Quick Sort algorithm in Fortran:

```fortran
program quicksort_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n) = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50]
    integer :: i
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Call quick sort
    call quicksort(arr, 1, n)
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''

contains

    subroutine quicksort(arr, low, high)
        implicit none
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: pivot, i, j
        
        if (low < high) then
            ! Partition the array and get pivot index
            pivot = partition(arr, low, high)
            
            ! Recursively sort elements before and after partition
            call quicksort(arr, low, pivot - 1)
            call quicksort(arr, pivot + 1, high)
        end if
    end subroutine quicksort

    integer function partition(arr, low, high)
        implicit none
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: i, j, pivot_value, temp
        
        ! Choose the rightmost element as pivot
        pivot_value = arr(high)
        
        ! Index of smaller element (indicates right position of pivot)
        i = low - 1
        
        do j = low, high - 1
            ! If current element is smaller than or equal to pivot
            if (arr(j) <= pivot_value) then
                i = i + 1
                ! Swap elements
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp
            end if
        end do
        
        ! Place pivot in its correct position
        temp = arr(i + 1)
        arr(i + 1) = arr(high)
        arr(high) = temp
        
        partition = i + 1
    end function partition

end program quicksort_example
```

## Output
```
Original array:
64 34 25 12 22 11 90 88 76 50 

Sorted array:
11 12 22 25 34 50 64 76 88 90 
```

## Key Features of this Implementation:

1. **Recursive Approach**: Uses recursion to sort subarrays
2. **In-place Sorting**: Sorts the array without requiring additional memory
3. **Partitioning**: Uses the last element as pivot for partitioning
4. **Generic Function**: Works with arrays of any size
5. **Proper Fortran Syntax**: Uses modern Fortran features like assumed-shape arrays

## How it Works:

1. **Partition Function**: Rearranges the array so that elements smaller than the pivot are on the left, and larger elements are on the right
2. **Quicksort Function**: Recursively applies the sorting process to subarrays
3. **Base Case**: When `low >= high`, the subarray is already sorted
4. **Pivot Selection**: Uses the last element as the pivot for simplicity

The time complexity is O(n log n) on average and O(n²) in the worst case, with O(log n) space complexity due to recursion.

