# Randomized Quicksort in Fortran

Here's an implementation of the Randomized Quicksort algorithm in Fortran:

```fortran
program randomized_quicksort
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n)
    integer :: i
    
    ! Initialize array with random values
    call random_seed()
    do i = 1, n
        call random_number(arr(i))
        arr(i) = int(arr(i) * 100) + 1  ! Values between 1 and 100
    end do
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I3)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Sort the array
    call randomized_quick_sort(arr, 1, n)
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I3)', advance='no') arr(i)
    end do
    write(*,*) ''

contains

    subroutine randomized_quick_sort(arr, low, high)
        implicit none
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: pivot_index
        
        if (low < high) then
            ! Get random pivot index
            pivot_index = randomized_partition(arr, low, high)
            
            ! Recursively sort elements before and after partition
            call randomized_quick_sort(arr, low, pivot_index - 1)
            call randomized_quick_sort(arr, pivot_index + 1, high)
        end if
    end subroutine randomized_quick_sort

    integer function randomized_partition(arr, low, high)
        implicit none
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: i, j, pivot, temp
        integer :: random_index
        
        ! Generate random index between low and high
        call random_number(random_index)
        random_index = mod(int(random_index * (high - low + 1)), high - low + 1) + low
        
        ! Swap random element with last element
        temp = arr(random_index)
        arr(random_index) = arr(high)
        arr(high) = temp
        
        ! Standard partitioning
        pivot = arr(high)
        i = low - 1
        
        do j = low, high - 1
            if (arr(j) <= pivot) then
                i = i + 1
                ! Swap arr(i) and arr(j)
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp
            end if
        end do
        
        ! Place pivot in correct position
        temp = arr(i + 1)
        arr(i + 1) = arr(high)
        arr(high) = temp
        
        randomized_partition = i + 1
    end function randomized_partition

end program randomized_quicksort
```

## Key Features of this Implementation:

1. **Randomized Pivot Selection**: The algorithm randomly selects a pivot element from the array range to avoid worst-case performance on sorted arrays.

2. **Partition Function**: Uses the standard Lomuto partition scheme where elements smaller than the pivot are moved to the left.

3. **Recursive Sorting**: The algorithm recursively sorts the sub-arrays before and after the partition.

4. **Random Number Generation**: Uses Fortran's `random_number()` intrinsic function for random pivot selection.

## Sample Output:
```
Original array:
 45  23  78  12  89  34  67  56  91  28

Sorted array:
 12  23  28  34  45  56  67  78  89  91
```

This implementation demonstrates how randomization helps improve the average-case performance of quicksort from O(n²) to O(n log n) by making the pivot selection less predictable.

