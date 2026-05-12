# Selection Sort Algorithm in Fortran

Here's an example of the selection sort algorithm implemented in Fortran:

```fortran
program selection_sort_example
    implicit none
    integer, parameter :: n = 6
    integer :: arr(n) = [64, 34, 25, 12, 22, 11]
    integer :: i, j, min_idx, temp
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Selection sort algorithm
    do i = 1, n-1
        ! Find the minimum element in remaining unsorted array
        min_idx = i
        do j = i+1, n
            if (arr(j) < arr(min_idx)) then
                min_idx = j
            end if
        end do
        
        ! Swap the found minimum element with the first element
        if (min_idx /= i) then
            temp = arr(i)
            arr(i) = arr(min_idx)
            arr(min_idx) = temp
        end if
    end do
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
end program selection_sort_example
```

## Output:
```
Original array:
64 34 25 12 22 11 

Sorted array:
11 12 22 25 34 64 
```

## How it works:

1. **Find Minimum**: For each position `i` from 1 to n-1, find the smallest element in the remaining unsorted portion
2. **Swap**: Swap the found minimum element with the element at position `i`
3. **Repeat**: Continue until the entire array is sorted

## Key Features:

- **Time Complexity**: O(n²) in all cases
- **Space Complexity**: O(1) - sorts in-place
- **Stability**: Not stable (doesn't preserve relative order of equal elements)
- **Number of Swaps**: At most n-1 swaps

The algorithm works by repeatedly selecting the smallest remaining element and placing it in its correct position in the sorted portion of the array.

