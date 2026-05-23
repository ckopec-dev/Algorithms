# Pigeonhole Sort in Fortran

Here's an implementation of the Pigeonhole Sort algorithm in Fortran:

```fortran
program pigeonhole_sort_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n) = [5, 2, 8, 1, 9, 3, 7, 4, 6, 0]
    integer :: i, min_val, max_val, range, hole_size
    integer, allocatable :: hole(:)
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Find minimum and maximum values
    min_val = arr(1)
    max_val = arr(1)
    do i = 2, n
        if (arr(i) < min_val) min_val = arr(i)
        if (arr(i) > max_val) max_val = arr(i)
    end do
    
    ! Calculate range and allocate hole array
    range = max_val - min_val + 1
    allocate(hole(range))
    
    ! Initialize hole array to 0
    hole = 0
    
    ! Place elements in holes
    do i = 1, n
        hole(arr(i) - min_val + 1) = hole(arr(i) - min_val + 1) + 1
    end do
    
    ! Reconstruct sorted array from holes
    i = 1
    do while (i <= n)
        do j = 1, range
            if (hole(j) > 0) then
                arr(i) = j + min_val - 1
                hole(j) = hole(j) - 1
                i = i + 1
            end if
        end do
    end do
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Clean up
    deallocate(hole)
    
end program pigeonhole_sort_example
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Holes**: Allocate an array (hole) of size (max - min + 1)
3. **Place Elements**: For each element, place it in the corresponding hole
4. **Reconstruct**: Extract elements from holes in order to form the sorted array

## Key Features:

- **Time Complexity**: O(n + k) where n is the number of elements and k is the range
- **Space Complexity**: O(k) for the hole array
- **Best For**: Sorting integers with a small range of values
- **Stable**: Maintains relative order of equal elements

## Sample Output:
```
Original array:
5 2 8 1 9 3 7 4 6 0 

Sorted array:
0 1 2 3 4 5 6 7 8 9 
```

This implementation demonstrates the pigeonhole sort algorithm's efficiency when dealing with integers that have a limited range of possible values.

