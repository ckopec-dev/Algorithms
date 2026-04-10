# Insertion Sort in Fortran

Here's an example of the insertion sort algorithm implemented in Fortran:

```fortran
program insertion_sort_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n)
    integer :: i, j, key
    
    ! Initialize array with random values
    arr = [12, 11, 13, 5, 6, 1, 8, 3, 9, 2]
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Insertion sort algorithm
    do i = 2, n
        key = arr(i)
        j = i - 1
        
        ! Move elements greater than key one position ahead
        do while (j >= 1 .and. arr(j) > key)
            arr(j + 1) = arr(j)
            j = j - 1
        end do
        
        ! Insert key at correct position
        arr(j + 1) = key
    end do
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
end program insertion_sort_example
```

## Output:
```
Original array:
12 11 13 5 6 1 8 3 9 2 

Sorted array:
1 2 3 5 6 8 9 11 12 13 
```

## Algorithm Explanation:

1. **Start from the second element** (index 2) since a single element is already "sorted"
2. **Compare the current element** with elements to its left
3. **Shift larger elements** one position to the right
4. **Insert the current element** at its correct position
5. **Repeat** until all elements are processed

## Key Features:

- **Time Complexity**: O(n²) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in-place
- **Stable**: Maintains relative order of equal elements
- **Adaptive**: Performs well on nearly sorted arrays

The algorithm works by building the final sorted array one element at a time, similar to how you might sort playing cards in your hand.

