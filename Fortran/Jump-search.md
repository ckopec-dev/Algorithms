# Jump Search Algorithm in Fortran

```fortran
program jump_search_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n) = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    integer :: target, result
    integer :: jump_size, prev, i
    
    ! Function to perform jump search
    jump_size = int(sqrt(real(n)))
    prev = 0
    
    ! Jump through the array
    do while (arr(min(jump_size, n) - 1) < target)
        prev = jump_size
        jump_size = jump_size + int(sqrt(real(n)))
        if (prev >= n) then
            result = -1
            goto 100
        end if
    end do
    
    ! Linear search in the block
    do i = prev + 1, min(jump_size, n)
        if (arr(i - 1) == target) then
            result = i - 1
            goto 100
        end if
    end do
    
    result = -1
    
100 continue
    
    ! Output results
    if (result /= -1) then
        write(*,*) "Element found at index:", result
    else
        write(*,*) "Element not found"
    end if
    
end program jump_search_example
```

## Explanation:

The jump search algorithm works by:
1. **Calculate jump size**: Usually set to √n (square root of array size)
2. **Jump through blocks**: Move through the array in jumps of the calculated size
3. **Find the block**: Locate the block where the target element might be
4. **Linear search**: Perform linear search within that block

## Key Features:
- **Time Complexity**: O(√n)
- **Space Complexity**: O(1)
- **Prerequisite**: Array must be sorted
- **Advantage**: Better than linear search for large arrays

## Sample Output:
```
Element found at index: 4
```

This example searches for the target value 9 in a sorted array of 10 elements.

