# Binary Search Algorithm in Fortran

Here's an example of a binary search algorithm implemented in Fortran:

```fortran
program binary_search_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n) = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    integer :: target, result
    integer :: low, high, mid
    
    ! Target value to search for
    target = 7
    
    ! Call binary search subroutine
    result = binary_search(arr, n, target)
    
    ! Display results
    if (result /= -1) then
        write(*,*) 'Element ', target, ' found at index ', result
    else
        write(*,*) 'Element ', target, ' not found in array'
    end if
    
contains
    
    integer function binary_search(array, size, value)
        implicit none
        integer, intent(in) :: array(:)
        integer, intent(in) :: size
        integer, intent(in) :: value
        integer :: low, high, mid
        
        low = 1
        high = size
        
        do while (low <= high)
            mid = (low + high) / 2
            
            if (array(mid) == value) then
                binary_search = mid
                return
            else if (array(mid) < value) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do
        
        ! Element not found
        binary_search = -1
    end function binary_search
    
end program binary_search_example
```

## Output
```
Element 7 found at index 4
```

## Key Features of this Implementation:

- **Array Declaration**: Uses Fortran's array syntax with predefined values
- **Subroutine Structure**: Separates the binary search logic into a reusable function
- **Parameter Passing**: Passes array, size, and target value as parameters
- **Loop Control**: Uses `do while` loop for the search process
- **Index Handling**: Properly handles array indexing (1-based in Fortran)
- **Return Values**: Returns index if found, -1 if not found

## How it Works:

1. Initialize `low` to 1 and `high` to array size
2. Calculate `mid` as the middle position
3. Compare middle element with target value
4. If match found, return the index
5. If target is greater, search right half
6. If target is smaller, search left half
7. Repeat until element found or search space exhausted

This implementation assumes the input array is sorted in ascending order, which is a requirement for binary search to work correctly.

