# Exponential Search Algorithm in Fortran

Here's an implementation of the exponential search algorithm in Fortran:

```fortran
program exponential_search_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n) = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    integer :: target, result
    integer :: i
    
    ! Print the array
    write(*,*) 'Array elements:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Test cases
    target = 7
    result = exponential_search(arr, n, target)
    write(*,*) 'Searching for ', target, ': Index = ', result
    
    target = 1
    result = exponential_search(arr, n, target)
    write(*,*) 'Searching for ', target, ': Index = ', result
    
    target = 19
    result = exponential_search(arr, n, target)
    write(*,*) 'Searching for ', target, ': Index = ', result
    
    target = 10
    result = exponential_search(arr, n, target)
    write(*,*) 'Searching for ', target, ': Index = ', result

contains

    integer function exponential_search(arr, n, target)
        implicit none
        integer, intent(in) :: arr(:), n, target
        integer :: i, bound
        integer :: left, right
        
        ! If target is first element
        if (arr(1) == target) then
            exponential_search = 1
            return
        end if
        
        ! Find range where target may exist
        bound = 1
        do while (bound < n .and. arr(bound+1) <= target)
            bound = bound * 2
        end do
        
        ! Binary search in the range [bound/2 + 1, min(bound, n)]
        left = bound/2 + 1
        right = min(bound, n)
        
        ! Binary search implementation
        do while (left <= right)
            integer :: mid
            mid = (left + right) / 2
            if (arr(mid) == target) then
                exponential_search = mid
                return
            else if (arr(mid) < target) then
                left = mid + 1
            else
                right = mid - 1
            end if
        end do
        
        ! Target not found
        exponential_search = -1
    end function exponential_search

end program exponential_search_example
```

## How it works:

1. **Initial Check**: First checks if the target is at the first position
2. **Exponential Growth**: Doubles the search range until finding an element greater than or equal to the target
3. **Binary Search**: Performs binary search within the identified range
4. **Return Value**: Returns the index if found, -1 if not found

## Expected Output:
```
Array elements:
1 3 5 7 9 11 13 15 17 19 

Searching for 7: Index = 4
Searching for 1: Index = 1
Searching for 19: Index = 10
Searching for 10: Index = -1
```

## Time Complexity:
- **Best Case**: O(1) - when target is at index 1
- **Average/Worst Case**: O(log n) - due to the binary search component

## Space Complexity:
- O(1) - only using a constant amount of extra space

This implementation demonstrates how exponential search is particularly useful when dealing with unbounded or very large arrays where the target element might be located at a relatively small index.

