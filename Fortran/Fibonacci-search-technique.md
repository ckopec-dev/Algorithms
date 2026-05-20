# Fibonacci Search Technique in Fortran

Here's an implementation of the Fibonacci search algorithm in Fortran:

```fortran
program fibonacci_search_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n) = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    integer :: key, result
    integer :: i
    
    ! Print the array
    write(*,*) 'Array elements:'
    do i = 1, n
        write(*,'(I3)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Search for a key value
    key = 11
    result = fibonacci_search(arr, n, key)
    
    if (result /= -1) then
        write(*,*) 'Element', key, 'found at index', result
    else
        write(*,*) 'Element', key, 'not found'
    end if
    
    ! Search for an element that doesn't exist
    key = 8
    result = fibonacci_search(arr, n, key)
    
    if (result /= -1) then
        write(*,*) 'Element', key, 'found at index', result
    else
        write(*,*) 'Element', key, 'not found'
    end if

contains

    integer function fibonacci_search(arr, n, key)
        implicit none
        integer, intent(in) :: arr(:), n, key
        integer :: fib_m2, fib_m1, fib_m, offset, i
        
        ! Generate Fibonacci numbers until we find a number >= n
        fib_m2 = 0  ! (m-2)th Fibonacci number
        fib_m1 = 1  ! (m-1)th Fibonacci number
        fib_m = fib_m2 + fib_m1  ! mth Fibonacci number
        
        do while (fib_m < n)
            fib_m2 = fib_m1
            fib_m1 = fib_m
            fib_m = fib_m2 + fib_m1
        end do
        
        offset = -1  ! Offset variable
        
        ! Continue searching while there are elements to search
        do while (fib_m > 1)
            ! Check if fib_m2 is valid index
            i = min(offset + fib_m2, n - 1)
            
            if (arr(i + 1) < key) then
                ! Move to right subarray
                fib_m = fib_m1
                fib_m1 = fib_m2
                fib_m2 = fib_m - fib_m1
                offset = i
            else if (arr(i + 1) > key) then
                ! Move to left subarray
                fib_m = fib_m2
                fib_m1 = fib_m1 - fib_m2
                fib_m2 = fib_m - fib_m1
            else
                ! Element found
                fibonacci_search = i + 1
                return
            end if
        end do
        
        ! Compare the last element with key
        if (fib_m1 == 1 .and. arr(offset + 1) == key) then
            fibonacci_search = offset + 1
            return
        end if
        
        ! Element not found
        fibonacci_search = -1
    end function fibonacci_search

end program fibonacci_search_example
```

## How the Algorithm Works:

1. **Fibonacci Number Generation**: The algorithm first generates Fibonacci numbers until it finds one that is greater than or equal to the array size.

2. **Search Process**: 
   - Compare the key with the element at position `offset + fib_m2`
   - If key is greater, move to the right subarray
   - If key is smaller, move to the left subarray
   - If key matches, return the index

3. **Time Complexity**: O(log n)
4. **Space Complexity**: O(1)

## Sample Output:
```
Array elements:
  1  3  5  7  9 11 13 15 17 19
Element 11 found at index 6
Element 8 not found
```

This implementation demonstrates the Fibonacci search technique, which is particularly useful for searching in sorted arrays and can be more efficient than binary search in certain scenarios due to its minimal number of comparisons.

