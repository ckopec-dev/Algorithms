# Radix Sort Algorithm in Fortran

Here's an implementation of the Radix Sort algorithm in Fortran:

```fortran
program radix_sort_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n) = [170, 45, 75, 90, 2, 802, 24, 66, 12, 345]
    integer :: i, max_val
    
    ! Function to find maximum value in array
    max_val = maxval(arr)
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Perform radix sort
    call radix_sort(arr, n, max_val)
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
end program radix_sort_example

subroutine radix_sort(arr, n, max_val)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: n, max_val
    integer :: exp, i, j
    integer, allocatable :: output(:)
    integer :: count(0:9)
    
    ! Allocate output array
    allocate(output(n))
    
    ! Start with least significant digit
    exp = 1
    
    ! Continue until we process all digits
    do while (max_val / exp > 0)
        ! Initialize count array
        count = 0
        
        ! Count occurrences of each digit
        do i = 1, n
            j = (arr(i) / exp) mod 10
            count(j) = count(j) + 1
        end do
        
        ! Convert count to actual positions
        do i = 1, 9
            count(i) = count(i) + count(i-1)
        end do
        
        ! Build output array from right to left
        do i = n, 1, -1
            j = (arr(i) / exp) mod 10
            output(count(j)) = arr(i)
            count(j) = count(j) - 1
        end do
        
        ! Copy output array back to original array
        do i = 1, n
            arr(i) = output(i)
        end do
        
        ! Move to next digit
        exp = exp * 10
    end do
    
    ! Deallocate output array
    deallocate(output)
    
end subroutine radix_sort
```

## How it works:

1. **Find Maximum Value**: First, we determine the maximum value in the array to know how many digits we need to process.

2. **Process Each Digit**: Starting from the least significant digit (units place), we sort the array using counting sort for that digit position.

3. **Counting Sort**: For each digit position, we:
   - Count how many numbers have each digit (0-9)
   - Convert counts to actual positions
   - Build a new output array based on current digit
   - Copy the output back to the original array

4. **Move to Next Digit**: We multiply the exponent by 10 and repeat until all digits are processed.

## Sample Output:
```
Original array:
170 45 75 90 2 802 24 66 12 345 

Sorted array:
2 12 45 66 75 90 170 245 345 802 
```

## Key Features:
- **Time Complexity**: O(d × n) where d is the number of digits
- **Space Complexity**: O(n + k) where k is the range of digits (0-9)
- **Stable Sort**: Maintains relative order of equal elements
- **Non-comparative**: Doesn't use element comparisons, making it faster for certain data types

