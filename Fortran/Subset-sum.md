# Subset Sum Algorithm in Fortran

Here's an implementation of the subset sum algorithm using dynamic programming in Fortran:

```fortran
program subset_sum
    implicit none
    integer, parameter :: n = 6
    integer :: arr(n) = [3, 34, 4, 12, 5, 2]
    integer :: target = 9
    logical :: dp(0:1000)  ! Assuming maximum sum is 1000
    integer :: i, j
    
    ! Initialize dp array
    dp = .false.
    dp(0) = .true.  ! Sum of 0 is always possible (empty subset)
    
    ! Fill the dp array
    do i = 1, n
        ! Traverse backwards to avoid using updated values
        do j = target, arr(i), -1
            if (dp(j - arr(i))) then
                dp(j) = .true.
            end if
        end do
    end do
    
    ! Check if target sum is possible
    if (dp(target)) then
        write(*,*) "Subset with sum ", target, " exists"
    else
        write(*,*) "No subset with sum ", target, " exists"
    end if
    
    ! Print the array for reference
    write(*,*) "Array elements:", (arr(i), i=1,n)
    write(*,*) "Target sum:", target
    
end program subset_sum
```

## How it works:

1. **Dynamic Programming Approach**: Uses a boolean array `dp` where `dp[i]` represents whether sum `i` is achievable
2. **Initialization**: Sets `dp(0) = true` since sum 0 is always possible (empty subset)
3. **Fill Process**: For each element in the array, updates the dp array from right to left to avoid using updated values
4. **Result**: Checks if `dp[target]` is true to determine if the subset sum exists

## Sample Output:
```
Subset with sum 9 exists
Array elements:           3          34           4          12           5           2
Target sum: 9
```

## Time and Space Complexity:
- **Time Complexity**: O(n × sum) where n is the number of elements and sum is the target sum
- **Space Complexity**: O(sum) for the dp array

This implementation efficiently solves the subset sum problem using dynamic programming approach.

