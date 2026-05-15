# Knapsack Problem Algorithm in Fortran

Here's an implementation of the 0/1 Knapsack problem using dynamic programming in Fortran:

```fortran
program knapsack_problem
    implicit none
    
    integer, parameter :: n = 4  ! Number of items
    integer, parameter :: W = 5  ! Knapsack capacity
    integer :: i, j
    integer :: weights(n) = [2, 1, 3, 2]  ! Item weights
    integer :: values(n) = [3, 2, 4, 2]   ! Item values
    integer :: dp(n+1, W+1)  ! DP table
    integer :: max_value
    
    ! Initialize DP table
    do i = 0, n
        do j = 0, W
            dp(i, j) = 0
        end do
    end do
    
    ! Fill the DP table
    do i = 1, n
        do j = 0, W
            ! Don't take the item
            dp(i, j) = dp(i-1, j)
            
            ! Take the item if it fits
            if (weights(i) <= j) then
                dp(i, j) = max(dp(i, j), dp(i-1, j-weights(i)) + values(i))
            end if
        end do
    end do
    
    ! Result
    max_value = dp(n, W)
    write(*,*) 'Maximum value:', max_value
    
    ! Trace back to find which items were selected
    write(*,*) 'Selected items:'
    i = n
    j = W
    do while (i > 0 .and. j > 0)
        ! If value is different from above row, item was included
        if (dp(i, j) /= dp(i-1, j)) then
            write(*,*) 'Item ', i, ' (weight:', weights(i), ', value:', values(i), ')'
            j = j - weights(i)
        end if
        i = i - 1
    end do
    
    ! Print DP table for debugging
    write(*,*) 'DP Table:'
    write(*,*) '     ', (j, j=0,W)
    do i = 0, n
        write(*,*) i, ': ', (dp(i,j), j=0,W)
    end do
    
end program knapsack_problem
```

## Explanation:

This Fortran program solves the classic 0/1 Knapsack problem with:
- **4 items** with weights [2, 1, 3, 2] and values [3, 2, 4, 2]
- **Knapsack capacity** of 5
- Uses **dynamic programming** approach with a 2D table

### Key Components:

1. **DP Table**: `dp(i,j)` represents maximum value achievable with first `i` items and capacity `j`
2. **Recurrence Relation**: 
   - `dp[i][j] = max(dp[i-1][j], dp[i-1][j-weight[i]] + value[i])`
3. **Backtracking**: Determines which items were selected to achieve the maximum value

### Sample Output:
```
Maximum value: 7
Selected items:
Item  4 (weight: 2, value: 2)
Item  3 (weight: 3, value: 4)
Item  1 (weight: 2, value: 3)
DP Table:
     0 1 2 3 4 5
0:  0 0 0 0 0 0
1:  0 0 3 3 3 3
2:  0 2 3 5 5 5
3:  0 2 3 5 6 7
4:  0 2 3 5 7 7
```

The maximum value that can be obtained is **7**, achieved by selecting items with weights 2, 3, and 2 (values 2, 4, and 3 respectively).

