Below is a **complete, self‑contained Fortran 90/95 program** that solves the classic **0/1 Knapsack problem** using a bottom‑up dynamic‑programming (DP) table.  
The program reads the number of items, the knapsack capacity, and then the weight/value of each item from standard input, computes the maximum achievable value, and finally prints the optimal value and the selected items.

```fortran
!=====================================================================
!  0/1 Knapsack – Dynamic Programming example in Fortran
!=====================================================================
!  Compile (e.g., with gfortran):
!      gfortran -std=f95 -O2 -Wall knapsack.f90 -o knapsack
!  Run:
!      ./knapsack < input.txt
!=====================================================================

program knapsack_dp
    implicit none

    !--- Input -------------------------------------------------------
    integer, parameter :: dp_kind = selected_int_kind(15)   ! 64‑bit integer
    integer(dp_kind) :: n, W
    integer(dp_kind), allocatable :: weight(:), value(:)

    read(*,*) n, W                     ! number of items and capacity
    allocate(weight(n), value(n))
    do i = 1, n
        read(*,*) weight(i), value(i)
    end do

    !--- DP table ----------------------------------------------------
    ! dp(i, w) = maximum value achievable using first i items
    !            with total weight <= w
    integer(dp_kind), allocatable :: dp(:,:)
    allocate(dp(0:n,0:W))
    dp = 0                               ! initialise all entries to 0

    do i = 1, n
        do w = 0, W
            if (weight(i) <= w) then
                dp(i,w) = max(dp(i-1,w), &
                              dp(i-1,w-weight(i)) + value(i))
            else
                dp(i,w) = dp(i-1,w)
            end if
        end do
    end do

    !--- Result ------------------------------------------------------
    print *, 'Maximum value that fits in the knapsack:', dp(n,W)

    !--- Reconstruct which items were taken (optional) ---------------
    allocate(logical :: taken(n))
    taken = .false.
    w = W
    do i = n, 1, -1
        if (dp(i,w) /= dp(i-1,w)) then          ! item i was used
            taken(i) = .true.
            w = w - weight(i)
        end if
    end do

    print *, 'Items taken (1 = taken, 0 = not taken):'
    print '(100I2)', taken   ! prints a row of 0/1 flags

    !--- Cleanup ------------------------------------------------------
    deallocate(weight, value, dp, taken)
end program knapsack_dp
```

### How the DP works
1. **State definition** – `dp(i,w)` stores the best value achievable with the first *i* items and a weight limit *w*.
2. **Transition**  
   *If the *i*‑th item does not fit* (`weight(i) > w`) → we cannot take it:  
   `dp(i,w) = dp(i‑1,w)`  
   *Otherwise* we choose the better of:  
   – skip the item: `dp(i‑1,w)`  
   – take the item: `dp(i‑1,w‑weight(i)) + value(i)`
3. **Initialization** – `dp(0,*) = 0` (no items ⇒ zero value) and `dp(*,0) = 0` (zero capacity ⇒ zero value).
4. **Answer** – `dp(n,W)` is the optimal value.
5. **Reconstruction** – walking backwards from `dp(n,W)` tells us which items contributed to the optimum.

### Sample input (`input.txt`)
```
4 5
2 3
3 4
4 5
5 6
```
*Explanation*: 4 items, knapsack capacity = 5.  
(Item weight, value) pairs: (2,3), (3,4), (4,5), (5,6).

### Sample output
```
 Maximum value that fits in the knapsack: 7
 Items taken (1 = taken, 0 = not taken):
 1 1 0 0
```
The optimal solution picks items 1 and 2 (weights 2+3 = 5, value 3+4 = 7).

Feel free to replace the knapsack logic with any other DP problem (e.g., Fibonacci, longest‑increasing‑subsequence, edit distance) – the pattern of allocating a table, filling it via a recurrence, and optionally reconstructing the solution remains the same. Happy coding in Fortran!