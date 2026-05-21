```fortran
program cocktail_shaker_sort_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n) = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50]
    integer :: i, j, temp, left, right
    logical :: swapped
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Cocktail Shaker Sort Algorithm
    left = 1
    right = n
    swapped = .true.
    
    do while (swapped .and. left < right)
        swapped = .false.
        
        ! Forward pass (left to right)
        do i = left, right - 1
            if (arr(i) > arr(i + 1)) then
                temp = arr(i)
                arr(i) = arr(i + 1)
                arr(i + 1) = temp
                swapped = .true.
            end if
        end do
        
        ! Reduce the right boundary
        right = right - 1
        
        ! Backward pass (right to left)
        do i = right, left + 1, -1
            if (arr(i) < arr(i - 1)) then
                temp = arr(i)
                arr(i) = arr(i - 1)
                arr(i - 1) = temp
                swapped = .true.
            end if
        end do
        
        ! Increase the left boundary
        left = left + 1
    end do
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
end program cocktail_shaker_sort_example
```

**Output:**
```
Original array:
64 34 25 12 22 11 90 88 76 50 

Sorted array:
11 12 22 25 34 50 64 76 88 90 
```

**Algorithm Explanation:**

The cocktail shaker sort (also known as bidirectional bubble sort) is a variation of bubble sort that sorts in both directions on each pass through the list. 

**Key characteristics:**
- **Time Complexity:** O(n²) worst case, O(n) best case
- **Space Complexity:** O(1) - sorts in place
- **Stability:** Stable sorting algorithm
- **Direction:** Alternates between left-to-right and right-to-left passes

**How it works:**
1. Perform a forward pass from left to right, swapping adjacent elements if they're in wrong order
2. Perform a backward pass from right to left, swapping adjacent elements if they're in wrong order
3. Continue until no swaps are needed
4. The boundaries (left and right) are adjusted after each pass since the largest/smallest elements are already in their correct positions

This algorithm is more efficient than simple bubble sort because it eliminates elements from both ends of the array in each iteration.

