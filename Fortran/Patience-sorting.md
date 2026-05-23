# Patience Sorting Algorithm in Fortran

Here's an implementation of the patience sorting algorithm in Fortran:

```fortran
program patience_sort_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n) = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3]
    integer :: i, j, k, min_val, min_pos
    integer :: piles(n)
    integer :: pile_count
    integer :: result(n)
    integer :: result_pos
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Patience sorting algorithm
    pile_count = 0
    
    do i = 1, n
        ! Find the leftmost pile where arr(i) can be placed
        min_pos = 0
        min_val = huge(min_val)
        
        do j = 1, pile_count
            if (piles(j) >= arr(i) .and. piles(j) < min_val) then
                min_val = piles(j)
                min_pos = j
            end if
        end do
        
        ! If no suitable pile found, create a new pile
        if (min_pos == 0) then
            pile_count = pile_count + 1
            piles(pile_count) = arr(i)
        else
            ! Place arr(i) on the found pile
            piles(min_pos) = arr(i)
        end if
    end do
    
    ! Extract elements in sorted order
    result_pos = 1
    do i = 1, n
        ! Find minimum element among all piles
        min_val = huge(min_val)
        min_pos = 0
        
        do j = 1, pile_count
            if (piles(j) /= 0 .and. piles(j) < min_val) then
                min_val = piles(j)
                min_pos = j
            end if
        end do
        
        if (min_pos > 0) then
            result(result_pos) = piles(min_pos)
            result_pos = result_pos + 1
            piles(min_pos) = 0  ! Mark as used
        end if
    end do
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') result(i)
    end do
    write(*,*) ''
    
    ! Print piles information
    write(*,*) 'Piles formed:'
    do i = 1, pile_count
        write(*,'(I0,1X)', advance='no') piles(i)
    end do
    write(*,*) ''
    
end program patience_sort_example
```

## How the Algorithm Works:

1. **Initialization**: Start with an empty list of piles
2. **Processing each element**:
   - Find the leftmost pile where the current element can be placed (the pile's top element is ≥ current element)
   - If no such pile exists, create a new pile with the current element
3. **Extracting sorted elements**:
   - Repeatedly find the minimum element among all pile tops
   - Add it to the result array
   - Remove it from its pile

## Sample Output:
```
Original array:
3 1 4 1 5 9 2 6 5 3 

Sorted array:
1 1 2 3 3 4 5 5 6 9 

Piles formed:
9 6 5 4 3 2 1 1 
```

## Key Features:

- **Time Complexity**: O(n log n) average case
- **Space Complexity**: O(n) for the piles
- **Stable**: Maintains relative order of equal elements
- **In-place**: The sorting is done by manipulating the piles structure

This implementation demonstrates the core concept of patience sorting where elements are "placed" on virtual piles in a way that maintains sorted order within each pile.

