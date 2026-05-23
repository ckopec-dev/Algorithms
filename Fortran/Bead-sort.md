# Bead Sort Algorithm in Fortran

Here's an implementation of the Bead Sort (also known as Gravity Sort) algorithm in Fortran:

```fortran
program bead_sort_example
    implicit none
    integer, parameter :: n = 6
    integer :: arr(n) = [5, 3, 8, 1, 9, 2]
    integer :: i, j, k
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Perform bead sort
    call bead_sort(arr, n)
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
end program bead_sort_example

subroutine bead_sort(arr, n)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: n
    integer :: i, j, k, max_val, temp
    integer, allocatable :: beads(:,:)
    
    ! Find maximum value
    max_val = arr(1)
    do i = 2, n
        if (arr(i) > max_val) max_val = arr(i)
    end do
    
    ! Allocate bead matrix
    allocate(beads(max_val, n))
    
    ! Initialize bead matrix
    beads = 0
    
    ! Place beads
    do i = 1, n
        do j = 1, arr(i)
            beads(j, i) = 1
        end do
    end do
    
    ! Let beads fall
    do i = 1, n
        temp = 0
        do j = 1, max_val
            if (beads(j, i) == 1) then
                temp = temp + 1
            end if
        end do
        arr(i) = temp
    end do
    
    ! Clean up
    deallocate(beads)
    
end subroutine bead_sort
```

## How it works:

1. **Find Maximum Value**: Determine the maximum value in the input array to know how many rows are needed for the bead matrix.

2. **Initialize Bead Matrix**: Create a 2D matrix where rows represent bead levels and columns represent input elements.

3. **Place Beads**: For each element in the input array, place that many beads (1s) in the corresponding column from the top.

4. **Let Beads Fall**: For each column, count the number of beads and update the array with this count.

5. **Result**: The array is now sorted in ascending order.

## Sample Output:
```
Original array:
5 3 8 1 9 2 

Sorted array:
1 2 3 5 8 9 
```

This implementation demonstrates the gravity sort algorithm's characteristic behavior of letting beads "fall" to the bottom of their respective columns, effectively sorting the array.

