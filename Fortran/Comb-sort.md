# Comb Sort Algorithm in Fortran

Here's an implementation of the Comb Sort algorithm in Fortran:

```fortran
program comb_sort_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n) = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50]
    integer :: i, gap, temp, swapped
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Perform comb sort
    call comb_sort(arr, n)
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
end program comb_sort_example

subroutine comb_sort(arr, n)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: n
    integer :: gap, i, temp, swapped
    logical :: go_on
    
    ! Initialize gap
    gap = n
    go_on = .true.
    
    do while (go_on)
        ! Update gap using shrink factor of 1.3
        gap = int(real(gap) / 1.3)
        
        ! If gap becomes 1, we're doing a final bubble sort
        if (gap < 1) then
            gap = 1
            go_on = .false.
        end if
        
        swapped = 0
        
        ! Compare elements with current gap
        do i = 1, n - gap
            if (arr(i) > arr(i + gap)) then
                ! Swap elements
                temp = arr(i)
                arr(i) = arr(i + gap)
                arr(i + gap) = temp
                swapped = swapped + 1
            end if
        end do
        
        ! If no swaps occurred in this pass, array is sorted
        if (gap == 1 .and. swapped == 0) then
            go_on = .false.
        end if
    end do
    
end subroutine comb_sort
```

## How it works:

1. **Initialization**: Start with a gap equal to the array size
2. **Gap reduction**: Reduce the gap by dividing by 1.3 (shrink factor)
3. **Comparison**: Compare elements separated by the current gap
4. **Swap**: If elements are out of order, swap them
5. **Repeat**: Continue until gap becomes 1 and no more swaps occur

## Key Features:

- **Shrink Factor**: Uses 1.3 as the shrink factor (commonly used value)
- **Optimization**: When gap reaches 1, it performs a final bubble sort pass
- **Efficiency**: Better than bubble sort but simpler than quicksort
- **Space Complexity**: O(1) - sorts in place
- **Time Complexity**: O(n²) worst case, O(n log n) average case

## Sample Output:
```
Original array:
64 34 25 12 22 11 90 88 76 50 

Sorted array:
11 12 22 25 34 50 64 76 88 90 
```

