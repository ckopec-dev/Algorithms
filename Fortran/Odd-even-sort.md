# Odd-Even Sort Algorithm in Fortran

Here's an implementation of the Odd-Even sort (also known as Brick Sort) algorithm in Fortran:

```fortran
program odd_even_sort_example
    implicit none
    integer, parameter :: n = 8
    integer :: arr(n) = [64, 34, 25, 12, 22, 11, 90, 88]
    integer :: i, j, temp
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Perform odd-even sort
    call odd_even_sort(arr, n)
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I0,1X)', advance='no') arr(i)
    end do
    write(*,*) ''
    
end program odd_even_sort_example

subroutine odd_even_sort(arr, n)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: n
    integer :: i, j, temp
    logical :: sorted
    
    sorted = .false.
    
    do while (.not. sorted)
        sorted = .true.
        
        ! Odd phase: compare odd indexed elements
        do i = 1, n-1, 2
            if (arr(i) > arr(i+1)) then
                temp = arr(i)
                arr(i) = arr(i+1)
                arr(i+1) = temp
                sorted = .false.
            end if
        end do
        
        ! Even phase: compare even indexed elements
        do i = 2, n-1, 2
            if (arr(i) > arr(i+1)) then
                temp = arr(i)
                arr(i) = arr(i+1)
                arr(i+1) = temp
                sorted = .false.
            end if
        end do
    end do
    
end subroutine odd_even_sort
```

## How it works:

1. **Odd-Even Sort** is a variation of Bubble Sort that works in parallel phases
2. **Odd Phase**: Compares and swaps elements at odd indices (1,3,5,...)
3. **Even Phase**: Compares and swaps elements at even indices (2,4,6,...)
4. The process continues until no more swaps are needed

## Sample Output:
```
Original array:
64 34 25 12 22 11 90 88 

Sorted array:
11 12 22 25 34 64 88 90 
```

## Key Features:
- **Time Complexity**: O(n²) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in-place
- **Stable**: Maintains relative order of equal elements
- **Parallelizable**: Odd and even phases can be executed in parallel

The algorithm is particularly useful for parallel computing environments where odd and even phases can be executed simultaneously across different processors.

