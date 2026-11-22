# BogoSort Algorithm in Fortran

Here's an implementation of the BogoSort (also known as "Stupid Sort") algorithm in Fortran:

```fortran
program bogo_sort_example
    implicit none
    integer, parameter :: n = 5
    integer :: arr(n)
    integer :: i
    logical :: is_sorted
    
    ! Initialize array with random values
    call random_seed()
    do i = 1, n
        call random_number(arr(i))
        arr(i) = int(arr(i) * 100) + 1  ! Random numbers between 1-100
    end do
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I3)', advance='no') arr(i)
    end do
    write(*,*)
    
    ! Perform BogoSort
    call bogo_sort(arr, n)
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I3)', advance='no') arr(i)
    end do
    write(*,*)
    
end program bogo_sort_example

subroutine bogo_sort(arr, n)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: n
    logical :: is_sorted
    integer :: i
    
    ! Continue until array is sorted
    do while (.not. is_sorted(arr, n))
        call shuffle_array(arr, n)
    end do
    
end subroutine bogo_sort

logical function is_sorted(arr, n)
    implicit none
    integer, intent(in) :: arr(:)
    integer, intent(in) :: n
    integer :: i
    
    is_sorted = .true.
    
    ! Check if array is in ascending order
    do i = 1, n-1
        if (arr(i) > arr(i+1)) then
            is_sorted = .false.
            return
        end if
    end do
    
end function is_sorted

subroutine shuffle_array(arr, n)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: n
    integer :: i, j, temp
    real :: r
    
    ! Fisher-Yates shuffle algorithm
    do i = n, 2, -1
        call random_number(r)
        j = int(r * i) + 1
        
        ! Swap elements
        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
    end do
    
end subroutine shuffle_array
```

## How it works:

1. **Main Program**: 
   - Creates an array of random integers
   - Calls the BogoSort subroutine
   - Displays both original and sorted arrays

2. **BogoSort Algorithm**:
   - Continuously shuffles the array until it's sorted
   - Uses a helper function `is_sorted()` to check if array is in order
   - Uses `shuffle_array()` to randomly rearrange elements

3. **Key Components**:
   - `is_sorted()`: Returns `.true.` if array is sorted in ascending order
   - `shuffle_array()`: Implements Fisher-Yates shuffle algorithm
   - Main loop continues until `is_sorted()` returns `.true.`

## Important Notes:

- **Time Complexity**: O((n+1)!) in worst case, which makes it extremely inefficient
- **Space Complexity**: O(1) - sorts in place
- **Practical Use**: This algorithm is purely for educational purposes due to its inefficiency
- **Randomness**: The sorting process relies on random shuffling, so results will vary between runs

The algorithm essentially keeps randomly shuffling the array until it happens to be sorted by chance, making it one of the least efficient sorting algorithms possible.