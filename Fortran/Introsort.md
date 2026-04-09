# Introsort Algorithm in Fortran

Here's an implementation of the Introsort (Introspective Sort) algorithm in Fortran:

```fortran
program introsort_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n)
    integer :: i
    
    ! Initialize array with random values
    arr = [5, 2, 8, 1, 9, 3, 7, 4, 6, 0]
    
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I3)', advance='no') arr(i)
    end do
    write(*,*)
    
    ! Perform introsort
    call introsort(arr, 1, n)
    
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I3)', advance='no') arr(i)
    end do
    write(*,*)
    
end program introsort_example

subroutine introsort(arr, low, high)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: low, high
    integer :: depth_limit
    
    ! Calculate depth limit (2 * log2(n))
    depth_limit = 2 * int(log(real(high - low + 1)) / log(2.0))
    
    call introsort_recursive(arr, low, high, depth_limit)
end subroutine introsort

subroutine introsort_recursive(arr, low, high, depth_limit)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: low, high, depth_limit
    integer :: pivot, i, j
    
    if (high - low + 1 <= 1) then
        return
    else if (depth_limit <= 0) then
        ! Use heapsort when depth limit reached
        call heapsort(arr, low, high)
    else if (high - low + 1 <= 16) then
        ! Use insertion sort for small arrays
        call insertion_sort(arr, low, high)
    else
        ! Use quicksort partitioning
        pivot = partition(arr, low, high)
        call introsort_recursive(arr, low, pivot - 1, depth_limit - 1)
        call introsort_recursive(arr, pivot + 1, high, depth_limit - 1)
    end if
end subroutine introsort_recursive

integer function partition(arr, low, high)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: low, high
    integer :: i, j, pivot_val, temp
    
    ! Use median-of-three for pivot selection
    call median_of_three(arr, low, high)
    
    pivot_val = arr(high)
    i = low - 1
    
    do j = low, high - 1
        if (arr(j) <= pivot_val) then
            i = i + 1
            call swap(arr(i), arr(j))
        end if
    end do
    
    call swap(arr(i + 1), arr(high))
    partition = i + 1
end function partition

subroutine median_of_three(arr, low, high)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: low, high
    integer :: mid, temp
    
    mid = (low + high) / 2
    
    ! Sort arr(low), arr(mid), arr(high) in ascending order
    if (arr(mid) < arr(low)) then
        call swap(arr(low), arr(mid))
    end if
    if (arr(high) < arr(low)) then
        call swap(arr(low), arr(high))
    end if
    if (arr(high) < arr(mid)) then
        call swap(arr(mid), arr(high))
    end if
    
    ! Place median at the end
    call swap(arr(mid), arr(high))
end subroutine median_of_three

subroutine swap(a, b)
    implicit none
    integer, intent(inout) :: a, b
    integer :: temp
    
    temp = a
    a = b
    b = temp
end subroutine swap

subroutine insertion_sort(arr, low, high)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: low, high
    integer :: i, j, key
    
    do i = low + 1, high
        key = arr(i)
        j = i - 1
        
        do while (j >= low .and. arr(j) > key)
            arr(j + 1) = arr(j)
            j = j - 1
        end do
        
        arr(j + 1) = key
    end do
end subroutine insertion_sort

subroutine heapsort(arr, low, high)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: low, high
    integer :: n, i
    
    n = high - low + 1
    
    ! Build max heap
    do i = n/2, 1, -1
        call heapify(arr, low, high, i)
    end do
    
    ! Extract elements from heap one by one
    do i = n, 2, -1
        call swap(arr(low), arr(low + i - 1))
        call heapify(arr, low, low + i - 2, 1)
    end do
end subroutine heapsort

subroutine heapify(arr, low, high, i)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: low, high, i
    integer :: largest, left, right, temp
    
    largest = i
    left = 2 * i
    right = 2 * i + 1
    
    if (left <= high - low + 1 .and. arr(low + left - 1) > arr(low + largest - 1)) then
        largest = left
    end if
    
    if (right <= high - low + 1 .and. arr(low + right - 1) > arr(low + largest - 1)) then
        largest = right
    end if
    
    if (largest /= i) then
        call swap(arr(low + i - 1), arr(low + largest - 1))
        call heapify(arr, low, high, largest)
    end if
end subroutine heapify
```

## How the Introsort Algorithm Works:

1. **Hybrid Approach**: Introsort combines three sorting algorithms:
   - **Quicksort** for general sorting
   - **Heapsort** as fallback when recursion depth exceeds limits
   - **Insertion sort** for small subarrays

2. **Key Features**:
   - Uses **median-of-three** pivot selection for better performance
   - Switches to heapsort when recursion depth exceeds `2 * log2(n)`
   - Uses insertion sort for subarrays with 16 or fewer elements
   - Guarantees O(n log n) worst-case time complexity

3. **Time Complexity**:
   - Average case: O(n log n)
   - Worst case: O(n log n) - thanks to the fallback to heapsort
   - Space complexity: O(log n) due to recursion

4. **Output**:
```
Original array:
  5  2  8  1  9  3  7  4  6  0
Sorted array:
  0  1  2  3  4  5  6  7  8  9
```

This implementation demonstrates the robustness of Introsort by combining the best properties of multiple sorting algorithms while maintaining predictable performance characteristics.

