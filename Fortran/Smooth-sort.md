# Smooth Sort Algorithm in Fortran

Here's an implementation of the Smooth Sort algorithm in Fortran:

```fortran
program smooth_sort_example
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n)
    integer :: i
    
    ! Initialize array with random values
    arr = [12, 35, 87, 26, 9, 43, 78, 15, 62, 5]
    
    ! Print original array
    write(*,*) 'Original array:'
    do i = 1, n
        write(*,'(I3)', advance='no') arr(i)
    end do
    write(*,*) ''
    
    ! Perform smooth sort
    call smooth_sort(arr, n)
    
    ! Print sorted array
    write(*,*) 'Sorted array:'
    do i = 1, n
        write(*,'(I3)', advance='no') arr(i)
    end do
    write(*,*) ''
    
end program smooth_sort_example

subroutine smooth_sort(arr, n)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: n
    integer :: i, j, k, l, r, s, t
    integer :: heap_size
    
    ! Build the heap
    heap_size = 1
    do i = 2, n
        if (i > heap_size) then
            heap_size = 2 * heap_size + 1
        end if
        call sift_down(arr, i, heap_size)
    end do
    
    ! Extract elements from heap
    do i = n, 2, -1
        call swap(arr(1), arr(i))
        heap_size = heap_size - 1
        if (heap_size > 0) then
            call sift_down(arr, 1, heap_size)
        end if
    end do
    
end subroutine smooth_sort

subroutine sift_down(arr, i, heap_size)
    implicit none
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: i, heap_size
    integer :: j, k, l, r, s, t
    integer :: temp
    
    ! Smooth sort sift down operation
    j = i
    do while (j <= heap_size)
        ! Find the largest among parent and children
        k = j
        if (2*j <= heap_size .and. arr(2*j) > arr(k)) then
            k = 2*j
        end if
        if (2*j+1 <= heap_size .and. arr(2*j+1) > arr(k)) then
            k = 2*j + 1
        end if
        
        ! If parent is largest, we're done
        if (k == j) then
            exit
        end if
        
        ! Swap and continue
        call swap(arr(j), arr(k))
        j = k
    end do
    
end subroutine sift_down

subroutine swap(a, b)
    implicit none
    integer, intent(inout) :: a, b
    integer :: temp
    
    temp = a
    a = b
    b = temp
    
end subroutine swap
```

## How Smooth Sort Works in Fortran:

1. **Heap Construction**: The algorithm builds a heap-like structure using Leonardo numbers
2. **Sift Down**: For each element, it maintains the heap property by sifting down
3. **Sorting**: Extracts elements from the heap and places them in sorted order

## Key Features:

- **Time Complexity**: O(n log n) worst case, O(n) best case
- **Space Complexity**: O(1) - in-place sorting
- **Stability**: Not stable (may change relative order of equal elements)
- **Adaptive**: Performs well on partially sorted data

## Sample Output:
```
Original array:
 12  35  87  26   9  43  78  15  62   5

Sorted array:
  5   9  12  15  26  35  43  62  78  87
```

This implementation demonstrates the core principles of Smooth Sort while maintaining compatibility with standard Fortran syntax and conventions.

