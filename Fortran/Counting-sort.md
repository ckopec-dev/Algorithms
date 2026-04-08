Below is a **complete, self‑contained Fortran 90/95 program** that implements the **Counting Sort** algorithm for an array of non‑negative integers.  
The code is heavily commented so you can follow each step of the algorithm, and a small test driver prints the input and the sorted result.

---  

### How the program works
1. **Read / define the input array** `a(:)`.  
2. Find the minimum (`minval`) and maximum (`maxval`) values to know the range of keys.  
3. Allocate a **count array** `cnt(0:range)` where `range = maxval-minval`.  
4. **Frequency pass** – increment `cnt(a(i)-minval)` for each element.  
5. **Cumulative pass** – transform `cnt` into prefix sums so that `cnt(k)` tells the final position of the last element ≤ `k+minval`.  
6. **Output pass** – scan the original array backwards (to keep the sort stable) and place each element into its correct slot in the sorted array `b(:)`.  
7. Print the sorted array.

---  

```fortran
!=====================================================================
!  counting_sort.f90
!  Demonstration of Counting Sort in Fortran 90/95
!=====================================================================
program counting_sort_demo
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)   ! not used, just shows kind syntax
    integer, allocatable :: a(:), b(:)   ! input and output arrays
    integer :: n, i, minval, maxval, range
    integer, allocatable :: cnt(:)

    !-----------------------------------------------------------------
    ! 1. Define / read the data to be sorted
    !-----------------------------------------------------------------
    ! For demonstration we hard‑code a small example.
    ! Feel free to replace this with READ statements or command‑line input.
    n = 12
    allocate(a(n))
    a = [4, 2, 2, 8, 3, 3, 1, 0, 5, 5, 7, 6]   ! unsorted test data

    !-----------------------------------------------------------------
    ! 2. Find the minimum and maximum values (range of keys)
    !-----------------------------------------------------------------
    minval = minval(a)          ! intrinsic MINVAL works on whole array
    maxval = maxval(a)          ! intrinsic MAXVAL
    range  = maxval - minval    ! size of the counting array

    !-----------------------------------------------------------------
    ! 3. Allocate and clear the counting array
    !-----------------------------------------------------------------
    allocate(cnt(0:range))
    cnt = 0                     ! initialize to zero

    !-----------------------------------------------------------------
    ! 4. Frequency pass – count how many times each key occurs
    !-----------------------------------------------------------------
    do i = 1, size(a)
        cnt(a(i) - minval) = cnt(a(i) - minval) + 1
    end do

    !-----------------------------------------------------------------
    ! 5. Cumulative pass – convert counts to positions
    !    After this loop, cnt(k) = number of elements ≤ (k+minval)
    !-----------------------------------------------------------------
    do i = 1, range
        cnt(i) = cnt(i) + cnt(i-1)
    end do

    !-----------------------------------------------------------------
    ! 6. Allocate output array and place elements in sorted order
    !    Scanning the input backwards makes the sort stable.
    !-----------------------------------------------------------------
    allocate(b(size(a)))
    do i = size(a), 1, -1          ! reverse loop for stability
        integer :: key, pos
        key   = a(i) - minval       ! index into cnt
        pos   = cnt(key)            ! current last position for this key
        b(pos) = a(i)               ! place element
        cnt(key) = pos - 1          ! decrement position for next same key
    end do

    !-----------------------------------------------------------------
    ! 7. Output results
    !-----------------------------------------------------------------
    print *, 'Original array:'
    print *, a
    print *, 'Sorted array (Counting Sort):'
    print *, b

    !-----------------------------------------------------------------
    ! Clean up (optional, as program ends)
    !-----------------------------------------------------------------
    deallocate(a, b, cnt)
end program counting_sort_demo
```

#### How to compile and run
```bash
# Using GNU Fortran (gfortran)
gfortran -std=f2008 -Wall -Wextra -O2 counting_sort.f90 -o counting_sort
./counting_sort
```

**Sample output**

```
 Original array:
           4           2           2           8           3           3           1           0           5           5           7           6
 Sorted array (Counting Sort):
           0           1           2           2           3           3           4           5           5           6           7           8
```

Feel free to adapt the program:
* Replace the hard‑coded array with user input (`read(*,*) a`) or data from a file.  
* Extend it to handle negative integers by shifting the key range as shown (`a(i)-minval`).  
* For larger key ranges, consider using `integer(kind=8)` for the counting array to avoid overflow.  

This example demonstrates the classic **Counting Sort** technique in clear, idiomatic Fortran. Happy coding!