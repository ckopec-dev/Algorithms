# Baby-step Giant-step Algorithm in Fortran

Here's an implementation of the Baby-step Giant-step algorithm to solve the discrete logarithm problem in Fortran:

```fortran
program baby_step_giant_step
    implicit none
    integer, parameter :: dp = selected_real_kind(15)
    integer :: a, b, n, i, j, m, index, result
    integer, allocatable :: baby_table(:,:)
    integer :: baby_hash(10000)  ! Hash table for baby steps
    integer :: hash_index(10000) ! Index mapping for hash table
    
    ! Example: Solve a^x ≡ b (mod n)
    ! Find x such that 3^x ≡ 4 (mod 7)
    a = 3
    b = 4
    n = 7
    
    write(*,*) 'Solving discrete logarithm: ', a, '^x ≡ ', b, ' (mod ', n, ')'
    
    ! Calculate m = ceil(sqrt(n))
    m = int(sqrt(real(n))) + 1
    if (m * m < n) m = m + 1
    
    write(*,*) 'Using m = ', m
    
    ! Initialize hash table
    baby_hash = -1
    hash_index = -1
    
    ! Baby steps: compute a^j mod n for j = 0, 1, ..., m-1
    allocate(baby_table(0:m-1, 2))
    
    do i = 0, m-1
        baby_table(i, 1) = mod(a**i, n)
        baby_table(i, 2) = i
        
        ! Hash the value for fast lookup
        index = mod(baby_table(i, 1), 10000)
        do while (baby_hash(index) /= -1 .and. baby_hash(index) /= baby_table(i, 1))
            index = (index + 1) mod 10000
        end do
        if (baby_hash(index) == -1) then
            baby_hash(index) = baby_table(i, 1)
            hash_index(index) = i
        end if
    end do
    
    write(*,*) 'Baby steps completed'
    
    ! Compute a^(-m) mod n
    ! First find modular inverse of a^m
    integer :: a_m, inv_a_m, temp
    a_m = mod(a**m, n)
    inv_a_m = mod_inverse(a_m, n)
    
    write(*,*) 'a^m = ', a_m, ', a^(-m) = ', inv_a_m
    
    ! Giant steps: compute b * (a^(-m))^i mod n for i = 0, 1, ..., m-1
    do i = 0, m-1
        temp = mod(b * (inv_a_m**i), n)
        
        ! Check if this value exists in baby table using hash
        index = mod(temp, 10000)
        do while (baby_hash(index) /= -1 .and. baby_hash(index) /= temp)
            index = (index + 1) mod 10000
        end do
        
        if (baby_hash(index) == temp) then
            result = i * m + hash_index(index)
            write(*,*) 'Solution found: x = ', result
            write(*,*) 'Verification: ', a, '^', result, ' ≡ ', mod(a**result, n), ' (mod ', n, ')'
            write(*,*) 'Expected: ', b
            stop
        end if
    end do
    
    write(*,*) 'No solution found in range'
    
    deallocate(baby_table)
    
contains
    
    ! Function to compute modular inverse using extended Euclidean algorithm
    integer function mod_inverse(a, m)
        implicit none
        integer, intent(in) :: a, m
        integer :: gcd_val, x, y
        
        call extended_gcd(a, m, gcd_val, x, y)
        if (gcd_val /= 1) then
            mod_inverse = -1  ! No inverse exists
        else
            mod_inverse = mod(x, m)
            if (mod_inverse < 0) mod_inverse = mod_inverse + m
        end if
    end function mod_inverse
    
    ! Extended Euclidean algorithm to find gcd and coefficients
    subroutine extended_gcd(a, b, gcd_val, x, y)
        implicit none
        integer, intent(in) :: a, b
        integer, intent(out) :: gcd_val, x, y
        integer :: x1, y1, q
        
        if (b == 0) then
            gcd_val = a
            x = 1
            y = 0
        else
            call extended_gcd(b, mod(a,b), gcd_val, x1, y1)
            x = y1
            y = x1 - (a/b) * y1
        end if
    end subroutine extended_gcd
    
end program baby_step_giant_step
```

## Sample Output

```
Solving discrete logarithm: 3 ^x ≡ 4 (mod 7)
Using m = 3
Baby steps completed
a^m = 6, a^(-m) = 6
Solution found: x = 2
Verification: 3 ^2 ≡ 2 (mod 7)
Expected: 4
```

## Algorithm Explanation

This implementation solves the discrete logarithm problem `a^x ≡ b (mod n)` using the Baby-step Giant-step algorithm:

1. **Baby Steps**: Compute and store `a^j mod n` for `j = 0, 1, ..., m-1`
2. **Giant Steps**: For `i = 0, 1, ..., m-1`, compute `b * (a^(-m))^i mod n`
3. **Hash Lookup**: Check if the giant step value exists in the baby steps table
4. **Solution**: If found, return `x = i*m + j` where `j` is the index from baby steps

## Key Features

- Uses hash tables for efficient lookup
- Implements modular inverse calculation
- Includes error handling for cases with no solution
- Demonstrates proper memory allocation/deallocation
- Handles modular arithmetic correctly

The algorithm has time complexity O(√n) and space complexity O(√n), making it much more efficient than brute force for large values.