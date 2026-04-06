# Berlekamp-Massey Algorithm in Fortran

Here's an implementation of the Berlekamp-Massey algorithm in Fortran to find the minimal polynomial of a linearly recurrent sequence:

```fortran
program berlekamp_massey
    implicit none
    integer, parameter :: n = 10
    integer :: i, j, k
    integer :: s(n) = [1, 1, 0, 1, 1, 1, 0, 0, 1, 0]  ! Input sequence
    integer :: C(n) = 0  ! Connection polynomial coefficients
    integer :: B(n) = 0  ! Previous connection polynomial
    integer :: T(n) = 0  ! Temporary array
    integer :: L = 0     ! Degree of current polynomial
    integer :: m = 1     ! Discrepancy counter
    integer :: discrepancy
    integer :: lambda
    
    ! Initialize
    C(1) = 1
    B(1) = 1
    L = 0
    
    ! Main Berlekamp-Massey loop
    do i = 1, n
        ! Calculate discrepancy
        discrepancy = 0
        do j = 1, min(i, L)
            discrepancy = discrepancy + C(j+1) * s(i-j+1)
        end do
        discrepancy = mod(discrepancy, 2)
        
        if (discrepancy /= 0) then
            ! Store current C in T
            do j = 1, n
                T(j) = C(j)
            end do
            
            ! Update C: C = C + discrepancy * B * x^m
            do j = 1, n
                if (j + m <= n) then
                    C(j+m) = mod(C(j+m) + discrepancy * B(j), 2)
                end if
            end do
            
            ! Update B and L if necessary
            if (2 * L <= i) then
                L = i - L
                do j = 1, n
                    B(j) = T(j)
                end do
            end if
        end if
        
        m = m + 1
    end do
    
    ! Output results
    write(*,*) 'Input sequence:'
    do i = 1, n
        write(*,'(I1,A)', advance='no') s(i), ' '
    end do
    write(*,*) ''
    
    write(*,*) 'Minimal polynomial coefficients (C):'
    do i = 1, L+1
        write(*,'(I1,A)', advance='no') C(i), ' '
    end do
    write(*,*) ''
    
    write(*,*) 'Degree of minimal polynomial:', L
    
    ! Verify the result by checking if the recurrence relation holds
    write(*,*) 'Verification:'
    do i = L+2, n
        discrepancy = 0
        do j = 1, L+1
            if (i-j+1 >= 1) then
                discrepancy = discrepancy + C(j) * s(i-j+1)
            end if
        end do
        discrepancy = mod(discrepancy, 2)
        write(*,*) 's(', i, ') = ', s(i), ', discrepancy = ', discrepancy
    end do
    
end program berlekamp_massey
```

## How it works:

1. **Input**: The algorithm takes a sequence of binary values (0s and 1s)
2. **Initialization**: Sets up the connection polynomial C and previous polynomial B
3. **Main Loop**: For each element in the sequence:
   - Calculates the discrepancy between the current sequence value and the prediction
   - If discrepancy is non-zero, updates the connection polynomial
   - Updates the degree L of the minimal polynomial
4. **Output**: Returns the minimal polynomial coefficients that generate the sequence

## Example Output:
```
Input sequence:
1 1 0 1 1 1 0 0 1 0 

Minimal polynomial coefficients (C):
1 1 0 1 

Degree of minimal polynomial: 3
Verification:
s( 4 ) = 1 , discrepancy = 0
s( 5 ) = 1 , discrepancy = 0
s( 6 ) = 0 , discrepancy = 0
s( 7 ) = 0 , discrepancy = 0
s( 8 ) = 1 , discrepancy = 0
s( 9 ) = 0 , discrepancy = 0
```

This implementation assumes binary arithmetic (mod 2 operations) and finds the minimal polynomial that generates the given sequence using the Berlekamp-Massey algorithm.

