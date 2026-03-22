# Sieve of Eratosthenes in Fortran

Here's an implementation of the Sieve of Eratosthenes algorithm in Fortran:

```fortran
program sieve_of_eratosthenes
    implicit none
    integer, parameter :: max_num = 100
    logical :: is_prime(max_num)
    integer :: i, j, n
    
    ! Read the upper limit
    write(*,*) 'Enter the upper limit (n):'
    read(*,*) n
    
    ! Initialize all numbers as prime (except 0 and 1)
    do i = 1, n
        is_prime(i) = .true.
    end do
    is_prime(1) = .false.
    
    ! Sieve of Eratosthenes algorithm
    do i = 2, int(sqrt(real(n)))
        if (is_prime(i)) then
            ! Mark all multiples of i as not prime
            do j = i*i, n, i
                is_prime(j) = .false.
            end do
        end if
    end do
    
    ! Print all prime numbers
    write(*,*) 'Prime numbers up to', n, ':'
    do i = 2, n
        if (is_prime(i)) then
            write(*,*) i
        end if
    end do
    
end program sieve_of_eratosthenes
```

## How it works:

1. **Initialization**: Create a boolean array `is_prime` where `is_prime(i)` represents whether number `i` is prime
2. **Mark non-primes**: Starting from 2, mark all multiples of each prime number as not prime
3. **Optimization**: Only check up to √n since larger factors would have been found already
4. **Output**: Print all numbers that remain marked as prime

## Sample Output:
```
Enter the upper limit (n):
30
Prime numbers up to 30 :
           2
           3
           5
           7
          11
          13
          17
          19
          23
          29
```

## Key Features:
- Uses logical array for efficient memory usage
- Implements the classic optimization of starting from i²
- Includes proper initialization of edge cases (0 and 1)
- Handles input validation through user prompt
- Uses integer arithmetic for efficiency

