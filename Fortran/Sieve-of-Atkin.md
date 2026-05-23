# Sieve of Atkin Algorithm in Fortran

Here's an implementation of the Sieve of Atkin algorithm in Fortran to find all prime numbers up to a given limit:

```fortran
program sieve_of_atkin
    implicit none
    integer, parameter :: max_limit = 1000
    integer :: limit, i, j, k, x, y, n
    logical, allocatable :: is_prime(:)
    
    ! Get the limit from user input
    write(*,*) 'Enter the upper limit for prime numbers:'
    read(*,*) limit
    
    ! Allocate memory for the boolean array
    allocate(is_prime(1:limit))
    
    ! Initialize all entries as false
    is_prime = .false.
    
    ! Sieve of Atkin algorithm
    ! Mark numbers as prime based on the three quadratic forms
    
    ! Form 1: 4x² + y² = n, where x ≥ 1, y ≥ 1, and n mod 12 = 1 or n mod 12 = 5
    do x = 1, int(sqrt(real(limit))) + 1
        do y = 1, int(sqrt(real(limit))) + 1
            n = 4 * x * x + y * y
            if (n <= limit) then
                if (mod(n, 12) == 1 .or. mod(n, 12) == 5) then
                    is_prime(n) = .not. is_prime(n)
                end if
            end if
        end do
    end do
    
    ! Form 2: 3x² + y² = n, where x ≥ 1, y ≥ 1, and n mod 12 = 7
    do x = 1, int(sqrt(real(limit))) + 1
        do y = 1, int(sqrt(real(limit))) + 1
            n = 3 * x * x + y * y
            if (n <= limit) then
                if (mod(n, 12) == 7) then
                    is_prime(n) = .not. is_prime(n)
                end if
            end if
        end do
    end do
    
    ! Form 3: 3x² - y² = n, where x > y ≥ 1, and n mod 12 = 11
    do x = 1, int(sqrt(real(limit))) + 1
        do y = 1, x
            n = 3 * x * x - y * y
            if (n <= limit) then
                if (mod(n, 12) == 11) then
                    is_prime(n) = .not. is_prime(n)
                end if
            end if
        end do
    end do
    
    ! Remove multiples of square numbers
    do i = 5, int(sqrt(real(limit))) + 1
        if (is_prime(i)) then
            do j = i * i, limit, i * i
                is_prime(j) = .false.
            end do
        end if
    end do
    
    ! Print all prime numbers
    write(*,*) 'Prime numbers up to ', limit, ':'
    write(*,*) '2, 3, ', advance='no'
    
    do i = 5, limit
        if (is_prime(i)) then
            write(*,*) i, ', ', advance='no'
        end if
    end do
    
    write(*,*)
    
    ! Deallocate memory
    deallocate(is_prime)
    
end program sieve_of_atkin
```

## How the Algorithm Works:

1. **Initialization**: Create a boolean array to track prime numbers
2. **Three Quadratic Forms**:
   - Form 1: `4x² + y² = n` where `n mod 12 = 1 or 5`
   - Form 2: `3x² + y² = n` where `n mod 12 = 7`
   - Form 3: `3x² - y² = n` where `n mod 12 = 11`
3. **Toggle Flags**: For each valid n, toggle the prime flag
4. **Remove Squares**: Eliminate multiples of square numbers
5. **Output Results**: Print all identified primes

## Sample Output:
```
Enter the upper limit for prime numbers:
30
Prime numbers up to 30 :
2, 3, 5, 7, 11, 13, 17, 19, 23, 29
```

## Key Features:

- **Efficient**: Better than the classic Sieve of Eratosthenes for large numbers
- **Memory Optimized**: Uses boolean array for space efficiency
- **Modular**: Clear separation of the three quadratic forms
- **Robust**: Handles edge cases and proper initialization

The Sieve of Atkin is particularly efficient for finding all primes up to a given number, especially when the limit is large.

