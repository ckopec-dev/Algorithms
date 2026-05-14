```fortran
program carmichael_function_example
    implicit none
    integer :: n, i, phi_n, lambda_n
    integer, allocatable :: factors(:)
    integer :: num_factors
    
    ! Example: Compute λ(15) where 15 = 3 × 5
    n = 15
    write(*,*) 'Computing Carmichael function λ(', n, ')'
    
    ! Find prime factors of n
    call find_prime_factors(n, factors, num_factors)
    
    ! Compute λ(n) using the formula
    lambda_n = carmichael_function(n, factors, num_factors)
    
    write(*,*) 'Prime factors of', n, ':'
    do i = 1, num_factors
        write(*,*) '  ', factors(i)
    end do
    
    write(*,*) 'Carmichael function λ(', n, ') =', lambda_n
    
    ! Additional example: λ(24) where 24 = 2³ × 3
    n = 24
    write(*,*) 'Computing λ(', n, ')'
    
    call find_prime_factors(n, factors, num_factors)
    lambda_n = carmichael_function(n, factors, num_factors)
    
    write(*,*) 'Prime factors of', n, ':'
    do i = 1, num_factors
        write(*,*) '  ', factors(i)
    end do
    
    write(*,*) 'Carmichael function λ(', n, ') =', lambda_n
    
    deallocate(factors)
    
contains
    
    ! Function to compute Euler's totient function φ(n)
    integer function euler_totient(n, factors, num_factors)
        integer, intent(in) :: n, num_factors
        integer, intent(in) :: factors(:)
        integer :: i, temp_n
        
        temp_n = n
        euler_totient = n
        
        do i = 1, num_factors
            if (mod(temp_n, factors(i)) == 0) then
                euler_totient = euler_totient * (factors(i) - 1) / factors(i)
                do while (mod(temp_n, factors(i)) == 0)
                    temp_n = temp_n / factors(i)
                end do
            end if
        end do
    end function euler_totient
    
    ! Function to find prime factors
    subroutine find_prime_factors(n, factors, num_factors)
        integer, intent(in) :: n
        integer, intent(out) :: factors(:)
        integer, intent(out) :: num_factors
        integer :: i, temp_n, count
        
        temp_n = n
        count = 0
        
        ! Check for factor 2
        if (mod(temp_n, 2) == 0) then
            count = count + 1
            factors(count) = 2
            do while (mod(temp_n, 2) == 0)
                temp_n = temp_n / 2
            end do
        end if
        
        ! Check for odd factors from 3 onwards
        i = 3
        do while (i * i <= temp_n)
            if (mod(temp_n, i) == 0) then
                count = count + 1
                factors(count) = i
                do while (mod(temp_n, i) == 0)
                    temp_n = temp_n / i
                end do
            end if
            i = i + 2
        end do
        
        ! If temp_n is still greater than 1, then it's a prime factor
        if (temp_n > 1) then
            count = count + 1
            factors(count) = temp_n
        end if
        
        num_factors = count
    end subroutine find_prime_factors
    
    ! Main Carmichael function computation
    integer function carmichael_function(n, factors, num_factors)
        integer, intent(in) :: n, num_factors
        integer, intent(in) :: factors(:)
        integer :: i, temp_n, prime_power, result, gcd_result
        integer, allocatable :: prime_powers(:)
        integer :: num_powers
        
        ! Allocate array for prime powers
        allocate(prime_powers(num_factors))
        
        ! Find prime powers in factorization
        temp_n = n
        num_powers = 0
        
        do i = 1, num_factors
            prime_power = 1
            do while (mod(temp_n, factors(i)) == 0)
                prime_power = prime_power * factors(i)
                temp_n = temp_n / factors(i)
            end do
            num_powers = num_powers + 1
            prime_powers(num_powers) = prime_power
        end do
        
        ! For n = 1, λ(1) = 1
        if (n == 1) then
            carmichael_function = 1
            deallocate(prime_powers)
            return
        end if
        
        ! For n = 2^k where k >= 3, λ(n) = 2^(k-2)
        if (num_factors == 1 .and. factors(1) == 2) then
            if (n >= 8) then
                carmichael_function = n / 4
                deallocate(prime_powers)
                return
            end if
        end if
        
        ! For n = 2^k where k = 1 or 2, λ(n) = 1
        if (num_factors == 1 .and. factors(1) == 2) then
            carmichael_function = 1
            deallocate(prime_powers)
            return
        end if
        
        ! General case: λ(n) = lcm(λ(p₁^k₁), λ(p₂^k₂), ..., λ(pₘ^kₘ))
        result = 1
        
        do i = 1, num_powers
            if (prime_powers(i) == 2) then
                ! λ(2^k) = 2^(k-1) for k >= 3, λ(2^2) = 2, λ(2^1) = 1
                if (prime_powers(i) == 2) then
                    if (n >= 8) then
                        result = lcm(result, prime_powers(i) / 2)
                    else
                        result = lcm(result, 1)
                    end if
                else
                    result = lcm(result, prime_powers(i) - 1)
                end if
            else
                result = lcm(result, prime_powers(i) - 1)
            end if
        end do
        
        carmichael_function = result
        deallocate(prime_powers)
    end function carmichael_function
    
    ! Helper function to compute LCM
    integer function lcm(a, b)
        integer, intent(in) :: a, b
        integer :: gcd_val
        
        gcd_val = gcd(a, b)
        lcm = (a * b) / gcd_val
    end function lcm
    
    ! Helper function to compute GCD
    integer function gcd(a, b)
        integer, intent(in) :: a, b
        integer :: temp_a, temp_b
        
        temp_a = abs(a)
        temp_b = abs(b)
        
        do while (temp_b /= 0)
            gcd = temp_b
            temp_b = mod(temp_a, temp_b)
            temp_a = gcd
        end do
    end function gcd
    
end program carmichael_function_example
```

This Fortran program demonstrates the computation of the Carmichael function λ(n) with the following key features:

1. **Prime Factorization**: The `find_prime_factors` subroutine finds all prime factors of a given number
2. **Carmichael Function Algorithm**: The `carmichael_function` computes λ(n) using the mathematical formula
3. **Special Cases**: Handles special cases like powers of 2 correctly
4. **LCM Calculation**: Uses helper functions to compute least common multiples
5. **Examples**: Computes λ(15) and λ(24) as examples

The algorithm works by:
- Finding the prime factorization of n
- Computing λ(p^k) for each prime power p^k in the factorization
- Taking the least common multiple of all these values

For example:
- λ(15) = λ(3 × 5) = lcm(λ(3), λ(5)) = lcm(2, 4) = 4
- λ(24) = λ(2³ × 3) = lcm(λ(2³), λ(3)) = lcm(2, 2) = 2

