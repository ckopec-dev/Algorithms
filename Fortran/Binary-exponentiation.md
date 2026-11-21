# Binary Exponentiation Algorithm in Fortran

Here's an example implementation of the binary exponentiation algorithm in Fortran:

```fortran
program binary_exponentiation
    implicit none
    integer, parameter :: long_int = selected_int_kind(18)
    integer(long_int) :: base, exponent, result
    integer :: i
    
    ! Test cases
    base = 3
    exponent = 13
    result = binary_pow(base, exponent)
    print *, '3^13 =', result
    
    base = 2
    exponent = 10
    result = binary_pow(base, exponent)
    print *, '2^10 =', result
    
    base = 5
    exponent = 0
    result = binary_pow(base, exponent)
    print *, '5^0 =', result

contains

    ! Binary exponentiation function
    integer(long_int) function binary_pow(b, n)
        implicit none
        integer(long_int), intent(in) :: b, n
        integer(long_int) :: base, exp, result
        
        ! Handle edge cases
        if (n == 0) then
            binary_pow = 1_long_int
            return
        end if
        
        if (n == 1) then
            binary_pow = b
            return
        end if
        
        base = b
        exp = n
        result = 1_long_int
        
        ! Binary exponentiation algorithm
        do while (exp > 0)
            ! If exponent is odd, multiply result by current base
            if (mod(exp, 2) == 1) then
                result = result * base
            end if
            ! Square the base and halve the exponent
            base = base * base
            exp = exp / 2
        end do
        
        binary_pow = result
    end function binary_pow

end program binary_exponentiation
```

## How it works:

1. **Algorithm Logic**: The function uses the binary representation of the exponent to compute powers efficiently
2. **Time Complexity**: O(log n) instead of O(n) for regular multiplication
3. **Key Steps**:
   - If exponent is odd, multiply result by current base
   - Square the base and halve the exponent
   - Continue until exponent becomes 0

## Sample Output:
```
3^13 = 1594323
2^10 = 1024
5^0 = 1
```

This implementation handles large numbers using `selected_int_kind(18)` for better precision and demonstrates the efficient binary exponentiation technique that reduces the number of multiplications needed.