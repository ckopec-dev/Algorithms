# Fermat's Factorization Method in Fortran

```fortran
program fermat_factorization
    implicit none
    integer :: n, a, b, sqrt_n
    integer :: i, j
    
    ! Read the number to factor
    write(*,*) 'Enter the number to factor:'
    read(*,*) n
    
    ! Calculate the square root of n
    sqrt_n = int(sqrt(real(n))) + 1
    
    ! Start Fermat's factorization
    a = sqrt_n
    write(*,*) 'Starting Fermat factorization...'
    write(*,*) 'Initial value of a:', a
    
    ! Main loop
    do while (a * a - n >= 0)
        b = a * a - n
        if (b == int(sqrt(real(b))) ** 2) then
            ! Found a perfect square
            write(*,*) 'Factors found!'
            write(*,*) 'a =', a
            write(*,*) 'b =', int(sqrt(real(b)))
            write(*,*) 'Factors are:', a + int(sqrt(real(b))), 'and', a - int(sqrt(real(b)))
            write(*,*) 'Verification:', (a + int(sqrt(real(b)))) * (a - int(sqrt(real(b)))) 
            return
        end if
        a = a + 1
    end do
    
    write(*,*) 'No factors found using Fermat''s method'
    
end program fermat_factorization
```

## Example Usage

For n = 5959:

```
Enter the number to factor:
5959
Starting Fermat factorization...
Initial value of a: 78
Factors found!
a = 80
b = 21
Factors are: 101 and 59
Verification: 5959
```

## How it works:

1. **Input**: Takes an odd composite number n to factor
2. **Initialization**: Sets a = ⌈√n⌉
3. **Loop**: For each a, computes b² = a² - n
4. **Check**: If b² is a perfect square, then n = (a+b)(a-b)
5. **Output**: Returns the factors

## Key Features:

- Uses integer arithmetic for efficiency
- Includes proper verification of results
- Handles the case where no factors are found
- Demonstrates the mathematical principle that n = a² - b² = (a+b)(a-b)

This implementation shows the classic approach to Fermat's factorization method, which is particularly effective for numbers that are close to perfect squares.

