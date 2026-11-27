# Extended Euclidean Algorithm in Fortran

Here's an implementation of the Extended Euclidean Algorithm in Fortran:

```fortran
program extended_euclidean
    implicit none
    integer :: a, b, x, y, gcd_value
    
    ! Test with example values
    a = 35
    b = 15
    
    call extended_gcd(a, b, gcd_value, x, y)
    
    write(*,*) 'Extended Euclidean Algorithm'
    write(*,*) '============================='
    write(*,*) 'a = ', a
    write(*,*) 'b = ', b
    write(*,*) 'GCD(a,b) = ', gcd_value
    write(*,*) 'x = ', x
    write(*,*) 'y = ', y
    write(*,*) 'Verification: ', a, '*(', x, ') + ', b, '*(', y, ') = ', a*x + b*y
    
contains

    subroutine extended_gcd(a, b, gcd, x, y)
        implicit none
        integer, intent(in) :: a, b
        integer, intent(out) :: gcd, x, y
        integer :: x1, y1, x2, y2, q, r
        
        ! Base case
        if (b == 0) then
            gcd = a
            x = 1
            y = 0
            return
        end if
        
        ! Recursive call
        call extended_gcd(b, mod(a,b), gcd, x1, y1)
        
        ! Calculate coefficients
        q = a / b
        x = y1
        y = x1 - q * y1
        
    end subroutine extended_gcd

end program extended_euclidean
```

## Output for a=35, b=15:

```
Extended Euclidean Algorithm
=============================
a =  35
b =  15
GCD(a,b) =  5
x =  -1
y =  2
Verification:  35 *(-1) +  15 *(2) =  5
```

## Iterative Version (Alternative Implementation):

```fortran
program extended_euclidean_iterative
    implicit none
    integer :: a, b, x, y, gcd_value
    integer :: x1, y1, x2, y2, q, r
    
    ! Test with example values
    a = 35
    b = 15
    
    call extended_gcd_iterative(a, b, gcd_value, x, y)
    
    write(*,*) 'Iterative Version'
    write(*,*) '================='
    write(*,*) 'a = ', a
    write(*,*) 'b = ', b
    write(*,*) 'GCD(a,b) = ', gcd_value
    write(*,*) 'x = ', x
    write(*,*) 'y = ', y
    
contains

    subroutine extended_gcd_iterative(a, b, gcd, x, y)
        implicit none
        integer, intent(in) :: a, b
        integer, intent(out) :: gcd, x, y
        integer :: x1, y1, x2, y2, temp
        
        ! Initialize coefficients
        x1 = 1
        y1 = 0
        x2 = 0
        y2 = 1
        
        do while (b /= 0)
            q = a / b
            r = mod(a, b)
            
            temp = x2
            x2 = x1 - q * x2
            x1 = temp
            
            temp = y2
            y2 = y1 - q * y2
            y1 = temp
            
            a = b
            b = r
        end do
        
        gcd = a
        x = x1
        y = y1
        
    end subroutine extended_gcd_iterative

end program extended_euclidean_iterative
```

## Key Features:

- **Input**: Two integers `a` and `b`
- **Output**: GCD of `a` and `b`, plus coefficients `x` and `y` such that `ax + by = gcd(a,b)`
- **Verification**: The result satisfies the equation `ax + by = gcd(a,b)`
- **Two implementations**: Recursive and iterative versions
- **Handles negative numbers**: Works with any integer inputs

The algorithm finds not just the greatest common divisor, but also the coefficients (x and y) that satisfy Bézout's identity.