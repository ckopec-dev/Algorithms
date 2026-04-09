# Euclidean Algorithm in Fortran

Here's an example implementation of the Euclidean algorithm to find the Greatest Common Divisor (GCD) in Fortran:

```fortran
program euclidean_gcd
    implicit none
    integer :: a, b, result
    
    ! Read input values
    write(*,*) 'Enter two positive integers:'
    read(*,*) a, b
    
    ! Call the GCD function
    result = gcd(a, b)
    
    ! Display the result
    write(*,*) 'GCD of', a, 'and', b, 'is:', result
    
contains
    
    ! Function to calculate GCD using Euclidean algorithm
    integer function gcd(x, y)
        implicit none
        integer, intent(in) :: x, y
        integer :: temp
        
        ! Make sure we work with positive numbers
        x = abs(x)
        y = abs(y)
        
        ! Euclidean algorithm
        do while (y /= 0)
            temp = y
            y = mod(x, y)
            x = temp
        end do
        
        gcd = x
    end function gcd
    
end program euclidean_gcd
```

## Alternative Recursive Implementation

```fortran
program euclidean_gcd_recursive
    implicit none
    integer :: a, b, result
    
    ! Read input values
    write(*,*) 'Enter two positive integers:'
    read(*,*) a, b
    
    ! Call the recursive GCD function
    result = gcd_recursive(a, b)
    
    ! Display the result
    write(*,*) 'GCD of', a, 'and', b, 'is:', result
    
contains
    
    ! Recursive function to calculate GCD
    integer function gcd_recursive(x, y)
        implicit none
        integer, intent(in) :: x, y
        
        if (y == 0) then
            gcd_recursive = x
        else
            gcd_recursive = gcd_recursive(y, mod(x, y))
        end if
    end function gcd_recursive
    
end program euclidean_gcd_recursive
```

## Sample Output

```
Enter two positive integers:
48 18
GCD of 48 and 18 is: 6
```

## How it works:

1. **Iterative version**: Uses a `do while` loop to repeatedly apply the Euclidean algorithm
2. **Recursive version**: Uses function recursion to implement the same algorithm
3. **Algorithm steps**:
   - Divide the larger number by the smaller number
   - Replace the larger number with the smaller number
   - Replace the smaller number with the remainder
   - Repeat until remainder becomes 0
   - The last non-zero remainder is the GCD

The Euclidean algorithm is efficient with a time complexity of O(log(min(a,b))).

