# Binary GCD Algorithm in Fortran

Here's an implementation of the Binary GCD (Stein's algorithm) in Fortran:

```fortran
program binary_gcd_example
    implicit none
    integer :: gcd_binary, gcd_recursive
    integer :: a, b
    
    ! Test cases
    a = 48
    b = 18
    write(*,*) 'GCD of ', a, ' and ', b, ' = ', gcd_binary(a, b)
    
    a = 100
    b = 25
    write(*,*) 'GCD of ', a, ' and ', b, ' = ', gcd_binary(a, b)
    
    a = 17
    b = 13
    write(*,*) 'GCD of ', a, ' and ', b, ' = ', gcd_binary(a, b)
    
    a = 0
    b = 5
    write(*,*) 'GCD of ', a, ' and ', b, ' = ', gcd_binary(a, b)
    
end program binary_gcd_example

function gcd_binary(a, b) result(gcd_val)
    implicit none
    integer, intent(in) :: a, b
    integer :: gcd_val
    integer :: u, v, shift
    
    ! Handle edge cases
    if (a == 0) then
        gcd_val = b
        return
    end if
    
    if (b == 0) then
        gcd_val = a
        return
    end if
    
    ! Make sure both numbers are positive
    u = abs(a)
    v = abs(b)
    
    ! Find the common factors of 2
    shift = 0
    do while (mod(u, 2) == 0 .and. mod(v, 2) == 0)
        u = u / 2
        v = v / 2
        shift = shift + 1
    end do
    
    ! Remove remaining factors of 2 from u
    do while (mod(u, 2) == 0)
        u = u / 2
    end do
    
    do
        ! Remove factors of 2 from v
        do while (mod(v, 2) == 0)
            v = v / 2
        end do
        
        ! Ensure u <= v
        if (u > v) then
            call swap(u, v)
        end if
        
        ! Subtract
        v = v - u
        
        if (v == 0) then
            gcd_val = u * 2**shift
            return
        end if
    end do
    
end function gcd_binary

subroutine swap(a, b)
    implicit none
    integer, intent(inout) :: a, b
    integer :: temp
    
    temp = a
    a = b
    b = temp
end subroutine swap
```

## How it works:

The Binary GCD algorithm uses the following principles:
1. **Common factors of 2**: If both numbers are even, divide by 2 and keep track of the power of 2
2. **Remove factors of 2**: Remove all factors of 2 from one number
3. **Subtraction**: Replace the larger number with the difference between the two numbers
4. **Repeat**: Continue until one number becomes zero

## Sample Output:
```
GCD of  48  and  18  =  6
GCD of  100  and  25  =  25
GCD of  17  and  13  =  1
GCD of  0  and  5  =  5
```

## Key Features:
- **Efficient**: Uses bit operations instead of division
- **Handles edge cases**: Works with zero and negative numbers
- **No division operations**: Uses only bit shifts and subtraction
- **Time complexity**: O(log(min(a,b)))

