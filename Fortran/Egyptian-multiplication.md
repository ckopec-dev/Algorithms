# Egyptian Multiplication Algorithm in Fortran

Here's an implementation of the Egyptian multiplication algorithm (also known as Russian peasant multiplication) in Fortran:

```fortran
program egyptian_multiplication
    implicit none
    integer :: a, b, result
    
    ! Read input values
    write(*,*) 'Enter two positive integers:'
    read(*,*) a, b
    
    ! Call the Egyptian multiplication function
    result = egyptian_mult(a, b)
    
    ! Display the result
    write(*,*) 'Result of ', a, ' × ', b, ' = ', result
    
end program egyptian_multiplication

integer function egyptian_mult(x, y)
    implicit none
    integer, intent(in) :: x, y
    integer :: a, b, sum
    
    ! Initialize variables
    a = x
    b = y
    sum = 0
    
    ! Egyptian multiplication algorithm
    do while (a > 0)
        ! If a is odd, add b to the sum
        if (mod(a, 2) == 1) then
            sum = sum + b
        end if
        
        ! Halve a and double b
        a = a / 2
        b = b * 2
    end do
    
    egyptian_mult = sum
    
end function egyptian_mult
```

## How it works:

1. **Input**: Two positive integers `x` and `y`
2. **Algorithm**:
   - While the first number is greater than 0:
     - If the first number is odd, add the second number to the result
     - Halve the first number (integer division)
     - Double the second number
3. **Output**: The final sum equals the product of the original two numbers

## Example execution:

```
Enter two positive integers:
7 13
Result of 7 × 13 = 91
```

## Trace of the algorithm for 7 × 13:

| a | b  | a odd? | sum |
|---|----|--------|-----|
| 7 | 13 | yes    | 13  |
| 3 | 26 | yes    | 39  |
| 1 | 52 | yes    | 91  |
| 0 | 104| -      | 91  |

The algorithm correctly computes 7 × 13 = 91.

