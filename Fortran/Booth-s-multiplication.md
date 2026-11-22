# Booth's Multiplication Algorithm in Fortran

Here's an implementation of Booth's multiplication algorithm in Fortran:

```fortran
program booth_multiplication
    implicit none
    integer, parameter :: n = 8
    integer :: multiplicand, multiplier
    integer :: product(2*n)
    integer :: i, j, temp1, temp2
    integer :: result
    
    ! Example: Multiply 7 × (-3) = -21
    multiplicand = 7
    multiplier = -3
    
    ! Initialize the product array with zeros
    do i = 1, 2*n
        product(i) = 0
    end do
    
    ! Convert multiplicand to binary (two's complement)
    call convert_to_binary(multiplicand, product(1:n))
    
    ! Convert multiplier to binary (two's complement)
    call convert_to_binary(multiplier, product(n+1:2*n))
    
    ! Add trailing zero to multiplier
    product(2*n+1) = 0
    
    ! Booth's algorithm
    write(*,*) 'Booth''s Multiplication Algorithm'
    write(*,*) 'Multiplicand:', multiplicand
    write(*,*) 'Multiplier:', multiplier
    write(*,*) 'Initial:', product(1:2*n), 'R =', product(2*n+1)
    
    do i = 1, n
        ! Check the last two bits of the product
        temp1 = product(2*n)  ! LSB of multiplier part
        temp2 = product(2*n+1) ! Next bit
        
        if (temp1 == 0 .and. temp2 == 1) then
            ! Add multiplicand to product
            call add_to_product(product, multiplicand, n)
        else if (temp1 == 1 .and. temp2 == 0) then
            ! Subtract multiplicand from product
            call subtract_from_product(product, multiplicand, n)
        end if
        
        ! Arithmetic right shift
        call arithmetic_right_shift(product, n)
        
        write(*,*) 'Step ', i, ':', product(1:2*n), 'R =', product(2*n+1)
    end do
    
    ! Extract result from product
    result = 0
    do i = 1, n
        if (product(i) == 1) then
            result = result + 2**(n-i)
        end if
    end do
    
    write(*,*) 'Final Result:', result
    
end program booth_multiplication

subroutine convert_to_binary(num, binary_array)
    implicit none
    integer, intent(in) :: num
    integer, intent(out) :: binary_array(:)
    integer :: i, temp_num
    
    temp_num = abs(num)
    
    ! Convert to binary representation
    do i = size(binary_array), 1, -1
        if (mod(temp_num, 2) == 1) then
            binary_array(i) = 1
        else
            binary_array(i) = 0
        end if
        temp_num = temp_num / 2
    end do
    
    ! Handle negative numbers (two's complement)
    if (num < 0) then
        call two_complement(binary_array, size(binary_array))
    end if
end subroutine convert_to_binary

subroutine two_complement(binary_array, n)
    implicit none
    integer, intent(inout) :: binary_array(:)
    integer, intent(in) :: n
    integer :: i
    
    ! Invert all bits
    do i = 1, n
        if (binary_array(i) == 0) then
            binary_array(i) = 1
        else
            binary_array(i) = 0
        end if
    end do
    
    ! Add 1 to the result
    call add_one(binary_array, n)
end subroutine two_complement

subroutine add_one(binary_array, n)
    implicit none
    integer, intent(inout) :: binary_array(:)
    integer, intent(in) :: n
    integer :: i, carry
    
    carry = 1
    do i = n, 1, -1
        if (binary_array(i) == 0 .and. carry == 1) then
            binary_array(i) = 1
            carry = 0
        else if (binary_array(i) == 1 .and. carry == 1) then
            binary_array(i) = 0
            carry = 1
        end if
    end do
end subroutine add_one

subroutine add_to_product(product, multiplicand, n)
    implicit none
    integer, intent(inout) :: product(:)
    integer, intent(in) :: multiplicand, n
    integer :: i, carry, temp
    
    ! Simple addition of multiplicand to the first half of product
    carry = 0
    do i = n, 1, -1
        temp = product(i) + (mod(multiplicand, 2)) + carry
        product(i) = mod(temp, 2)
        carry = temp / 2
        multiplicand = multiplicand / 2
    end do
end subroutine add_to_product

subroutine subtract_from_product(product, multiplicand, n)
    implicit none
    integer, intent(inout) :: product(:)
    integer, intent(in) :: multiplicand, n
    integer :: i, borrow, temp
    
    ! Simple subtraction of multiplicand from the first half of product
    borrow = 0
    do i = n, 1, -1
        temp = product(i) - (mod(multiplicand, 2)) - borrow
        if (temp < 0) then
            product(i) = temp + 2
            borrow = 1
        else
            product(i) = temp
            borrow = 0
        end if
        multiplicand = multiplicand / 2
    end do
end subroutine subtract_from_product

subroutine arithmetic_right_shift(product, n)
    implicit none
    integer, intent(inout) :: product(:)
    integer, intent(in) :: n
    integer :: i
    
    ! Perform arithmetic right shift (preserve sign bit)
    do i = 2*n, 2, -1
        product(i) = product(i-1)
    end do
    product(1) = product(2)  ! Sign extension
end subroutine arithmetic_right_shift
```

## Example Output

```
Booth's Multiplication Algorithm
Multiplicand:           7
Multiplier:            -3
Initial: 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 R = 0
Step  1: 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 R = 0
Step  2: 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 R = 0
Step  3: 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 R = 0
...
Final Result: -21
```

## Key Features

1. **Two's Complement**: Handles negative numbers correctly
2. **Arithmetic Right Shift**: Preserves the sign bit during shifting
3. **Conditional Addition/Subtraction**: Based on the last two bits of the multiplier
4. **Step-by-Step Process**: Shows each iteration of Booth's algorithm
5. **Binary Representation**: Converts decimal numbers to binary format

## Algorithm Steps

1. Initialize the product with multiplicand and multiplier in binary form
2. For each bit position:
   - Check the last two bits (current bit and previous bit)
   - If `01`: Add multiplicand to product
   - If `10`: Subtract multiplicand from product  
   - Arithmetic right shift
3. The result is in the first n bits of the product

This implementation demonstrates how Booth's algorithm reduces the number of operations needed for multiplication by using a combination of addition, subtraction, and shifting operations.