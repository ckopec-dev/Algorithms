```fortran
program fast_inv_sqrt_example
    implicit none
    real :: x, y
    integer :: i
    integer, parameter :: N = 1000000
    
    ! Example usage of fast inverse square root
    x = 25.0
    
    ! Call the fast inverse square root function
    y = fast_inverse_square_root(x)
    
    ! Print results
    write(*,*) 'Input: ', x
    write(*,*) 'Fast inverse sqrt: ', y
    write(*,*) 'Actual inverse sqrt: ', 1.0/sqrt(x)
    write(*,*) 'Difference: ', abs(y - (1.0/sqrt(x)))
    
    ! Performance test
    call cpu_time(start_time)
    do i = 1, N
        y = fast_inverse_square_root(real(i))
    end do
    call cpu_time(end_time)
    write(*,*) 'Time for ', N, ' calls: ', end_time - start_time, ' seconds'
    
contains
    
    real function fast_inverse_square_root(number)
        implicit none
        real, intent(in) :: number
        real :: x2
        integer :: i
        real, parameter :: threehalfs = 1.5
        
        ! Calculate 1/sqrt(x) using fast inverse square root
        x2 = number * 0.5
        i = int(number)  ! Convert to integer (bit manipulation)
        
        ! Magic number for fast inverse square root
        i = 0x5f3759df - (i shr 1)
        
        ! Convert back to float
        number = real(i)
        
        ! First Newton-Raphson iteration
        number = number * (threehalfs - (x2 * number * number))
        
        ! Second Newton-Raphson iteration (optional, for better accuracy)
        number = number * (threehalfs - (x2 * number * number))
        
        fast_inverse_square_root = number
    end function fast_inverse_square_root
    
    ! Helper function for bit shifting (since Fortran doesn't have direct bit shift)
    integer function shr(value, shift)
        implicit none
        integer, intent(in) :: value, shift
        shr = iand(value, (2**31 - 1)) shr shift
    end function shr
    
end program fast_inv_sqrt_example
```

**Note:** The above code contains some conceptual elements that need to be adjusted for actual Fortran compatibility. Here's a more practical version:

```fortran
program fast_inv_sqrt_example
    implicit none
    real :: x, y
    integer :: i
    real, parameter :: threehalfs = 1.5
    real, parameter :: magic_number = 0.5
    
    ! Example usage
    x = 25.0
    
    ! Call the fast inverse square root function
    y = fast_inverse_square_root(x)
    
    ! Print results
    write(*,*) 'Input: ', x
    write(*,*) 'Fast inverse sqrt: ', y
    write(*,*) 'Actual inverse sqrt: ', 1.0/sqrt(x)
    write(*,*) 'Difference: ', abs(y - (1.0/sqrt(x)))
    
contains
    
    real function fast_inverse_square_root(number)
        implicit none
        real, intent(in) :: number
        real :: x2, y
        integer :: i
        integer :: i_temp
        
        ! Calculate 1/sqrt(x) using fast inverse square root
        x2 = number * 0.5
        
        ! Convert float to integer representation (simplified)
        ! Note: This is a conceptual implementation
        i_temp = transfer(number, i_temp)  ! Fortran's transfer intrinsic
        
        ! Apply bit manipulation (simplified - actual implementation varies)
        i_temp = 0x5f3759df - (i_temp / 2)
        
        ! Convert back to float
        y = transfer(i_temp, y)
        
        ! Newton-Raphson iterations
        y = y * (threehalfs - (x2 * y * y))
        
        fast_inverse_square_root = y
    end function fast_inverse_square_root
    
end program fast_inv_sqrt_example
```

