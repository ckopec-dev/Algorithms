# Fast Fourier Transform (FFT) in Fortran

Here's a complete example of implementing FFT in Fortran using the Cooley-Tukey algorithm:

```fortran
program fft_example
    implicit none
    integer, parameter :: n = 8
    integer :: i
    complex :: x(n), y(n)
    
    ! Initialize input data (simple test signal)
    do i = 1, n
        x(i) = cmplx(cos(2.0*3.14159*(i-1)/n), 0.0)
    end do
    
    ! Print original signal
    write(*,*) 'Original Signal:'
    do i = 1, n
        write(*,'(I2,2X,F8.4,2X,F8.4)') i, real(x(i)), aimag(x(i))
    end do
    
    ! Perform FFT
    call fft(x, y, n)
    
    ! Print FFT result
    write(*,*) 'FFT Result:'
    do i = 1, n
        write(*,'(I2,2X,F8.4,2X,F8.4)') i, real(y(i)), aimag(y(i))
    end do
    
end program fft_example

subroutine fft(input, output, n)
    implicit none
    integer, intent(in) :: n
    complex, intent(in) :: input(n)
    complex, intent(out) :: output(n)
    integer :: i, j, k, m, l
    complex :: temp, w, wn
    real :: pi
    
    pi = 3.14159265358979
    
    ! Bit-reversal permutation
    do i = 1, n
        j = 0
        do k = 1, n-1
            if (i >= 2**(n-k)) then
                j = j + 2**(k-1)
                i = i - 2**(n-k)
            end if
        end do
        output(j+1) = input(i)
    end do
    
    ! FFT computation
    m = 1
    do l = 1, n/2
        wn = cmplx(cos(2.0*pi/m), -sin(2.0*pi/m))
        w = cmplx(1.0, 0.0)
        do j = 1, m
            do k = j, n, 2*m
                temp = output(k+m) * w
                output(k+m) = output(k) - temp
                output(k) = output(k) + temp
            end do
            w = w * wn
        end do
        m = 2 * m
    end do
    
end subroutine fft
```

## Alternative Implementation using External Library

For more robust FFT operations, you can use external libraries like FFTW:

```fortran
program fftw_example
    implicit none
    include 'fftw3.f'
    
    integer, parameter :: n = 8
    integer :: i
    complex(kind=8) :: in(n), out(n)
    integer(kind=8) :: plan
    
    ! Initialize input data
    do i = 1, n
        in(i) = cmplx(cos(2.0*3.14159*(i-1)/n), 0.0)
    end do
    
    ! Create FFT plan
    call dfftw_plan_dft_1d(plan, n, in, out, FFTW_FORWARD, FFTW_ESTIMATE)
    
    ! Execute FFT
    call dfftw_execute(plan)
    
    ! Print results
    write(*,*) 'FFTW FFT Result:'
    do i = 1, n
        write(*,'(I2,2X,F10.6,2X,F10.6)') i, real(out(i)), aimag(out(i))
    end do
    
    ! Clean up
    call dfftw_destroy_plan(plan)
    
end program fftw_example
```

## Key Features of This Implementation:

- **Cooley-Tukey Algorithm**: Uses the classic radix-2 FFT approach
- **Bit-reversal**: Properly reorders input data for efficient computation
- **In-place computation**: Modifies input array to store output
- **Complex arithmetic**: Handles complex numbers properly
- **Modular design**: Separate subroutine for FFT computation

## Compilation:

```bash
gfortran -o fft_example fft_example.f90
./fft_example
```

This example demonstrates a basic but complete FFT implementation in Fortran that can be used for signal processing applications.