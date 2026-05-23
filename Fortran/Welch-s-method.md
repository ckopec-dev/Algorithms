# Welch's Method Algorithm in Fortran

Here's an implementation of Welch's method for power spectral density estimation in Fortran:

```fortran
program welch_method
    implicit none
    integer, parameter :: n = 1024, nfft = 1024, nseg = 64, noverlap = 32
    integer :: i, j, k, nwin, nfft2
    real, dimension(n) :: x, window
    real, dimension(nseg, nfft/2 + 1) :: psd_matrix
    real, dimension(nfft/2 + 1) :: psd_avg
    real :: fs = 100.0, dt = 1.0/fs
    real :: sum_psd, window_sum
    
    ! Initialize data array (example: sine wave + noise)
    do i = 1, n
        x(i) = sin(2.0*3.14159*10.0*(i-1)*dt) + 0.5*sin(2.0*3.14159*25.0*(i-1)*dt) &
              + 0.1*randn()
    end do
    
    ! Create Hamming window
    nwin = nseg * (n - noverlap) + noverlap
    call hamming_window(window, n)
    
    ! Initialize PSD matrix
    psd_matrix = 0.0
    
    ! Apply Welch's method
    do i = 1, nseg
        ! Extract segment
        do j = 1, nfft
            if ((i-1)*(n - noverlap) + j <= n) then
                psd_matrix(i, j) = x((i-1)*(n - noverlap) + j) * window(j)
            else
                psd_matrix(i, j) = 0.0
            end if
        end do
        
        ! Compute FFT (simplified - in practice use FFT library)
        call fft(psd_matrix(i, :), nfft)
    end do
    
    ! Average the PSD estimates
    psd_avg = 0.0
    do i = 1, nseg
        do j = 1, nfft/2 + 1
            psd_avg(j) = psd_avg(j) + abs(psd_matrix(i, j))**2
        end do
    end do
    
    psd_avg = psd_avg / real(nseg)
    
    ! Output results
    write(*,*) 'Welch''s Method PSD Results:'
    write(*,*) 'Frequency (Hz)    PSD'
    do i = 1, nfft/2 + 1
        write(*,*) (i-1)*fs/nfft, psd_avg(i)
    end do
    
contains
    
    ! Simple Hamming window function
    subroutine hamming_window(w, n)
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(out) :: w
        integer :: i
        real, parameter :: alpha = 0.54, beta = 0.46
        
        do i = 1, n
            w(i) = alpha - beta * cos(2.0*3.14159*(i-1)/(n-1))
        end do
    end subroutine hamming_window
    
    ! Simple FFT function (simplified version)
    subroutine fft(x, n)
        implicit none
        integer, intent(in) :: n
        real, dimension(n), intent(inout) :: x
        ! This is a placeholder - in practice use a proper FFT library
        ! such as FFTW or implement a proper FFT algorithm
        write(*,*) 'FFT routine called for n =', n
    end subroutine fft
    
    ! Random number generator (simple version)
    real function randn()
        implicit none
        real, parameter :: pi = 3.14159
        real :: u1, u2
        
        ! Box-Muller transform
        u1 = rand()
        u2 = rand()
        randn = sqrt(-2.0*log(u1)) * cos(2.0*pi*u2)
    end function randn
    
end program welch_method
```

## Key Components of Welch's Method Implementation:

### 1. **Segmentation**
- Divides the signal into overlapping segments
- Uses a specified overlap (typically 50%)
- Applies windowing to each segment

### 2. **FFT Computation**
- Computes FFT of each windowed segment
- Uses Fast Fourier Transform algorithm

### 3. **Averaging**
- Averages the squared magnitude of FFTs
- Reduces variance of the power spectral density estimate

### 4. **Windowing**
- Applies Hamming window to reduce spectral leakage
- Helps in obtaining smoother spectral estimates

## Parameters Used:
- `n`: Total number of samples
- `nseg`: Number of segments
- `noverlap`: Number of overlapping samples
- `nfft`: FFT size (typically same as segment length)

## Notes:
- This is a simplified implementation
- In practice, use optimized FFT libraries like FFTW
- The `randn()` function generates normally distributed random numbers
- The actual FFT computation would use a proper FFT routine
- Memory allocation and error handling should be added for production code

This implementation demonstrates the core principles of Welch's method for power spectral density estimation.

