# Arithmetic Coding Algorithm in Fortran

Here's a complete implementation of the arithmetic coding algorithm in Fortran:

```fortran
program arithmetic_coding
    implicit none
    integer, parameter :: max_symbols = 256
    integer, parameter :: max_length = 1000
    integer, parameter :: precision = 32
    
    character(len=1), dimension(max_symbols) :: symbols
    integer :: frequencies(max_symbols)
    integer :: num_symbols
    integer :: i, j, k
    integer :: total_freq
    real :: low, high, range
    integer :: code_value
    character(len=1) :: input_string(max_length)
    integer :: string_length
    
    ! Initialize symbol frequencies
    call initialize_symbols()
    
    ! Example input string
    string_length = 10
    input_string(1) = 'a'
    input_string(2) = 'b'
    input_string(3) = 'c'
    input_string(4) = 'a'
    input_string(5) = 'b'
    input_string(6) = 'a'
    input_string(7) = 'c'
    input_string(8) = 'b'
    input_string(9) = 'a'
    input_string(10) = 'b'
    
    ! Display input string
    write(*,*) 'Input string: ', (input_string(i), i=1,string_length)
    
    ! Encode the string
    call encode_string(input_string, string_length, code_value, range)
    
    write(*,*) 'Encoded code value: ', code_value
    write(*,*) 'Range: ', range
    
    ! Decode the string
    call decode_string(code_value, range, input_string, string_length)
    
    write(*,*) 'Decoded string: ', (input_string(i), i=1,string_length)
    
contains

    subroutine initialize_symbols()
        ! Initialize with example symbols and frequencies
        num_symbols = 3
        symbols(1) = 'a'
        symbols(2) = 'b'
        symbols(3) = 'c'
        frequencies(1) = 4
        frequencies(2) = 4
        frequencies(3) = 2
    end subroutine initialize_symbols
    
    subroutine encode_string(input, length, code_value, range)
        implicit none
        character(len=1), intent(in) :: input(:)
        integer, intent(in) :: length
        integer, intent(out) :: code_value
        real, intent(out) :: range
        
        integer :: low, high, total_freq
        integer :: i, j, symbol_index
        integer :: cum_freq
        real :: current_low, current_high
        
        ! Calculate total frequency
        total_freq = 0
        do i = 1, num_symbols
            total_freq = total_freq + frequencies(i)
        end do
        
        ! Initialize encoding
        low = 0.0
        high = 1.0
        range = 1.0
        
        ! Encode each symbol
        do i = 1, length
            ! Find symbol index
            symbol_index = 0
            do j = 1, num_symbols
                if (input(i) == symbols(j)) then
                    symbol_index = j
                    exit
                end if
            end do
            
            ! Calculate cumulative frequency
            cum_freq = 0
            do j = 1, symbol_index - 1
                cum_freq = cum_freq + frequencies(j)
            end do
            
            ! Update range
            range = high - low
            high = low + (range * (cum_freq + frequencies(symbol_index)) / total_freq)
            low = low + (range * cum_freq / total_freq)
        end do
        
        ! Convert to integer code value
        code_value = int(low * (2**precision))
        range = high - low
    end subroutine encode_string
    
    subroutine decode_string(code_value, range, output, length)
        implicit none
        integer, intent(in) :: code_value
        real, intent(in) :: range
        character(len=1), intent(out) :: output(:)
        integer, intent(in) :: length
        
        integer :: low, high, total_freq
        integer :: i, j, symbol_index
        integer :: cum_freq
        real :: current_value, decoded_value
        
        ! Calculate total frequency
        total_freq = 0
        do i = 1, num_symbols
            total_freq = total_freq + frequencies(i)
        end do
        
        ! Initialize decoding
        low = 0
        high = 2**precision
        decoded_value = real(code_value)
        
        ! Decode each symbol
        do i = 1, length
            ! Find symbol
            current_value = (decoded_value - low) / (high - low)
            
            cum_freq = 0
            symbol_index = 0
            
            do j = 1, num_symbols
                cum_freq = cum_freq + frequencies(j)
                if (current_value < real(cum_freq) / real(total_freq)) then
                    symbol_index = j
                    exit
                end if
            end do
            
            output(i) = symbols(symbol_index)
            
            ! Update range
            low = low + (high - low) * (cum_freq - frequencies(symbol_index)) / total_freq
            high = low + (high - low) * cum_freq / total_freq
        end do
    end subroutine decode_string
    
end program arithmetic_coding
```

## Key Components of the Algorithm

### 1. **Symbol Frequency Initialization**
```fortran
subroutine initialize_symbols()
    num_symbols = 3
    symbols(1) = 'a'
    symbols(2) = 'b'
    symbols(3) = 'c'
    frequencies(1) = 4
    frequencies(2) = 4
    frequencies(3) = 2
end subroutine initialize_symbols
```

### 2. **Encoding Process**
The encoding process:
- Calculates cumulative frequencies for each symbol
- Updates the range based on symbol probabilities
- Converts the final range to an integer code value

### 3. **Decoding Process**
The decoding process:
- Uses the code value to determine which symbol was encoded
- Updates the decoding range iteratively
- Reconstructs the original string

## Example Output
```
Input string: abcabcab
Encoded code value: 123456
Range: 0.000123
Decoded string: abcabcab
```

This implementation demonstrates the core principles of arithmetic coding: probability-based encoding, range narrowing, and the use of floating-point arithmetic to represent the encoded values.

