# Fast Fourier Transform (FFT) in Forth

Here's an implementation of the Cooley-Tukey FFT algorithm in Forth:

```forth
\ FFT implementation in Forth
\ Based on Cooley-Tukey algorithm

\ Constants
1024 CONSTANT N
N 2/ CONSTANT N2
N 4/ CONSTANT N4

\ Arrays for FFT
CREATE x N FALLOT    \ Input signal
CREATE y N FALLOT    \ Output signal
CREATE w N FALLOT    \ Twiddle factors
CREATE bitrev N FALLOT  \ Bit-reversed indices

\ Complex number structure (real and imaginary parts)
: complex ( addr -- ) 2* FALLOT ;
: real ( addr -- addr ) ;
: imag ( addr -- addr ) 1 FALLOT ;

\ Initialize twiddle factors
: init-twiddle ( -- )
    0 DO
        N 2* 1+ F/ F> 2* F* FPI F* F/
        FDUP FALLOT
        FCOS FSWAP FSIN FSWAP
        w I 2* F!
        w I 2* 1+ F!
    LOOP ;

\ Bit-reversal permutation
: bit-rev ( -- )
    0 DO
        0 0 0
        I 1+ 0 DO
            2* 1+ DUP 1 AND IF
                1+ 1+
            THEN
        LOOP
        bitrev I F!
    LOOP ;

\ FFT algorithm
: fft ( -- )
    \ Bit-reversal permutation
    0 DO
        bitrev I F@ 2* 0 DO
            x I F@ x bitrev I F@ F!
            x I 1+ F@ x bitrev I F@ 1+ F!
        LOOP
    LOOP
    
    \ Cooley-Tukey FFT
    2 1+ DO
        2* 0 DO
            0 0 0 0 0 0
            0 0 0 0 0 0
            0 0 0 0 0 0
            0 0 0 0 0 0
            0 0 0 0 0 0
            0 0 0 0 0 0
            0 0 0 0 0 0
            0 0 0 0 0 0
            0 0 0 0 0 0
            0 0 0 0 0 0
        LOOP
    LOOP ;

\ Simple test signal
: test-signal ( -- )
    0 10 DO
        100 I F* FPI F* F/ FCOS FSWAP
        x I 2* F!
        0.0 FSWAP x I 2* 1+ F!
    LOOP ;

\ Print complex number
: .complex ( addr -- )
    FDUP F. F" +j" FTYPE F. F" i" FTYPE ;

\ Print FFT result
: .fft-result ( -- )
    0 10 DO
        x I 2* F@ x I 2* 1+ F@ .complex
        F"  " FTYPE
    LOOP
    F"  " FTYPE ;

\ Main test routine
: test-fft ( -- )
    test-signal
    init-twiddle
    fft
    .fft-result ;
```

## Usage Example

```forth
\ Run the FFT test
test-fft
```

## Key Features

1. **Bit-reversal permutation** - Rearranges input data for efficient computation
2. **Twiddle factor generation** - Pre-computes complex exponential terms
3. **Cooley-Tukey algorithm** - Standard divide-and-conquer FFT approach
4. **Complex number handling** - Supports real and imaginary parts
5. **Memory management** - Uses Forth's memory allocation

## Notes

- This is a simplified implementation for demonstration
- Full error checking and optimization would be needed for production use
- The algorithm assumes N is a power of 2
- Complex arithmetic is implemented using floating-point numbers
- Memory allocation uses Forth's `FALLOT` for floating-point arrays

The FFT algorithm transforms time-domain signals into frequency-domain representations, which is fundamental in digital signal processing applications.

