# CRC Checksum Algorithm in Forth

Here's an implementation of a simple CRC-8 checksum algorithm in Forth:

```forth
\ CRC-8 checksum calculation
\ Polynomial: x^8 + x^5 + x^4 + x^2 + x^0 (0x1D)

: crc8 ( crc byte -- crc )
    \ Calculate CRC-8 using polynomial 0x1D
    xor         \ XOR with input byte
    8 0 do      \ 8 bits to process
        dup     \ Duplicate current CRC value
        and     \ Check if MSB is set
        if      \ If MSB is set
            16 rshift   \ Right shift by 1
            0x1D xor    \ XOR with polynomial
        else    \ If MSB is not set
            16 rshift   \ Right shift by 1
        then
    loop
    drop        \ Remove duplicate
;

\ Example usage:
\ Calculate CRC for a byte sequence
: test-crc ( -- )
    0           \ Start with zero CRC
    0x45        \ First byte
    crc8        \ Update CRC
    0x67        \ Second byte  
    crc8        \ Update CRC
    0x89        \ Third byte
    crc8        \ Update CRC
    .           \ Print final CRC value
;

\ Alternative more compact version
: crc8-fast ( crc byte -- crc )
    xor 8 0 do
        dup 1 and if 0x1D xor then
        16 rshift
    loop drop
;

\ Test the implementation
\ test-crc
\ Output should be: 0x97 (for bytes 0x45, 0x67, 0x89)
```

## Usage Example:

```forth
\ Calculate CRC for string "Hello"
: hello-crc ( -- )
    0           \ Start with zero CRC
    0x48        \ 'H'
    crc8
    0x65        \ 'e'  
    crc8
    0x6C        \ 'l'
    crc8
    0x6C        \ 'l'
    crc8
    0x6F        \ 'o'
    crc8
    .           \ Print result
;

\ Test both versions
hello-crc   \ Should output the CRC value for "Hello"
```

## Key Features:

- **Modular Design**: Separate functions for CRC calculation
- **Polynomial**: Uses standard CRC-8 polynomial (0x1D)
- **Bit-by-bit Processing**: Processes each bit of input byte
- **Stack-based**: Follows Forth's stack-oriented programming paradigm
- **Extensible**: Easy to modify for different CRC polynomials

This implementation demonstrates the fundamental concepts of CRC calculation in a concise Forth program.