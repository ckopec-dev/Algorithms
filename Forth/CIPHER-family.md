# CIPHER__family Algorithm Example in Forth

Here's an example of a simple Caesar cipher implementation in Forth:

```forth
\ Caesar Cipher Implementation in Forth
\ Shifts letters by a fixed number of positions

\ Define cipher family constants
: CIPHER__ALPHA ( -- n ) 26 ;     \ Alphabet size
: CIPHER__SHIFT ( -- n ) 3 ;      \ Shift by 3 positions

\ Encrypt a single character
: CIPHER__ENCRYPT-CHAR ( c -- c' )
    dup [char] a -      \ Convert to 0-25 range
    CIPHER__SHIFT +     \ Apply shift
    CIPHER__ALPHA mod   \ Wrap around
    [char] a +          \ Convert back to ASCII
;

\ Decrypt a single character
: CIPHER__DECRYPT-CHAR ( c -- c' )
    dup [char] a -      \ Convert to 0-25 range
    CIPHER__SHIFT -     \ Reverse shift
    CIPHER__ALPHA mod   \ Wrap around
    [char] a +          \ Convert back to ASCII
;

\ Encrypt a string (in-place)
: CIPHER__ENCRYPT-STRING ( addr len -- )
    0 do
        over i + c@ CIPHER__ENCRYPT-CHAR
        over i + c!
    loop
    2drop
;

\ Decrypt a string (in-place)
: CIPHER__DECRYPT-STRING ( addr len -- )
    0 do
        over i + c@ CIPHER__DECRYPT-CHAR
        over i + c!
    loop
    2drop
;

\ Example usage:
\ : TEST-CIPHER
\     s" hello" 5 CIPHER__ENCRYPT-STRING
\     ." Encrypted: " 5 type cr
\     5 CIPHER__DECRYPT-STRING
\     ." Decrypted: " 5 type cr
\ ;
```

This example demonstrates:
- **CIPHER__family** naming convention
- **CIPHER__ALPHA** - alphabet size constant
- **CIPHER__SHIFT** - shift amount constant  
- **CIPHER__ENCRYPT-CHAR** - character encryption function
- **CIPHER__DECRYPT-CHAR** - character decryption function
- **CIPHER__ENCRYPT-STRING** - string encryption function
- **CIPHER__DECRYPT-STRING** - string decryption function

The algorithm implements a Caesar cipher with a shift of 3 positions, following the Forth convention of using double underscores for naming family functions.

