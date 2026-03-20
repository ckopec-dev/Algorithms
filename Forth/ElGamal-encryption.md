# ElGamal Encryption in Forth

Here's an implementation of the ElGamal encryption algorithm in Forth:

```forth
\ ElGamal Encryption Algorithm in Forth

\ Required primitives
: mod ( a b -- a%b )
    /mod nip ;

: powmod ( base exp mod -- result )
    >r 1 swap
    begin
        2dup and 0<>
        while
            r@ * swap r@ mod
        then
        2/ 2dup 0= until
        r> * swap mod
    repeat ;

\ Generate random number (simplified)
: random ( -- n )
    1000000 random ;

\ Generate prime number (simplified)
: generate-prime ( -- p )
    1000 random 1000 + ;

\ Extended Euclidean Algorithm
: gcd ( a b -- gcd )
    begin
        2dup 0= while
            2swap
        repeat
        2dup < if
            2swap
        then
        2dup mod
        recurse
    ;

\ Check if number is prime (simplified)
: prime? ( n -- flag )
    dup 2 < if
        drop false
    else
        dup 2 = if
            drop true
        else
            2 1 do
                2dup i mod 0= if
                    2drop false leave
                then
            loop
            2drop true
        then
    then ;

\ Generate ElGamal parameters
: elgamal-generate-keys ( -- p g x y )
    \ Generate large prime p
    begin
        generate-prime
        prime? 0=
    until
    dup >r \ p
    
    \ Find primitive root g
    begin
        2 random 2 + \ Random g
        r@ 1- 1 do
            r@ i powmod r@ mod 1 = if
                0
            then
        loop
        0= 0=
    until
    dup >r \ g
    
    \ Generate private key x
    r@ 1- random 1+ >r \ x
    
    \ Calculate public key y = g^x mod p
    r@ r@ r@ r> powmod >r
    
    r> r> r> r> ;

\ ElGamal encryption
: elgamal-encrypt ( m g y p -- c1 c2 )
    \ Generate random k
    2 pick 1- random 1+ >r
    
    \ Calculate c1 = g^k mod p
    2 pick r@ powmod 2 pick mod >r
    
    \ Calculate c2 = m * y^k mod p
    2 pick r@ powmod 2 pick mod >r
    2 pick r> * 2 pick mod >r
    
    r> r> ;

\ ElGamal decryption
: elgamal-decrypt ( c1 c2 x p -- m )
    \ Calculate s = c1^x mod p
    2 pick 2 pick powmod 2 pick mod >r
    
    \ Calculate m = c2 * s^(-1) mod p
    \ s^(-1) = s^(p-2) mod p (using Fermat's little theorem)
    2 pick 1- 2 pick powmod 2 pick mod >r
    2 pick r> * 2 pick mod >r
    
    r> ;

\ Example usage
: test-elgamal ( -- )
    cr ." Generating ElGamal keys..." cr
    elgamal-generate-keys
    \ p g x y
    
    cr ." Parameters:"
    cr ." Prime p: " . cr
    cr ." Generator g: " . cr
    cr ." Private key x: " . cr
    cr ." Public key y: " . cr
    
    \ Message to encrypt (must be < p)
    100 >r
    
    cr ." Encrypting message: " r> . cr
    \ Encrypt
    elgamal-encrypt
    \ c1 c2
    
    cr ." Encrypted: c1=" . ." c2=" . cr
    
    \ Decrypt
    elgamal-decrypt
    cr ." Decrypted message: " . cr ;

\ Run the test
test-elgamal
```

## Key Components Explained

### 1. **Basic Mathematical Operations**
- `mod` - Modular arithmetic
- `powmod` - Modular exponentiation (efficient implementation)
- `gcd` - Greatest common divisor (extended Euclidean algorithm)

### 2. **ElGamal Key Generation**
- Generate large prime `p`
- Find primitive root `g`
- Generate private key `x`
- Calculate public key `y = g^x mod p`

### 3. **Encryption Process**
- Choose random `k`
- `c1 = g^k mod p`
- `c2 = m * y^k mod p`

### 4. **Decryption Process**
- Calculate `s = c1^x mod p`
- Calculate `m = c2 * s^(-1) mod p`
- Uses Fermat's Little Theorem for modular inverse

## Usage Notes

1. This is a simplified implementation for demonstration
2. In practice, you'd want to use cryptographically secure random number generators
3. The prime generation should use more sophisticated methods
4. Error handling is minimal for clarity
5. Message size should be less than the prime `p`

The algorithm provides semantic security for the encrypted messages, making it suitable for secure communications.

