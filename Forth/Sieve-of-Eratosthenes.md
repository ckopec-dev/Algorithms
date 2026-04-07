# Sieve of Eratosthenes in Forth

Here's an implementation of the Sieve of Eratosthenes algorithm in Forth:

```forth
\ Sieve of Eratosthenes implementation in Forth

\ Define constants
1000 CONSTANT MAX
MAX 8 / CONSTANT SIEVE-SIZE

\ Create sieve array (bit array)
CREATE SIEVE SIEVE-SIZE ALLOT

\ Initialize sieve array to all 1s (true)
: INIT-SIEVE ( -- )
    SIEVE SIEVE-SIZE 0 DO
        1 SWAP I + C!
    LOOP
;

\ Mark multiples of a prime as composite
: MARK-COMPOSITE ( prime -- )
    DUP 2 < IF EXIT THEN
    2* 0 DO
        DUP I + MAX < IF
            SIEVE I + C@ 0= IF
                1 SWAP I + C!
            THEN
        THEN
    LOOP
;

\ Sieve of Eratosthenes algorithm
: SIEVE-ERATOSTHENES ( -- )
    INIT-SIEVE
    2 MAX SQRT 1+ DO
        SIEVE I + C@ 0= IF
            I MARK-COMPOSITE
        THEN
    LOOP
;

\ Print primes up to MAX
: PRINT-PRIMES ( -- )
    2 MAX 1+ DO
        SIEVE I + C@ 0= IF
            I .
        THEN
    LOOP
    CR
;

\ Example usage
SIEVE-ERATOSTHENES
." Primes up to " MAX .
." : "
PRINT-PRIMES
```

## Alternative Implementation (More Efficient)

```forth
\ More efficient version using bit manipulation

\ Create bit array for numbers 2 to MAX
MAX 8 / 1+ ALLOT CONSTANT BIT-ARRAY

\ Set bit to 1 (prime)
: SET-PRIME ( n -- )
    DUP 2 < IF EXIT THEN
    1 SWAP 1- 8 MOD LSHIFT
    BIT-ARRAY SWAP 1- 8 / + C!
;

\ Clear bit (composite)
: CLEAR-PRIME ( n -- )
    DUP 2 < IF EXIT THEN
    1 SWAP 1- 8 MOD LSHIFT
    0 SWAP 1- 8 / + C!
;

\ Check if bit is set
: IS-PRIME ( n -- flag )
    DUP 2 < IF EXIT THEN
    1 SWAP 1- 8 MOD LSHIFT
    0 SWAP 1- 8 / + C@ AND 0<>
;

\ Sieve algorithm
: ERATOSTHENES ( -- )
    \ Initialize all numbers as prime
    0 0 DO
        0 I + C!
    LOOP
    2 MAX 1+ DO
        I IS-PRIME IF
            I I * MAX < IF
                I I * 0 DO
                    I I + I * + MAX < IF
                        I I + I * + CLEAR-PRIME
                    THEN
                LOOP
            THEN
        THEN
    LOOP
;

\ Print primes
: PRINT-PRIMES ( -- )
    2 MAX 1+ DO
        I IS-PRIME IF
            I .
        THEN
    LOOP
    CR
;
```

## Usage Example

```forth
\ Run the sieve
ERATOSTHENES
PRINT-PRIMES
```

## Key Features

1. **Bit Array Storage**: Uses bit manipulation for efficient memory usage
2. **Initialization**: Sets all numbers as potentially prime (1s)
3. **Marking Composites**: Marks multiples of primes as composite (0s)
4. **Optimization**: Only checks up to √MAX for efficiency
5. **Prime Output**: Prints all found primes

## Time Complexity
- **Time**: O(n log log n)
- **Space**: O(n)

The algorithm efficiently finds all prime numbers up to a given limit by iteratively marking composite numbers, leaving only primes unmarked.

