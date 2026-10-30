# Euclidean Algorithm in Forth

Here's an implementation of the Euclidean algorithm (for finding the Greatest Common Divisor) in Forth:

```forth
\ Euclidean algorithm for GCD
: gcd ( a b -- gcd )
    begin
        2dup >
    while
        2swap
    repeat
    begin
        0 <>
    while
        2dup mod
        2swap
        drop
    repeat
    nip ;

\ Alternative implementation using recursion
: gcd-rec ( a b -- gcd )
    dup 0=
    if
        drop
    else
        2dup mod
        swap
        gcd-rec
    then ;

\ Example usage:
\ 48 18 gcd . \ Output: 6
\ 17 13 gcd . \ Output: 1
\ 100 25 gcd . \ Output: 25
```

## How it works:

The algorithm repeatedly applies the principle that `gcd(a,b) = gcd(b, a mod b)` until one number becomes zero.

**Example trace for gcd(48, 18):**
1. `48 18` → `18 48` (swap because 48 > 18)
2. `18 48` → `18 12` (48 mod 18 = 12)  
3. `18 12` → `12 6` (18 mod 12 = 6)
4. `12 6` → `6 0` (12 mod 6 = 0)
5. Return `6`

## Key Forth concepts used:
- `begin...while...repeat` - loop structure
- `2dup` - duplicate top two stack items
- `2swap` - swap the top two pairs of stack items  
- `mod` - modulo operation
- `nip` - remove the second item from stack
- `drop` - remove top item from stack