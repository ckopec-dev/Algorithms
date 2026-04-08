## Bloom‑filter example in **Forth**

Below is a self‑contained Forth implementation of a Bloom filter that:

* stores the bit vector in a plain memory area (`bloom-bits`);
* uses three independent hash functions (simple variations of FNV‑1a);
* provides `bloom-add` to insert an element and `bloom-test` to query membership;
* works for any string passed as a counted string (`addr len`).

Feel free to copy‑paste the code into a Forth system (e.g., Gforth, SwiftForth, VFX) and run the test at the end.

```forth
\ --------------------------------------------------------------
\  Bloom filter parameters (tweak to suit your needs)
\ --------------------------------------------------------------
3 CONSTANT #hashes          \ number of hash functions
128 CONSTANT bloom-bits     \ size of the bit vector (must be multiple of cell*8)

\ --------------------------------------------------------------
\  Bit‑vector helpers
\ --------------------------------------------------------------
: cell-bits   ( -- n ) 8 * ;          \ bits per cell
: bloom-size  ( -- n ) bloom-bits cell-bits / ;   \ number of cells needed

CREATE bloom-storage  bloom-size cells ALLOT   \ raw memory for the bits

\ set a bit (index from 0..bloom-bits-1)
: set-bit   ( u -- )
    dup bloom-bits u< IF
        abort" Bloom filter: bit index out of range"
    THEN
    bloom-bits /mod          \ ( cell-index bit-offset )
    >r                       \ save cell-index on return stack
    1 swap lshift            \ mask with 1 shifted left by bit-offset
    bloom-storage @ + r> cells +  \ address of the cell
    dup @ swap or swap !     \ set the bit
    rdrop ;
\ test a bit (returns true/false flag)
: test-bit  ( u -- f )
    dup bloom-bits u< IF
        abort" Bloom filter: bit index out of range"
    THEN
    bloom-bits /mod          \ ( cell-index bit-offset )
    >r
    1 swap lshift            \ mask
    bloom-storage @ + r> cells +  \ address of the cell
    @ swap and 0<> ;         \ non‑zero -> flag true

\ --------------------------------------------------------------
\  Very simple hash functions (FNV‑1a variations)
\ --------------------------------------------------------------
\ FNV offset basis and prime (32‑bit)
2166136261 CONSTANT fnv-offset
16777619   CONSTANT fnv-prime

: fnv1a-base  ( addr u -- n )
    fnv-offset
    0 ?DO
        i c@ xor fnv-prime *
    LOOP ;

: hash1  ( addr u -- n )   fnv1a-base bloom-bits MOD ;
: hash2  ( addr u -- n )   fnv1a-base 0x9e3779b9 + bloom-bits MOD ;
: hash3  ( addr u -- n )   fnv1a-base 0x85ebca6b + bloom-bits MOD ;

\ --------------------------------------------------------------
\  Core Bloom‑filter operations
\ --------------------------------------------------------------
: bloom-clear  ( -- )
    bloom-storage bloom-size cells ERASE ;

: bloom-add    ( addr u -- )
    #hashes 0 DO
        OVER OVER          \ (addr u addr u) for each hash
        I hash1 set-bit
        I hash2 set-bit
        I hash3 set-bit
    LOOP
    2DROP ;

: bloom-test   ( addr u -- f )
    #hashes 0 DO
        OVER OVER
        I hash1 test-bit 0= IF
            2DROP false LEAVE
        THEN
        I hash2 test-bit 0= IF
            2DROP false LEAVE
        THEN
        I hash3 test-bit 0= IF
            2DROP false LEAVE
        THEN
    LOOP
    2DROP true ;

\ --------------------------------------------------------------
\  Demo / simple test
\ --------------------------------------------------------------
: demo-bloom  ( -- )
    ." Clearing filter..." cr
    bloom-clear

    ." Adding: hello, world, forth, bloom" cr
    s" hello" bloom-add
    s" world" bloom-add
    s" forth" bloom-add
    s" bloom" bloom-add

    ." Testing known elements:" cr
    s" hello" bloom-test IF ." hello  -> present" cr ELSE ." hello  -> absent (false negative!)" cr THEN
    s" world" bloom-test IF ." world  -> present" cr ELSE ." world  -> absent (false negative!)" cr THEN
    s" forth" bloom-test IF ." forth  -> present" cr ELSE ." forth  -> absent (false negative!)" cr THEN
    s" bloom" bloom-test IF ." bloom  -> present" cr ELSE ." bloom  -> absent (false negative!)" cr THEN

    ." Testing unknown elements (may give false positives):" cr
    s" foo"   bloom-test IF ." foo    -> possibly present (false positive)" cr ELSE ." foo    -> definitely absent" cr THEN
    s" bar"   bloom-test IF ." bar    -> possibly present (false positive)" cr ELSE ." bar    -> definitely absent" cr THEN
    s" baz123"bloom-test IF ." baz123 -> possibly present (false positive)" cr ELSE ." baz123 -> definitely absent" cr THEN ;

\ Run the demo
demo-bloom
```

### How it works

1. **Bit vector** – `bloom-storage` holds `bloom-bits` bits (here 128 bits → 2 cells on a 32‑bit Forth, 1 cell on a 64‑bit Forth).  
2. **Hash functions** – Three independent 32‑bit FNV‑1a hashes with different constants; each result is reduced modulo `bloom-bits` to obtain a bit index.  
3. **Insertion** – For each hash we call `set-bit` to turn the corresponding bit on.  
4. **Query** – We test the three bits; if any is off we know the element is *definitely not* present; if all are on we report *possibly present* (the usual Bloom‑filter false‑positive case).  

### Tweaking the filter

| Parameter | Meaning | Typical choice |
|-----------|---------|----------------|
| `#hashes` | Number of hash functions (`k`) | `k = (m/n) * ln(2)` where `m` = bits, `n` = expected items |
| `bloom-bits` | Size of the bit vector (`m`) | Larger `m` → lower false‑positive rate |
| Hash functions | Quality of randomness | Replace with stronger hash (e.g., MurmurHash3, xxHash) if needed |

Feel free to adjust the constants, add more hash functions, or store the bit vector in a larger array for bigger capacities. The core words (`bloom-add`, `bloom-test`) remain unchanged. Happy Forth‑coding!