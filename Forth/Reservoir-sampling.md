# Reservoir Sampling Algorithm in Forth

Here's an implementation of the Reservoir Sampling algorithm in Forth, which randomly selects k items from a stream of unknown length:

```forth
\ Reservoir Sampling Implementation in Forth
\ This algorithm selects k random items from a stream of data

\ Constants
10 constant RESERVOIR_SIZE    \ Size of reservoir (number of items to keep)
1000 constant STREAM_LENGTH   \ Length of simulated stream

\ Global variables
create reservoir RESERVOIR_SIZE cells allot  \ Storage for reservoir
variable count                               \ Counter for stream items
variable seed                                \ Random seed

\ Initialize random number generator
: init-random ( -- )
    12345 seed !              \ Set initial seed
;

\ Simple pseudo-random number generator
: random ( -- n )
    seed @ 1664525 * 1013904223 +  \ Linear congruential generator
    dup seed !
    10000 mod                       \ Keep within reasonable range
;

\ Initialize reservoir with first k items
: init-reservoir ( -- )
    0 count !                    \ Reset counter
    
    \ Fill reservoir with first RESERVOIR_SIZE items
    0 RESERVOIR_SIZE 0 do
        i random cells reservoir + !
    loop
;

\ Main reservoir sampling algorithm
: reservoir-sample ( item -- )
    count @ 1+ count !           \ Increment counter
    
    count @ RESERVOIR_SIZE < if
        \ Still filling reservoir
        count @ cells reservoir + !
    else
        \ Replace elements with decreasing probability
        RESERVOIR_SIZE random 0= if
            \ Replace a random element in reservoir
            RESERVOIR_SIZE random cells reservoir + !
        then
    then
;

\ Alternative implementation using the standard algorithm
: reservoir-sample2 ( item -- )
    count @ 1+ count !
    
    count @ RESERVOIR_SIZE < if
        \ Still filling reservoir - add item to reservoir
        count @ cells reservoir + !
    else
        \ Replace with probability 1/count
        count @ random 0= if
            RESERVOIR_SIZE random cells reservoir + !
        then
    fi
;

\ Print reservoir contents
: print-reservoir ( -- )
    cr ." Reservoir contents:" cr
    0 RESERVOIR_SIZE 0 do
        i cells reservoir + @ . space
    loop cr
;

\ Test the algorithm
: test-reservoir-sampling ( -- )
    init-random
    init-reservoir
    
    \ Simulate streaming data
    0 STREAM_LENGTH 1+ 0 do
        i random reservoir-sample
    loop
    
    print-reservoir
;

\ Run the test
test-reservoir-sampling
```

## How it works:

1. **Initialization**: The reservoir is filled with the first k items from the stream
2. **Sampling**: For each subsequent item (n > k), we replace a random element in the reservoir with probability 1/n
3. **Final Result**: After processing all items, the reservoir contains k randomly selected items

## Key features:

- **Memory Efficient**: Only stores k items regardless of stream size
- **Unbiased**: Each item has equal probability of being selected
- **Single Pass**: Processes each item exactly once
- **Streaming**: Works with unknown total stream length

## Example usage:

```forth
\ Initialize and run the algorithm
init-random
init-reservoir
0 100 do
    i random reservoir-sample
loop
print-reservoir
```

This implementation demonstrates the fundamental reservoir sampling algorithm in Forth, suitable for scenarios where you need to randomly sample from a large or infinite data stream while maintaining constant memory usage.