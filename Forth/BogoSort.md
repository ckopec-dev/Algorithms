# BogoSort Algorithm in Forth

Here's an implementation of the BogoSort (also known as "Stupid Sort") algorithm in Forth:

```forth
\ BogoSort implementation in Forth

\ Function to check if array is sorted
: sorted? ( addr len -- flag )
    dup 1 <= if drop true exit then
    0 do
        over i cells + @ over i 1+ cells + @ < if
            drop false leave
        then
    loop
    drop true ;

\ Function to shuffle array elements randomly
: shuffle ( addr len -- )
    dup 1 > if
        0 do
            dup rand mod cells +      \ Get random index
            over i cells + @          \ Get element at current position
            swap i cells + !          \ Store at random position
            over i cells + @          \ Get element at random position
            swap i cells + !          \ Store at current position
        loop
    then
    drop ;

\ BogoSort algorithm
: bogosort ( addr len -- )
    begin
        sorted? not
    while
        shuffle
    repeat ;

\ Example usage:
\ Create an array of 5 elements
create test-array 5 cells allot

\ Initialize array with random values
test-array 0 30 rand mod 1+ !     \ First element
test-array 1 30 rand mod 1+ !     \ Second element
test-array 2 30 rand mod 1+ !     \ Third element
test-array 3 30 rand mod 1+ !     \ Fourth element
test-array 4 30 rand mod 1+ !     \ Fifth element

\ Print array before sorting
cr ." Before sorting:" 
5 0 do
    test-array i cells + @ . space
loop cr

\ Sort the array
test-array 5 bogosort

\ Print array after sorting
cr ." After sorting:"
5 0 do
    test-array i cells + @ . space
loop cr
```

## How it works:

1. **`sorted?`** - Checks if the array is sorted in ascending order
2. **`shuffle`** - Randomly shuffles the elements in the array
3. **`bogosort`** - Main algorithm that repeatedly shuffles until sorted

## Key Features:

- Uses `cells +` to access array elements by their memory offset
- Implements random shuffling using the built-in `rand` function
- Continues sorting until `sorted?` returns true
- Works with any sized array (as long as it's at least 1 element)

## Note:
BogoSort has an average time complexity of O(n!) and is primarily used for educational purposes to demonstrate inefficient algorithms. The random nature means it can theoretically take infinite time to complete, though in practice it will eventually sort the array given enough time.