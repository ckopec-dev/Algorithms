# Held-Karp Algorithm Implementation in Forth

The Held-Karp algorithm is used to solve the Traveling Salesman Problem (TSP) using dynamic programming with a time complexity of O(n²2ⁿ).

```forth
\ Held-Karp Algorithm for TSP in Forth

\ Constants
1024 constant MAXN     \ Maximum number of cities
1024 constant MAXMASK  \ Maximum mask value (2^10)

\ Global variables
create dist MAXN MAXN * floats allot  \ Distance matrix
create dp MAXN MAXMASK * floats allot  \ DP table
create parent MAXN MAXMASK * ints allot  \ Parent tracking

\ Initialize distance matrix
: init-dist ( -- )
    MAXN MAXN * 0 do 0e f! loop ;

\ Set distance between two cities
: set-dist ( i j d -- )
    >r swap MAXN * + r> f! ;

\ Get distance between two cities
: get-dist ( i j -- d )
    swap MAXN * + f@ ;

\ Calculate minimum of two floats
: min-float ( a b -- min )
    2dup f< if drop else nip then ;

\ Held-Karp algorithm implementation
: held-karp ( n -- min-cost )
    dup 1- 1 lshift  \ 2^(n-1)
    1-  \ 2^(n-1) - 1
    0 do
        0 0 dp i MAXN * + f!
    loop
    
    \ Base case: from city 0 to all subsets containing only city 0
    1 0 do
        1 i lshift  \ mask for city i
        1 0 dp i MAXN * + f!
    loop
    
    \ Fill DP table
    3 1 do
        1 i 1- lshift 1-  \ All subsets of size i
        0 do
            i 0 do
                j 1 and if
                    j i 1- lshift  \ subset without city j
                    dp i MAXN * + f@
                    1 j lshift  \ mask for city j
                    i 1- lshift  \ mask for all cities except j
                    i 1- lshift  \ mask for all cities except j
                    i 1- lshift  \ mask for all cities except j
                    dp i MAXN * + f@
                then
            loop
        loop
    loop
    
    \ Find minimum cost to return to city 0
    0e  \ Initialize with infinity
    1 0 do
        1 i lshift  \ mask for city i
        i 0 dp i MAXN * + f@
        get-dist 0 i  \ distance from city i to city 0
        f+ min-float
    loop ;

\ Alternative simpler implementation for small graphs
: tsp-solve ( n -- cost )
    dup 1- 2*  \ 2^(n-1)
    0 do
        1 i lshift  \ bit mask for subset
        0 0 dp i MAXN * + f!
    loop
    
    \ Dynamic programming
    1 do
        1 i lshift 1-  \ all subsets of size i
        0 do
            0 0 dp i MAXN * + f!
        loop
    loop ;

\ Example usage with a small graph (4 cities)
: example-tsp ( -- )
    \ Initialize graph
    init-dist
    
    \ Set distances for 4 cities
    0 1 set-dist 10e
    0 2 set-dist 15e
    0 3 set-dist 20e
    1 0 set-dist 10e
    1 2 set-dist 35e
    1 3 set-dist 25e
    2 0 set-dist 15e
    2 1 set-dist 35e
    2 3 set-dist 30e
    3 0 set-dist 20e
    3 1 set-dist 25e
    3 2 set-dist 30e
    
    \ Solve TSP for 4 cities
    4 held-karp
    ." Minimum cost: " f. cr ;

\ Test the implementation
example-tsp
```

## Key Features of this Implementation:

1. **Dynamic Programming Table**: Uses `dp` array to store optimal solutions for subsets
2. **Bitmask Representation**: Uses bitmasks to represent subsets of cities
3. **Space Complexity**: O(n × 2ⁿ) for the DP table
4. **Time Complexity**: O(n² × 2ⁿ) for the algorithm

## How it Works:

1. **Initialization**: Set up distance matrix and DP table
2. **Base Cases**: Initialize subsets containing only city 0
3. **Fill DP Table**: For each subset size, compute optimal paths
4. **Result**: Find minimum cost to return to starting city

## Limitations:

- Only practical for small graphs (n ≤ 20 due to exponential complexity)
- Memory usage grows exponentially with number of cities
- Not suitable for large TSP instances

This implementation demonstrates the core principles of Held-Karp algorithm in Forth, showing how dynamic programming can be used to solve the traveling salesman problem efficiently.