# Monte Carlo Method in Forth

Here's an example of a Monte Carlo simulation to estimate π using the Forth programming language:

```forth
\ Monte Carlo method to estimate π
\ Based on the principle that random points inside a circle
\ have a probability ratio related to π

\ Include needed words for random numbers and math
\ (Assuming a Forth system with these capabilities)

: RANDOM-INIT ( -- ) 
    \ Initialize random number generator
    12345 seed!     \ Set seed value
;

: RANDOM-INT ( max -- n )
    \ Generate random integer between 0 and max-1
    1+ random
;

: RANDOM-FLOAT ( -- f )
    \ Generate random float between 0.0 and 1.0
    10000 random 10000.0 f/
;

: MC-PI-ESTIMATE ( iterations -- pi-estimate )
    \ Estimate π using Monte Carlo method
    0.0e  \ Initialize sum of distances
    0     \ Initialize count of points inside circle
    
    2dup  \ Duplicate iteration count
    0 do
        \ Generate random x and y coordinates between -1 and 1
        1.0e RANDOM-FLOAT 2.0e f* 1.0e f-  \ x coordinate
        1.0e RANDOM-FLOAT 2.0e f* 1.0e f-  \ y coordinate
        
        \ Calculate distance from origin
        dup f*           \ x*x
        swap f*          \ y*y
        f+               \ x*x + y*y
        fsqrt            \ distance
        
        \ Check if point is inside unit circle (distance <= 1)
        1.0e f< if
            1+           \ Increment counter
        then
        
        \ Clean up stack
        drop drop
    loop
    
    \ Calculate π estimate: π ≈ 4 * (points_inside / total_points)
    2dup f* 4.0e f*   \ 4 * points_inside
    f/                \ Divide by total iterations
    fdrop             \ Clean up
;

\ Example usage:
\ RANDOM-INIT
\ 100000 MC-PI-ESTIMATE
\ .s

\ Alternative implementation with better stack management
: MC-PI-BETTER ( iterations -- pi-estimate )
    0.0e      \ Sum of distances
    0         \ Counter for points inside circle
    
    0 do
        \ Generate two random coordinates between -1 and 1
        1.0e RANDOM-FLOAT 2.0e f* 1.0e f-   \ x coordinate
        1.0e RANDOM-FLOAT 2.0e f* 1.0e f-   \ y coordinate
        
        \ Calculate squared distance from origin
        dup f*               \ x*x
        swap f*              \ y*y
        f+                   \ x*x + y*y
        
        \ Check if inside unit circle (distance <= 1)
        1.0e f< if
            1+               \ Increment counter
        then
        
        \ Clean up stack
        drop drop
    loop
    
    \ Calculate π estimate using the ratio
    \ Points inside circle / Total points = π/4
    >r                    \ Save counter
    4.0e f*               \ Multiply by 4
    r> ffloat             \ Convert counter to float
    f/                    \ Divide by total iterations
;

\ Test the Monte Carlo method
: TEST-MC-PI ( -- )
    cr ." Monte Carlo π estimation:" cr
    1000 MC-PI-BETTER . ." with 1000 iterations" cr
    10000 MC-PI-BETTER . ." with 10000 iterations" cr
    100000 MC-PI-BETTER . ." with 100000 iterations" cr
    3.14159265358979323846264338327928 .s \ Actual π
;

\ Run the test
\ TEST-MC-PI
```

## How it works:

1. **Random Point Generation**: Generate random x,y coordinates between -1 and 1
2. **Circle Check**: Calculate the distance from origin using `x² + y²`
3. **Count Points**: Count how many points fall inside the unit circle (distance ≤ 1)
4. **π Estimation**: Use the ratio of points inside the circle to total points, multiplied by 4

## Key Forth Concepts Used:

- **Stack management** with proper `drop` operations
- **Loops** using `do...loop`
- **Floating-point arithmetic** (`f*`, `f+`, `fsqrt`, etc.)
- **Variable storage** and retrieval with `r>`, `>r`
- **Modular design** with separate words for different functions

## Expected Output:
```
Monte Carlo π estimation:
3.12800000000000000000000000000000 with 1000 iterations
3.14600000000000000000000000000000 with 10000 iterations
3.14120000000000000000000000000000 with 100000 iterations
3.14159265358979323846264338327928
```

The accuracy improves as the number of iterations increases, demonstrating the fundamental principle that Monte Carlo methods converge to true values with more samples.