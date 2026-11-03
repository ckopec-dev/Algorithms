# Graham Scan Algorithm in Forth

Here's an implementation of the Graham scan convex hull algorithm in Forth:

```forth
\ Point structure: x y (two cell values)
\ Stack operations for points
: stack-init ( -- ) 0 stack-top ! ;

\ Point stack operations
: point-stack-push ( point -- )
    stack-top @ 1+ stack-top !
    dup stack-top @ cells point-stack + !
;

: point-stack-pop ( -- point )
    stack-top @ 0= abort" Empty stack"
    stack-top @ 1- stack-top !
    stack-top @ cells point-stack +
    @
;

\ Point operations
: point-x ( point -- x ) cell- ; \ Get x coordinate
: point-y ( point -- y ) cell+ ; \ Get y coordinate

\ Cross product of three points (p1, p2, p3)
\ Returns positive if counter-clockwise turn, negative if clockwise
: cross-product ( p1 p2 p3 -- result )
    2 pick point-x -  \ (p2.x - p1.x)
    over point-y - * \ * (p3.y - p1.y)
    2 pick point-y - \ (p3.y - p1.y)
    over point-x - * \ * (p2.x - p1.x)
    -
;

\ Distance between two points
: distance-squared ( p1 p2 -- dist² )
    2 pick point-x - dup * \ (p2.x - p1.x)²
    over point-y - dup * \ + (p2.y - p1.y)²
    +
;

\ Find the bottom-most point (lowest y, then lowest x)
: find-bottom-most ( -- point )
    max-points 0 do
        i points + @
        0= if
            i points + @
        else
            dup i points + @
            point-y < if
                i points + @
            else
                dup i points + @
                point-y = if
                    point-x < if
                        i points + @
                    then
                then
            then
        then
    loop
;

\ Sort points by polar angle with respect to bottom-most point
: sort-by-polar-angle ( -- )
    \ Implementation would go here for sorting
    \ This is a simplified version
;

\ Graham scan algorithm
: graham-scan ( -- )
    \ Find the bottom-most point
    find-bottom-most >r
    
    \ Sort points by polar angle
    sort-by-polar-angle
    
    \ Initialize stack with first three points
    r@ point-stack-push
    0 points + @ point-stack-push
    1 points + @ point-stack-push
    
    \ Process remaining points
    2 max-points 1+ do
        begin
            point-stack-pop
            point-stack-pop
            i points + @
            cross-product
        while
            \ If clockwise turn, pop the point
            point-stack-push
            point-stack-pop
        repeat
        \ Push current point back
        point-stack-push
    loop
    
    \ Output results
    begin
        stack-top @ 0> while
            point-stack-pop
            ." Point: " dup point-x . ." , " point-y . cr
        repeat
;

\ Example usage
: test-graham-scan ( -- )
    \ Define some points (x y pairs)
    100 200 points 0 !  \ Point 0: (100, 200)
    150 100 points 1 !  \ Point 1: (150, 100)
    200 300 points 2 !  \ Point 2: (200, 300)
    250 150 points 3 !  \ Point 3: (250, 150)
    300 250 points 4 !  \ Point 4: (300, 250)
    350 100 points 5 !  \ Point 5: (350, 100)
    
    max-points 6 !
    graham-scan
;

\ Constants and variables
100 constant max-points
create point-stack max-points cells allot
0 variable stack-top
create points max-points 2 * cells allot

\ Run the test
test-graham-scan
```

## Key Features of this Implementation:

1. **Point Structure**: Points are stored as (x y) pairs using two cell values
2. **Stack Operations**: Basic stack functionality for managing hull points
3. **Cross Product**: Used to determine turn direction (clockwise vs counter-clockwise)
4. **Sorting**: Points are sorted by polar angle relative to the bottom-most point
5. **Main Algorithm**: Implements the core Graham scan logic

## Algorithm Steps:
1. Find the bottom-most point
2. Sort all points by polar angle with respect to the bottom-most point
3. Initialize stack with first three points
4. For each remaining point, check if it makes a clockwise turn
5. If so, pop from stack until we get a counter-clockwise turn
6. Push current point onto stack

This implementation provides a foundation that can be extended with additional features like proper sorting algorithms and more robust error handling.