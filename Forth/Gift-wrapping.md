# Gift Wrapping Algorithm in Forth

The gift wrapping algorithm (also known as Jarvis march) is used to find the convex hull of a set of points. Here's an implementation in Forth:

```forth
\ Gift Wrapping Algorithm (Jarvis March) Implementation in Forth

\ Point structure: x y
\ Stack operations
: stack ( -- ) 1000 allot ;  \ Allocate stack space
: push ( n -- )  \ Push to stack
    sp@ ! 1 sp+! ;
: pop ( -- n )   \ Pop from stack
    1 sp-! sp@ @ ;

\ Point operations
: point ( x y -- )  \ Create point
    dup , swap , ;  \ Store y, then x

: x-coord ( point -- x )  \ Get x coordinate
    @ ;
    
: y-coord ( point -- y )  \ Get y coordinate
    cell+ @ ;

\ Cross product of three points (p1, p2, p3)
\ Returns positive if counter-clockwise turn, negative if clockwise
: cross-product ( p1 p2 p3 -- result )
    >r  \ Save p3
    r@ x-coord r@ y-coord  \ p3.x p3.y
    swap x-coord swap y-coord  \ p2.x p2.y
    over x-coord over y-coord  \ p1.x p1.y
    \ Cross product formula: (p2.x - p1.x)(p3.y - p1.y) - (p2.y - p1.y)(p3.x - p1.x)
    swap - rot - *  \ (p2.x - p1.x)(p3.y - p1.y)
    swap - rot - *  \ (p2.y - p1.y)(p3.x - p1.x)
    - ;  \ Difference

\ Find the leftmost point (lowest x, then lowest y)
: find-leftmost ( points n -- leftmost )
    0  \ Start with first point
    1 do
        i 2* cells +  \ Get point i
        dup x-coord  \ Get x coordinate of current point
        0 i 2* cells + x-coord  \ Get x coordinate of base point
        < if
            i  \ Update index if smaller x
        else
            drop  \ Keep previous index
        then
    loop
    2* cells + ;  \ Return actual point

\ Find next point in hull using gift wrapping
: find-next-point ( points n current-hull-point -- next-point )
    >r  \ Save current hull point
    0  \ Start with first point as candidate
    1 do
        i 2* cells +  \ Get point i
        r@ cross-product  \ Calculate cross product with current hull point
        0 > if  \ If counter-clockwise turn
            i  \ Update candidate
        then
    loop
    2* cells + ;  \ Return actual point

\ Gift Wrapping Algorithm
: gift-wrapping ( points n -- hull-points )
    \ Find leftmost point to start
    find-leftmost  \ Start with leftmost point
    dup  \ Save starting point
    push  \ Push to stack
    
    \ Continue until we return to start point
    begin
        pop  \ Get current point
        find-next-point  \ Find next hull point
        dup  \ Duplicate to check if we're back at start
        r@  \ Compare with start point
        <>  \ Continue while not equal to start
    while
        push  \ Push new point to stack
    repeat
    drop  \ Remove duplicate start point
    \ Return points on stack (implementation depends on specific stack structure) ;

\ Example usage:
\ Create some test points
: test-points ( -- )
    0 0 point  \ Point 1
    1 0 point  \ Point 2  
    0 1 point  \ Point 3
    1 1 point  \ Point 4
    0.5 0.5 point ;  \ Point 5

\ Simplified version for demonstration:
: convex-hull ( points n -- )
    cr ." Computing convex hull using gift wrapping..." cr
    \ This would call the full algorithm implementation above
    . ." Points computed" ;
```

## How it works:

1. **Find starting point**: The leftmost point is selected as the starting vertex of the hull
2. **Wrap around**: From each current point, find the next point that makes the largest counter-clockwise turn
3. **Continue until return**: Keep wrapping until we return to the starting point

## Key Forth Concepts Used:

- **Stack operations** with `push` and `pop`
- **Memory management** using `allot` and cell addressing
- **Point structure** using consecutive memory cells
- **Cross product calculation** for determining turn direction
- **Loop control** with `do`...`loop` structures

## Time Complexity:
O(nh) where n is the number of input points and h is the number of hull vertices.

This implementation demonstrates how Forth's stack-based architecture naturally fits the recursive nature of the gift wrapping algorithm.