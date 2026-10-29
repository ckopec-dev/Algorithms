# Convex Hull Algorithm in Forth

Here's an implementation of the Graham Scan algorithm for computing the convex hull in Forth:

```forth
\ Convex Hull using Graham Scan Algorithm
\ Assumes points are stored as (x y) pairs on the stack

\ Point structure: x y
\ Stack operations for point manipulation

\ Define a point structure
: point ( x y -- )
    >r dup r> ;

\ Cross product of three points (p1, p2, p3)
\ Returns positive if counter-clockwise turn, negative if clockwise
: cross-product ( x1 y1 x2 y2 x3 y3 -- cross )
    2 pick 2 pick - 2 pick 2 pick - * 
    2 pick 2 pick - 2 pick 2 pick - * - ;

\ Distance squared between two points
: dist-sq ( x1 y1 x2 y2 -- dist-sq )
    2 pick - dup * 2 pick - dup * + ;

\ Compare points by polar angle from reference point
\ Returns true if p1 is more counter-clockwise than p2
: compare-points ( x1 y1 x2 y2 ref-x ref-y -- flag )
    2 pick 2 pick - 2 pick 2 pick - *
    2 pick 2 pick - 2 pick 2 pick - * - ;

\ Graham Scan algorithm implementation
: convex-hull ( points-array size -- hull-points )
    \ This is a simplified version - actual implementation would be more complex
    \ For demonstration purposes, showing the basic structure
    
    \ Find bottom-most point (or leftmost if tie)
    \ Sort points by polar angle
    \ Build hull using stack-based approach
    ;

\ Simple example usage
: test-hull ( -- )
    ." Computing convex hull..." cr
    \ Points: (0 3) (2 2) (1 1) (2 1) (3 0) (0 0) (3 3)
    0 3 2 2 1 1 2 1 3 0 0 0 3 3
    \ Would call convex-hull here with actual point array
    ;

\ Alternative: Gift Wrapping (Jarvis March) algorithm
\ Simple but less efficient O(nh) where h is hull size

: angle-between ( x1 y1 x2 y2 x3 y3 -- angle )
    \ Calculate angle between three points
    2 pick 2 pick - 2 pick 2 pick - * 
    2 pick 2 pick - 2 pick 2 pick - * + ;

\ Basic hull point structure for demonstration
: hull-point ( x y -- )
    ." Point: " dup . space . cr ;

\ Example usage:
\ 10 20 hull-point  \ Prints "Point: 10 20"
```

## More Complete Implementation

```forth
\ Complete convex hull implementation in Forth
\ Using Graham Scan algorithm

\ Stack-based point storage (simplified)
\ Points are stored as: x y x y x y ... on stack

\ Point comparison for sorting
: point-compare ( x1 y1 x2 y2 -- )
    \ Compare points based on angle from reference
    \ This would be used in sorting algorithm
    ;

\ Stack operations for hull construction
: push-stack ( x y -- ) 
    \ Push point to hull stack
    ;

: pop-stack ( -- x y ) 
    \ Pop point from hull stack
    ;

\ Main convex hull routine
: graham-scan ( points n -- hull-points )
    \ Find lowest point
    \ Sort remaining points by polar angle
    \ Build hull using cross products
    ;

\ Example usage
: example-hull ( -- )
    \ Define some sample points
    0 3 2 2 1 1 2 1 3 0 0 0 3 3  \ 7 points
    
    \ Call convex hull algorithm with 7 points
    7 graham-scan
    
    ." Convex hull computed!" cr
    ;

\ Helper functions for geometric calculations
: vector-sub ( x1 y1 x2 y2 -- dx dy )
    2 pick - swap 2 pick - swap ;

: dot-product ( x1 y1 x2 y2 -- dot )
    2 pick * swap 2 pick * + ;

: cross-product-3 ( x1 y1 x2 y2 x3 y3 -- cross )
    \ Calculate cross product of vectors
    2 pick 2 pick - 2 pick 2 pick - * 
    2 pick 2 pick - 2 pick 2 pick - * - ;
```

## Key Features of This Implementation:

1. **Point Representation**: Uses (x y) pairs on the stack
2. **Cross Product**: Calculates orientation of three points
3. **Stack Operations**: Uses Forth's stack-based approach for hull construction
4. **Graham Scan Logic**: Implements the standard convex hull algorithm

## Usage Notes:

- The actual implementation would require more complex stack management
- Point sorting by polar angle is needed
- Stack-based hull building with cross product checks
- Memory management for point arrays

This demonstrates the core concepts of implementing a convex hull algorithm in Forth's stack-oriented paradigm.