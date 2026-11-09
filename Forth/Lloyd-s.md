# Lloyd's Algorithm in Forth

Lloyd's algorithm (K-means clustering) implemented in Forth. This example demonstrates the core clustering process.

```forth
\ Lloyd's Algorithm Implementation in Forth

\ Constants
1000 CONSTANT MAX_ITERATIONS
20   CONSTANT MAX_POINTS
10   CONSTANT MAX_CENTROIDS
2    CONSTANT DIMENSIONS

\ Memory areas for data
CREATE points      MAX_POINTS DIMENSIONS * FLOATS ALLOT
CREATE centroids   MAX_CENTROIDS DIMENSIONS * FLOATS ALLOT
CREATE assignments MAX_POINTS   FLOATS ALLOT
CREATE counts      MAX_CENTROIDS FLOATS ALLOT

\ Helper functions
: float+ ( addr n -- addr' )  FLOATS + ;
: f@ ( addr -- f )  FSWAP F@ ;
: f! ( f addr -- )  FSWAP F! ;

\ Initialize centroids randomly
: init-centroids ( -- )
    MAX_CENTROIDS 0 DO
        DIMENSIONS 0 DO
            100 RANDOM F.
            centroids I DIMENSIONS * J + F!
        LOOP
    LOOP ;

\ Calculate Euclidean distance between two points
: distance ( p1 p2 -- dist )
    0.0E FSWAP
    DIMENSIONS 0 DO
        points over I + F@   \ Get point1 coordinate
        points over I + F@   \ Get point2 coordinate
        F-                   \ Difference
        F*                   \ Square it
        F+                   \ Add to sum
    LOOP
    Fsqrt ;                  \ Take square root

\ Assign points to nearest centroid
: assign-points ( -- )
    MAX_POINTS 0 DO
        0.0E FSWAP                    \ Initialize minimum distance
        0.0E FSWAP                    \ Initialize best centroid index
        
        MAX_CENTROIDS 0 DO
            points I DIMENSIONS * F@  \ Get point coordinates
            centroids I DIMENSIONS * F@
            distance                  \ Calculate distance
            F2OVER F< IF              \ If closer than current minimum
                FSWAP FSWAP           \ Swap to get new minimum
                I F.                  \ Store centroid index
            THEN
        LOOP
        
        assignments I F!              \ Store assignment
    LOOP ;

\ Update centroids based on current assignments
: update-centroids ( -- )
    MAX_CENTROIDS 0 DO
        0.0E FSWAP                    \ Initialize sum for each dimension
        0.0E FSWAP                    \ Initialize count
        MAX_POINTS 0 DO
            assignments I F@          \ Get assignment
            I F= IF                   \ If point belongs to this centroid
                points I DIMENSIONS * F@  \ Add coordinate
                F+
                1.0E F+               \ Increment count
            THEN
        LOOP
        FSWAP F/                      \ Calculate mean
        centroids I DIMENSIONS * F!   \ Store new centroid
    LOOP ;

\ Main Lloyd's algorithm loop
: lloyds-algorithm ( -- )
    init-centroids
    MAX_ITERATIONS 0 DO
        assign-points
        update-centroids
    LOOP ;

\ Example usage
: test-lloyds ( -- )
    ." Initializing centroids..." CR
    init-centroids
    
    ." Running Lloyd's algorithm..." CR
    lloyds-algorithm
    
    ." Final centroids:" CR
    MAX_CENTROIDS 0 DO
        DIMENSIONS 0 DO
            centroids I DIMENSIONS * J + F@
            F. SPACE
        LOOP
        CR
    LOOP ;
```

## Key Features:

1. **Memory Management**: Uses `FLOATS ALLOT` for dynamic memory allocation
2. **Distance Calculation**: Implements Euclidean distance formula
3. **Assignment Logic**: Finds nearest centroid for each point
4. **Centroid Update**: Recalculates centroids based on assignments
5. **Iteration Control**: Runs for a maximum number of iterations

## Usage:
```forth
test-lloyds \ Run the clustering example
```

This implementation demonstrates the core concepts of Lloyd's algorithm in a Forth environment, though it's simplified for clarity and doesn't include advanced features like convergence checking or input validation.