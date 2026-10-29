# De Boor's Algorithm in Forth

De Boor's algorithm is used for evaluating B-spline curves at given parameter values. Here's an implementation in Forth:

```forth
\ De Boor's Algorithm for B-spline evaluation
\ Input: t (parameter), knot vector, control points, degree
\ Output: point on curve

\ Constants
1000 CONSTANT MAX_DEGREE
1000 CONSTANT MAX_KNOTS
1000 CONSTANT MAX_POINTS

\ Arrays for storage
create knots    MAX_KNOTS 2* floats allot
create points   MAX_POINTS 2* floats allot
create temp     MAX_DEGREE 2* floats allot

\ De Boor's algorithm implementation
: de-boor ( t knot_vec ctrl_pts degree -- x y )
    >r              \ Save degree on return stack
    r@ 1+           \ degree + 1
    0 do
        0.0         \ Initialize result to 0.0
        i 1+        \ j = i + 1
        0 do
            \ Compute blending coefficient
            knots i 2* floats +   \ u_j
            knots i r@ + 2* floats +  \ u_{j+p}
            knots i 1+ 2* floats +  \ u_{j+1}
            knots i r@ + 1+ 2* floats +  \ u_{j+p+1}
            
            \ Check for division by zero
            over over - 0= if
                drop drop drop
                points i 2* floats +  \ Get control point
                2dup 2* floats +  \ Point to store result
                f!  \ Store x coordinate
                points i 1+ 2* floats +  \ Get control point y
                2dup 1+ 2* floats +  \ Point to store result
                f!  \ Store y coordinate
            else
                \ Compute blending coefficient
                tover over - 0= if
                    drop drop 1.0
                else
                    tover over - 
                    tover over - 0= if
                        drop drop 0.0
                    else
                        over over - 2dup f- f/  \ (u-t)/(u_{j+p}-u_j)
                    then
                then
                
                \ Multiply with previous result and add
                temp i 2* floats +  \ Get current result
                f*  \ Multiply by coefficient
                f+  \ Add to accumulated result
            then
        loop
        temp i 2* floats +  \ Store intermediate result
        f!  \ Store x coordinate
        temp i 1+ 2* floats +  \ Get y coordinate
        f!  \ Store y coordinate
    loop
    
    r> drop           \ Clean up return stack
    points 0 floats +  \ Return x coordinate
    points 1 floats +  \ Return y coordinate
;

\ Simplified version for cubic B-splines (degree=3)
: de-boor-cubic ( t knot_vec ctrl_pts -- x y )
    3 0 do
        \ For cubic case, we have 4 control points to work with
        0.0  \ Initialize result
        
        \ Compute the four blending coefficients for cubic case
        knots 12 floats +  \ u_4 (assuming 16 knots)
        knots 8 floats +   \ u_3
        knots 4 floats +   \ u_2
        knots 0 floats +   \ u_1
        
        \ Simplified blending computation for cubic B-spline
        tover over - 0= if
            drop drop 1.0
        else
            tover over - 
            tover over - 0= if
                drop drop 0.0
            else
                over over - 2dup f- f/  \ (u-t)/(u_{j+p}-u_j)
            then
        then
        
        \ Apply to control points and accumulate
        points i 2* floats +  \ Get control point x
        f* f+  \ Add weighted contribution
        
        points i 1+ 2* floats +  \ Get control point y
        f* f+  \ Add weighted contribution
    loop
    
    points 0 floats +  \ Return x coordinate
    points 1 floats +  \ Return y coordinate
;

\ Example usage:
\ Define a simple cubic B-spline with control points
: example-de-boor ( -- )
    \ Set up knots (example uniform knot vector)
    0.0 knots 0 floats + f!
    0.0 knots 2 floats + f!
    0.0 knots 4 floats + f!
    1.0 knots 6 floats + f!
    1.0 knots 8 floats + f!
    1.0 knots 10 floats + f!
    
    \ Set up control points (4 points)
    0.0 points 0 floats + f!
    0.0 points 2 floats + f!
    1.0 points 4 floats + f!
    1.0 points 6 floats + f!
    
    \ Evaluate at t = 0.5
    0.5 0 0 3 de-boor-cubic
    cr . ." X = " . ." Y = " .
;

\ Alternative clean implementation using stack operations:
: de-boor-simple ( t knot_vec ctrl_pts degree -- x y )
    \ This is a simplified version that demonstrates the core concept
    \ In practice, this would be more complex with proper knot handling
    
    \ Get current parameter value
    >r  \ Save parameter on return stack
    
    \ Initialize control point array for interpolation
    r@ 0 do
        \ For demonstration, just return first control point
        points i 2* floats +  \ Get x coordinate of control point i
        points i 1+ 2* floats +  \ Get y coordinate of control point i
    loop
    
    r> drop  \ Clean up return stack
    \ Return result (simplified)
;

\ Usage example:
\ : test-de-boor ( -- )
\     \ Set up sample data
\     0.0 knots 0 floats + f!
\     1.0 knots 2 floats + f!
\     2.0 knots 4 floats + f!
\     3.0 knots 6 floats + f!
\     
\     0.0 points 0 floats + f!
\     1.0 points 2 floats + f!
\     2.0 points 4 floats + f!
\     3.0 points 6 floats + f!
\     
\     \ Evaluate at t = 1.5
\     1.5 knots 0 points 0 3 de-boor-cubic
\     cr . ." Result: " . ." , " .
\ ;
```

## Key Features of This Implementation:

1. **Stack-based**: Uses Forth's stack for parameter passing and intermediate calculations
2. **Floating-point support**: Assumes floating-point operations are available (floats)
3. **Modular design**: Separates knot vector, control points, and degree handling
4. **Return stack usage**: Uses the return stack to preserve parameters during recursion
5. **Memory management**: Allocates arrays for knots and control points

## Notes:

- This is a simplified implementation that demonstrates the concept
- A full implementation would require proper knot interval checking and edge case handling
- The actual De Boor algorithm involves recursive computation of blending coefficients
- Error checking for division by zero and boundary conditions should be added in production code
- Floating-point operations assume Forth system supports `f!`, `f+`, `f*` etc.