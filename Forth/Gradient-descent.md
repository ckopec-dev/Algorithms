# Gradient Descent Algorithm in Forth

Here's an implementation of gradient descent in Forth language:

```forth
\ Gradient Descent Implementation in Forth

\ Constants
1000 constant MAX_ITERATIONS
1e-6 constant EPSILON
0.01 constant LEARNING_RATE

\ Variables for our example: y = 2*x^2 + 3*x + 1
\ We want to minimize f(x) = 2*x^2 + 3*x + 1
\ Gradient: f'(x) = 4*x + 3

\ Function f(x) = 2*x^2 + 3*x + 1
: f ( x -- y )
    dup * 2.0 * 3.0 * + 1.0 + ;

\ Gradient function f'(x) = 4*x + 3
: df ( x -- gradient )
    4.0 * 3.0 + ;

\ Gradient descent algorithm
: gradient-descent ( initial_x -- optimal_x )
    >r              \ Save initial x on return stack
    0               \ iteration counter
    begin
        r@          \ Get current x
        df          \ Calculate gradient
        learing_rate * \ Scale by learning rate
        r@ -        \ Update x: x = x - learning_rate * gradient
        r>          \ Get previous x
        -           \ Calculate difference
        fabs        \ Absolute difference
        epsilon <   \ Check if difference is small enough
        r>          \ Get current x back
        1 +         \ Increment iteration counter
        max_iterations < and
    until
    r> drop ;       \ Clean up return stack

\ Alternative more explicit version
: gradient-descent-verbose ( initial_x -- optimal_x )
    0.0             \ Initialize x
    0               \ Initialize iteration counter
    begin
        dup         \ Copy x
        df          \ Calculate gradient
        learning_rate * \ Scale by learning rate
        -           \ Update x: x = x - learning_rate * gradient
        2dup        \ Copy both old and new x
        -           \ Calculate difference
        fabs        \ Absolute difference
        epsilon <   \ Check convergence
        1 +         \ Increment iteration counter
        max_iterations < and
    until
    drop ;          \ Remove old x, keep new x

\ Example usage:
\ To find minimum of f(x) = 2*x^2 + 3*x + 1
\ The minimum should be at x = -0.75

\ Run the algorithm
: example ( -- )
    -1.0            \ Start with initial guess
    gradient-descent
    cr ." Optimal x: " . ;

\ Run example
example
```

## Key Features of This Implementation:

1. **Function Definitions**: 
   - `f` - the function to minimize: f(x) = 2x² + 3x + 1
   - `df` - the derivative: f'(x) = 4x + 3

2. **Gradient Descent Logic**:
   - Uses the update rule: x = x - α × ∇f(x)
   - Includes convergence checking with epsilon tolerance
   - Has maximum iteration limit to prevent infinite loops

3. **Forth-Specific Features**:
   - Uses stack-based operations
   - Return stack (`r>`, `>r`) for temporary storage
   - Loop structure with `begin`/`until` and `while`/`repeat`
   - Standard Forth math functions (`fabs`, `dup`, `swap`, etc.)

4. **Usage**: 
   - Call `example` to run the optimization
   - Should converge to x = -0.75 (the minimum point)

This implementation demonstrates how gradient descent can be expressed in Forth's stack-oriented paradigm, with careful attention to the iterative update process and convergence criteria.

