# Newton-Raphson Method in Forth

Here's an implementation of the Newton-Raphson method for finding roots of equations in Forth:

```forth
\ Newton-Raphson method implementation
\ Finds root of f(x) = 0 using x_{n+1} = x_n - f(x_n)/f'(x_n)

\ Example function: f(x) = x^2 - 2 (root is sqrt(2))
: f ( x -- y )
    dup * 2 - ;  \ x^2 - 2

\ Derivative of f(x): f'(x) = 2x
: df ( x -- y )
    2 * ;        \ 2x

\ Newton-Raphson iteration step
: newton-step ( x -- x_new )
    >r           \ Save current x
    r@ f         \ f(x)
    r@ df        \ f'(x)
    /            \ f(x)/f'(x)
    r> -         \ x - f(x)/f'(x)
    ;

\ Newton-Raphson method with convergence check
: newton-raphson ( x0 epsilon max-iter -- root )
    >r           \ Save epsilon and max-iter
    >r           \ Save initial guess
    0            \ Initialize iteration counter
    
    begin
        r@         \ Get current guess
        newton-step \ Apply Newton-Raphson step
        r>         \ Get epsilon
        2dup       \ Duplicate current and previous values
        -          \ Difference
        fabs       \ Absolute value
        <          \ Check if difference < epsilon
        r>         \ Get max-iter
        1+         \ Increment counter
        over       \ Duplicate counter
        <          \ Check if iterations < max-iter
        and          \ Both conditions must be true
    while
        2swap      \ Swap previous and current values
    repeat
    
    2drop        \ Remove unused values
    ;

\ Example usage:
\ Find sqrt(2) using Newton-Raphson
\ Initial guess: 1.0, epsilon: 0.000001, max iterations: 100
\ 1.0 0.000001 100 newton-raphson .  \ Should output approximately 1.414213

\ Alternative implementation with explicit loop
: find-root ( x0 epsilon -- root )
    >r           \ Save epsilon
    >r           \ Save initial guess
    
    begin
        r@         \ Get current guess
        newton-step \ Apply Newton-Raphson step
        r>         \ Get epsilon
        2dup       \ Duplicate current and previous values
        -          \ Difference
        fabs       \ Absolute value
        <          \ Check if difference < epsilon
    until
    
    2drop        \ Clean up stack
    ;

\ Test example: Find root of x^2 - 2 = 0
\ 1.0 0.000001 find-root .  \ Should output approximately 1.414213
```

## How it works:

1. **Function definition**: `f` represents the function we want to find roots for
2. **Derivative**: `df` is the derivative of the function
3. **Iteration step**: `newton-step` implements the Newton-Raphson formula
4. **Main algorithm**: `newton-raphson` handles convergence checking and iteration
5. **Usage**: Call with initial guess, tolerance, and maximum iterations

## Example usage:
```forth
1.0 0.000001 100 newton-raphson .  \ Finds sqrt(2) ≈ 1.414213
```

This implementation finds the root of x² - 2 = 0, which is √2, demonstrating the method's effectiveness in converging to the correct solution.

