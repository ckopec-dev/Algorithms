```fsharp
// Newton-Raphson Method Implementation in F#

// Function to find root using Newton-Raphson method
let newtonRaphson f df x0 tolerance maxIterations =
    let rec iterate x iteration =
        if iteration >= maxIterations then
            failwith "Maximum iterations reached"
        
        let fx = f x
        let dfx = df x
        
        // Check for zero derivative
        if abs dfx < 1e-15 then
            failwith "Derivative too close to zero"
        
        // Newton-Raphson formula: x_new = x - f(x)/f'(x)
        let xNew = x - fx / dfx
        
        // Check for convergence
        if abs (xNew - x) < tolerance then
            xNew
        else
            iterate xNew (iteration + 1)
    
    iterate x0 0

// Example: Find root of f(x) = x^2 - 4 (root is 2 or -2)
let f x = x * x - 4.0
let df x = 2.0 * x  // Derivative of f(x)

// Test with initial guess of 3.0
let result = newtonRaphson f df 3.0 1e-10 100
printfn "Root found: %f" result

// Example 2: Find root of f(x) = x^3 - 2x - 5 (root is approximately 2.0946)
let f2 x = x ** 3.0 - 2.0 * x - 5.0
let df2 x = 3.0 * x ** 2.0 - 2.0

let result2 = newtonRaphson f2 df2 2.0 1e-10 100
printfn "Root found: %f" result2

// Example 3: Find root of f(x) = cos(x) - x (transcendental equation)
let f3 x = cos x - x
let df3 x = -sin x - 1.0

let result3 = newtonRaphson f3 df3 0.5 1e-10 100
printfn "Root found: %f" result3
```

This implementation includes:

1. **Generic Newton-Raphson function** that takes:
   - `f`: the function to find root of
   - `df`: the derivative of the function
   - `x0`: initial guess
   - `tolerance`: convergence threshold
   - `maxIterations`: maximum number of iterations

2. **Error handling** for:
   - Maximum iterations reached
   - Zero derivative (which would cause division by zero)

3. **Three examples** demonstrating:
   - Finding square root of 4
   - Finding root of cubic equation
   - Solving transcendental equation

The algorithm uses tail recursion for efficiency and includes proper convergence checking to ensure reliable results.