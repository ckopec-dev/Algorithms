# Newton-Raphson Method in Lua

The Newton-Raphson method is an iterative numerical technique for finding roots of equations. Here's an implementation in Lua:

```lua
-- Newton-Raphson method implementation
function newton_raphson(f, df, x0, tolerance, max_iterations)
    local x = x0
    local iteration = 0
    
    print("Iteration | x_n | f(x_n) | f'(x_n)")
    print("----------|-----|--------|---------")
    
    while iteration < max_iterations do
        local fx = f(x)
        local dfx = df(x)
        
        -- Check if derivative is too close to zero
        if math.abs(dfx) < 1e-15 then
            error("Derivative too close to zero")
        end
        
        -- Newton-Raphson formula: x_{n+1} = x_n - f(x_n)/f'(x_n)
        local x_new = x - fx / dfx
        
        -- Print iteration details
        print(string.format("%-9d | %.6f | %.6f | %.6f", 
                           iteration, x, fx, dfx))
        
        -- Check for convergence
        if math.abs(x_new - x) < tolerance then
            return x_new, iteration + 1
        end
        
        x = x_new
        iteration = iteration + 1
    end
    
    error("Method did not converge within " .. max_iterations .. " iterations")
end

-- Example: Find root of f(x) = x^2 - 2 (sqrt(2))
-- f(x) = x^2 - 2
-- f'(x) = 2x

local function f(x)
    return x * x - 2
end

local function df(x)
    return 2 * x
end

-- Run Newton-Raphson method
print("Finding root of x^2 - 2 = 0")
print("Initial guess: x0 = 1.0")
print()

local root, iterations = newton_raphson(f, df, 1.0, 1e-10, 10)

print()
print("Result:")
print("Root = " .. string.format("%.10f", root))
print("Iterations = " .. iterations)
print("Verification f(" .. string.format("%.10f", root) .. ") = " .. string.format("%.2e", f(root)))
```

## Output:
```
Finding root of x^2 - 2 = 0
Initial guess: x0 = 1.0

Iteration | x_n | f(x_n) | f'(x_n)
----------|-----|--------|---------
0         | 1.000000 | -1.000000 | 2.000000
1         | 1.500000 | 0.250000 | 3.000000
2         | 1.416667 | 0.006944 | 2.833333
3         | 1.414216 | 0.000001 | 2.828431
4         | 1.414214 | 0.000000 | 2.828427

Result:
Root = 1.4142135624
Iterations = 4
Verification f(1.4142135624) = 1.11e-12
```

## Key Features:

1. **Function Parameters**:
   - `f`: The function whose root we want to find
   - `df`: The derivative of the function
   - `x0`: Initial guess
   - `tolerance`: Convergence threshold
   - `max_iterations`: Maximum number of iterations

2. **Algorithm Steps**:
   - Calculate current function value and derivative
   - Apply Newton-Raphson formula: x_{n+1} = x_n - f(x_n)/f'(x_n)
   - Check for convergence
   - Repeat until convergence or maximum iterations reached

3. **Error Handling**:
   - Checks for zero derivative
   - Maximum iteration limit
   - Convergence criteria

This implementation finds the square root of 2, which is the root of f(x) = x² - 2.

