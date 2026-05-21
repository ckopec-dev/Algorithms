# Conjugate Gradient Method in Lua

Here's an implementation of the conjugate gradient method for solving linear systems Ax = b:

```lua
-- Conjugate Gradient Method Implementation
function conjugate_gradient(A, b, x0, max_iter, tolerance)
    local n = #b
    local x = {}
    for i = 1, n do
        x[i] = x0[i] or 0
    end
    
    -- Initialize
    local r = {}
    local p = {}
    local Ap = {}
    
    -- Compute initial residual: r = b - Ax
    for i = 1, n do
        r[i] = b[i]
        local sum = 0
        for j = 1, n do
            sum = sum + A[i][j] * x[j]
        end
        r[i] = r[i] - sum
    end
    
    -- Initialize p = r
    for i = 1, n do
        p[i] = r[i]
    end
    
    local rsold = 0
    for i = 1, n do
        rsold = rsold + r[i] * r[i]
    end
    
    -- Main iteration loop
    for iter = 1, max_iter do
        -- Compute Ap = A * p
        for i = 1, n do
            local sum = 0
            for j = 1, n do
                sum = sum + A[i][j] * p[j]
            end
            Ap[i] = sum
        end
        
        -- Compute alpha = r^T * r / (p^T * Ap)
        local pAp = 0
        for i = 1, n do
            pAp = pAp + p[i] * Ap[i]
        end
        
        if math.abs(pAp) < 1e-15 then
            print("Algorithm terminated: p^T * Ap = 0")
            break
        end
        
        local alpha = rsold / pAp
        
        -- Update x: x = x + alpha * p
        for i = 1, n do
            x[i] = x[i] + alpha * p[i]
        end
        
        -- Update r: r = r - alpha * Ap
        for i = 1, n do
            r[i] = r[i] - alpha * Ap[i]
        end
        
        -- Compute new residual norm
        local rsnew = 0
        for i = 1, n do
            rsnew = rsnew + r[i] * r[i]
        end
        
        -- Check convergence
        if math.sqrt(rsnew) < tolerance then
            print(string.format("Converged after %d iterations", iter))
            return x
        end
        
        -- Compute beta = r_new^T * r_new / r_old^T * r_old
        local beta = rsnew / rsold
        
        -- Update p: p = r + beta * p
        for i = 1, n do
            p[i] = r[i] + beta * p[i]
        end
        
        rsold = rsnew
    end
    
    print(string.format("Max iterations (%d) reached", max_iter))
    return x
end

-- Example usage
function example_usage()
    -- Example system: Ax = b
    -- 3x + 2y - z = 1
    -- 2x - 2y + 4z = -2
    -- -x + 0.5y - z = 0
    
    local A = {
        {3, 2, -1},
        {2, -2, 4},
        {-1, 0.5, -1}
    }
    
    local b = {1, -2, 0}
    local x0 = {0, 0, 0}  -- Initial guess
    
    local max_iter = 100
    local tolerance = 1e-8
    
    print("Solving system Ax = b using Conjugate Gradient Method")
    print("Matrix A:")
    for i = 1, #A do
        print(table.concat(A[i], " "))
    end
    print("Vector b:", table.concat(b, " "))
    
    local solution = conjugate_gradient(A, b, x0, max_iter, tolerance)
    
    print("Solution x:")
    for i = 1, #solution do
        print(string.format("x[%d] = %.6f", i, solution[i]))
    end
    
    -- Verify solution
    print("\nVerification (A * x):")
    for i = 1, #A do
        local sum = 0
        for j = 1, #A do
            sum = sum + A[i][j] * solution[j]
        end
        print(string.format("Row %d: %.6f (expected: %.6f)", i, sum, b[i]))
    end
end

-- Run the example
example_usage()
```

## Key Features of this Implementation:

1. **Input Parameters**:
   - `A`: Coefficient matrix (n×n)
   - `b`: Right-hand side vector (n×1)
   - `x0`: Initial guess (optional, defaults to zero)
   - `max_iter`: Maximum number of iterations
   - `tolerance`: Convergence threshold

2. **Algorithm Steps**:
   - Initialize residual `r = b - Ax`
   - Set search direction `p = r`
   - Iteratively update solution using conjugate direction method
   - Check convergence at each step
   - Return final solution when converged or max iterations reached

3. **Convergence Criteria**:
   - Residual norm falls below tolerance
   - Maximum iterations reached

4. **Numerical Stability**:
   - Checks for zero denominator in alpha calculation
   - Uses appropriate tolerance values

This implementation assumes the matrix A is symmetric and positive definite, which is required for the conjugate gradient method to converge.

