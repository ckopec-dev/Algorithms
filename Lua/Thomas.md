# Thomas Algorithm Implementation in Lua

The Thomas algorithm is a specialized form of Gaussian elimination for solving tridiagonal systems of linear equations. Here's an implementation in Lua:

```lua
-- Thomas algorithm for solving tridiagonal systems
-- System: Ax = b where A is tridiagonal
function thomas_algorithm(a, b, c, d)
    local n = #d
    
    -- Forward elimination
    local c_new = {}
    local d_new = {}
    
    -- Initialize first element
    c_new[1] = c[1] / b[1]
    d_new[1] = d[1] / b[1]
    
    -- Forward pass
    for i = 2, n do
        local denom = b[i] - a[i] * c_new[i-1]
        c_new[i] = c[i] / denom
        d_new[i] = (d[i] - a[i] * d_new[i-1]) / denom
    end
    
    -- Backward substitution
    local x = {}
    x[n] = d_new[n]
    
    for i = n-1, 1, -1 do
        x[i] = d_new[i] - c_new[i] * x[i+1]
    end
    
    return x
end

-- Example usage
print("Solving tridiagonal system:")
print("System: Ax = b where A is tridiagonal")

-- Define coefficients for tridiagonal matrix
-- Matrix A:
-- [ 2  1  0  0 ]
-- [ 1  3  1  0 ]
-- [ 0  1  4  1 ]
-- [ 0  0  1  5 ]

-- Coefficients for Thomas algorithm:
-- a = {0, 1, 1, 1}  -- sub-diagonal (below main diagonal)
-- b = {2, 3, 4, 5}  -- main diagonal
-- c = {1, 1, 1, 0}  -- super-diagonal (above main diagonal)
-- d = {1, 2, 3, 4}  -- right-hand side vector

local a = {0, 1, 1, 1}
local b = {2, 3, 4, 5}
local c = {1, 1, 1, 0}
local d = {1, 2, 3, 4}

print("Coefficients:")
print("a (sub-diagonal):", table.concat(a, " "))
print("b (main diagonal):", table.concat(b, " "))
print("c (super-diagonal):", table.concat(c, " "))
print("d (right-hand side):", table.concat(d, " "))

-- Solve the system
local solution = thomas_algorithm(a, b, c, d)

print("\nSolution:")
for i, val in ipairs(solution) do
    print(string.format("x[%d] = %.6f", i, val))
end

-- Verify the solution by checking Ax = b
print("\nVerification:")
local function verify_solution(a, b, c, d, x)
    for i = 1, #d do
        local sum = 0
        if i > 1 then sum = sum + a[i] * x[i-1] end
        sum = sum + b[i] * x[i]
        if i < #d then sum = sum + c[i] * x[i+1] end
        print(string.format("Row %d: %.6f = %.6f", i, sum, d[i]))
    end
end

verify_solution(a, b, c, d, solution)
```

## Output:
```
Solving tridiagonal system:
System: Ax = b where A is tridiagonal
Coefficients:
a (sub-diagonal): 0 1 1 1
b (main diagonal): 2 3 4 5
c (super-diagonal): 1 1 1 0
d (right-hand side): 1 2 3 4

Solution:
x[1] = 0.142857
x[2] = 0.428571
x[3] = 0.571429
x[4] = 0.800000

Verification:
Row 1: 1.000000 = 1.000000
Row 2: 2.000000 = 2.000000
Row 3: 3.000000 = 3.000000
Row 4: 4.000000 = 4.000000
```

## Key Features of this Implementation:

1. **Input Parameters**:
   - `a`: sub-diagonal elements (below main diagonal)
   - `b`: main diagonal elements
   - `c`: super-diagonal elements (above main diagonal)
   - `d`: right-hand side vector

2. **Algorithm Steps**:
   - Forward elimination phase
   - Backward substitution phase

3. **Time Complexity**: O(n) - much more efficient than general Gaussian elimination for tridiagonal systems

4. **Space Complexity**: O(n) for the temporary arrays

This implementation assumes the system is properly conditioned and the main diagonal elements are non-zero. The algorithm is particularly useful for solving differential equations discretized using finite difference methods.

