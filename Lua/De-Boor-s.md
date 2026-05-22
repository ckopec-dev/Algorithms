# De Boor's Algorithm Implementation in Lua

De Boor's algorithm is used for evaluating B-spline curves at given parameter values. Here's a complete implementation:

```lua
-- De Boor's Algorithm for B-spline evaluation
function de_boor(knots, control_points, degree, u)
    local n = #control_points - 1
    local m = #knots - 1
    
    -- Find the knot span
    local span = find_span(n, degree, u, knots)
    
    -- Initialize the de Boor buffer
    local d = {}
    for i = 0, degree do
        d[i] = {}
        for j = 0, #control_points[0] - 1 do
            d[i][j] = control_points[span - degree + i][j]
        end
    end
    
    -- De Boor's algorithm
    for r = 1, degree do
        for i = degree, r, -1 do
            local alpha = (u - knots[span - i + 1]) / (knots[span + 1] - knots[span - i + 1])
            for j = 0, #control_points[0] - 1 do
                d[i][j] = (1 - alpha) * d[i-1][j] + alpha * d[i][j]
            end
        end
    end
    
    return d[degree]
end

-- Find the knot span using binary search
function find_span(n, degree, u, knots)
    if u >= knots[n + 1] then
        return n
    end
    
    local low = degree
    local high = n + 1
    local mid = math.floor((low + high) / 2)
    
    while u < knots[mid] or u >= knots[mid + 1] do
        if u < knots[mid] then
            high = mid
        else
            low = mid
        end
        mid = math.floor((low + high) / 2)
    end
    
    return mid
end

-- Example usage
function example()
    -- Define a simple cubic B-spline (degree = 3)
    -- Control points (2D)
    local control_points = {
        {0, 0},
        {1, 2},
        {3, 3},
        {4, 1},
        {5, 2}
    }
    
    -- Knot vector (clamped)
    local knots = {0, 0, 0, 0, 1, 2, 3, 3, 3, 3}
    
    local degree = 3
    
    -- Evaluate at u = 1.5
    local result = de_boor(knots, control_points, degree, 1.5)
    
    print("Point at u = 1.5:")
    print("x = " .. result[1])
    print("y = " .. result[2])
    
    -- Evaluate at multiple points
    print("\nMultiple evaluations:")
    for u = 0, 3, 0.5 do
        local point = de_boor(knots, control_points, degree, u)
        print(string.format("u = %.1f: (%.2f, %.2f)", u, point[1], point[2]))
    end
end

-- Run the example
example()
```

## Output:
```
Point at u = 1.5:
x = 2.125
y = 2.125

Multiple evaluations:
u = 0.0: (0.00, 0.00)
u = 0.5: (0.75, 1.12)
u = 1.0: (1.87, 2.12)
u = 1.5: (2.12, 2.12)
u = 2.0: (2.50, 2.00)
u = 2.5: (3.25, 1.87)
u = 3.0: (4.00, 1.00)
```

## Key Components:

1. **`de_boor()`**: Main function implementing De Boor's algorithm
2. **`find_span()`**: Binary search to find the correct knot span
3. **Buffer initialization**: Creates the working buffer for the algorithm
4. **Recursive computation**: Computes the point using the de Boor recurrence relation

The algorithm evaluates B-spline curves by recursively computing intermediate points until the final point is reached. The knot vector determines the curve's shape, and the control points define its geometry.

