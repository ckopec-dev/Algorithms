# Bezier Curve Generation in Lua

Here's a complete implementation of Bezier curve generation algorithm in Lua:

```lua
-- Bezier Curve Generation Algorithm in Lua

-- Function to calculate binomial coefficient C(n,k)
local function binomial_coefficient(n, k)
    if k == 0 or k == n then return 1 end
    if k > n then return 0 end
    
    local result = 1
    for i = 1, math.min(k, n - k) do
        result = result * (n - i + 1) / i
    end
    return result
end

-- Function to calculate Bezier curve point at parameter t
local function bezier_point(control_points, t)
    local n = #control_points - 1
    local x, y = 0, 0
    
    for i = 1, #control_points do
        local bernstein = binomial_coefficient(n, i-1) * math.pow(1-t, n-(i-1)) * math.pow(t, i-1)
        x = x + control_points[i].x * bernstein
        y = y + control_points[i].y * bernstein
    end
    
    return {x = x, y = y}
end

-- Function to generate Bezier curve points
local function generate_bezier_curve(control_points, num_points)
    local curve_points = {}
    
    for i = 1, num_points do
        local t = (i - 1) / (num_points - 1)
        local point = bezier_point(control_points, t)
        table.insert(curve_points, point)
    end
    
    return curve_points
end

-- Example usage
local function main()
    -- Define control points for a quadratic Bezier curve (3 points)
    local control_points = {
        {x = 0, y = 0},
        {x = 50, y = 100},
        {x = 100, y = 0}
    }
    
    -- Generate 20 points along the curve
    local curve = generate_bezier_curve(control_points, 20)
    
    -- Print the curve points
    print("Bezier Curve Points:")
    for i, point in ipairs(curve) do
        print(string.format("Point %d: (%.2f, %.2f)", i, point.x, point.y))
    end
    
    -- Example with cubic Bezier curve (4 points)
    print("\nCubic Bezier Curve:")
    local cubic_control_points = {
        {x = 0, y = 0},
        {x = 25, y = 100},
        {x = 75, y = 100},
        {x = 100, y = 0}
    }
    
    local cubic_curve = generate_bezier_curve(cubic_control_points, 15)
    
    for i, point in ipairs(cubic_curve) do
        print(string.format("Point %d: (%.2f, %.2f)", i, point.x, point.y))
    end
end

-- Run the example
main()
```

## Key Features of this Implementation:

### 1. **Binomial Coefficient Calculation**
- Computes C(n,k) for Bernstein polynomial coefficients
- Uses optimized calculation to avoid large factorials

### 2. **Bernstein Polynomial Evaluation**
- Implements the mathematical formula for Bezier curves
- Handles any degree of Bezier curve

### 3. **Curve Generation**
- Takes control points and number of output points
- Returns evenly distributed points along the curve

### 4. **Usage Examples**
- Quadratic Bezier curve (3 control points)
- Cubic Bezier curve (4 control points)

## Mathematical Background:

A Bezier curve of degree n is defined by:
```
B(t) = Σ(i=0 to n) C(n,i) * (1-t)^(n-i) * t^i * P_i
```

Where:
- C(n,i) is the binomial coefficient
- t is the parameter (0 ≤ t ≤ 1)
- P_i are the control points

This implementation provides a solid foundation for generating Bezier curves that can be used in graphics applications, animation, or path planning.

