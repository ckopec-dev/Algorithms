# Monte Carlo Method Example in Lua

Here's a complete example of using the Monte Carlo method to estimate the value of π:

```lua
-- Monte Carlo Method to Estimate π
function estimate_pi(n)
    local inside_circle = 0
    
    -- Seed the random number generator
    math.randomseed(os.time())
    
    -- Run n simulations
    for i = 1, n do
        -- Generate random point in unit square [0,1] x [0,1]
        local x = math.random()
        local y = math.random()
        
        -- Calculate distance from origin
        local distance = math.sqrt(x*x + y*y)
        
        -- Check if point is inside unit circle
        if distance <= 1 then
            inside_circle = inside_circle + 1
        end
    end
    
    -- Estimate π using the ratio of areas
    -- Area of circle = π * r² = π (since r = 1)
    -- Area of square = 4 * r² = 4 (since side = 2r = 2)
    -- Ratio = π/4
    local pi_estimate = 4 * (inside_circle / n)
    
    return pi_estimate
end

-- Example usage
print("Monte Carlo π Estimation")
print("========================")

local iterations = 1000000
local estimated_pi = estimate_pi(iterations)

print("Number of iterations: " .. iterations)
print("Estimated π: " .. string.format("%.6f", estimated_pi))
print("Actual π: " .. string.format("%.6f", math.pi))
print("Error: " .. string.format("%.6f", math.abs(estimated_pi - math.pi)))

-- Run multiple trials to show convergence
print("\nConvergence demonstration:")
for _, trials in ipairs({1000, 10000, 100000, 1000000}) do
    local result = estimate_pi(trials)
    print("Trials: " .. trials .. " → π ≈ " .. string.format("%.6f", result))
end
```

## How it works:

1. **Random Sampling**: Generate random points within a unit square (0,0) to (1,1)
2. **Circle Test**: For each point, calculate the distance from the origin using the Pythagorean theorem
3. **Counting**: If the distance is ≤ 1, the point is inside the unit circle
4. **Estimation**: Use the ratio of points inside the circle to total points to estimate π

## Mathematical Principle:

- The ratio of points inside the circle to total points approximates π/4
- Since the circle has area π and the square has area 4, the ratio is π/4
- Therefore: π ≈ 4 × (points inside circle / total points)

## Sample Output:
```
Monte Carlo π Estimation
========================
Number of iterations: 1000000
Estimated π: 3.141892
Actual π: 3.141593
Error: 0.000299

Convergence demonstration:
Trials: 1000 → π ≈ 3.140000
Trials: 10000 → π ≈ 3.141600
Trials: 100000 → π ≈ 3.141560
Trials: 1000000 → π ≈ 3.141892
```

This demonstrates how the Monte Carlo method converges to the true value of π as the number of iterations increases.

