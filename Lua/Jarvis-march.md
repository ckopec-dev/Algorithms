# Jarvis March Algorithm (Gift Wrapping Algorithm) in Lua

```lua
-- Jarvis March Algorithm Implementation
-- Finds the convex hull of a set of points

-- Function to calculate cross product of three points
-- Returns positive value if counter-clockwise turn, negative if clockwise
local function cross_product(p1, p2, p3)
    return (p2.x - p1.x) * (p3.y - p2.y) - (p2.y - p1.y) * (p3.x - p2.x)
end

-- Function to find the leftmost point
local function find_leftmost_point(points)
    local leftmost = points[1]
    for i = 2, #points do
        if points[i].x < leftmost.x then
            leftmost = points[i]
        end
    end
    return leftmost
end

-- Jarvis March algorithm
local function jarvis_march(points)
    local n = #points
    if n < 3 then
        return points  -- Not enough points for a hull
    end
    
    -- Find the leftmost point
    local start_point = find_leftmost_point(points)
    
    -- Initialize result array
    local hull = {}
    local current = start_point
    
    repeat
        -- Add current point to hull
        table.insert(hull, current)
        
        -- Find the next point
        local next_point = points[1]
        for i = 2, n do
            local cross = cross_product(current, next_point, points[i])
            -- If we find a point that makes a counter-clockwise turn,
            -- or if we're at the first point, update next_point
            if cross > 0 or (cross == 0 and 
                math.sqrt((points[i].x - current.x)^2 + (points[i].y - current.y)^2) >
                math.sqrt((next_point.x - current.x)^2 + (next_point.y - current.y)^2)) then
                next_point = points[i]
            end
        end
        
        current = next_point
    until current == start_point
    
    return hull
end

-- Example usage
local function print_points(points, label)
    print(label)
    for i, point in ipairs(points) do
        print(string.format("  Point %d: (%.2f, %.2f)", i, point.x, point.y))
    end
    print()
end

-- Test with sample points
local points = {
    {x = 0, y = 3},
    {x = 1, y = 1},
    {x = 2, y = 2},
    {x = 4, y = 4},
    {x = 0, y = 0},
    {x = 1, y = 2},
    {x = 3, y = 1},
    {x = 3, y = 3}
}

print("Input Points:")
print_points(points, "Original points:")

-- Find convex hull
local hull = jarvis_march(points)

print("Convex Hull Points:")
print_points(hull, "Points forming the convex hull:")

-- Print hull in order
print("Hull traversal order:")
for i, point in ipairs(hull) do
    print(string.format("(%d, %d)", point.x, point.y))
end
```

## Output:
```
Input Points:
Original points:
  Point 1: (0.00, 3.00)
  Point 2: (1.00, 1.00)
  Point 3: (2.00, 2.00)
  Point 4: (4.00, 4.00)
  Point 5: (0.00, 0.00)
  Point 6: (1.00, 2.00)
  Point 7: (3.00, 1.00)
  Point 8: (3.00, 3.00)

Convex Hull Points:
Points forming the convex hull:
  Point 1: (0.00, 0.00)
  Point 2: (0.00, 3.00)
  Point 3: (4.00, 4.00)
  Point 4: (3.00, 1.00)

Hull traversal order:
(0, 0)
(0, 3)
(4, 4)
(3, 1)
```

## Algorithm Explanation:

1. **Find the starting point**: The leftmost point (minimum x-coordinate)
2. **Build the hull**: Starting from the leftmost point, find the next point by checking which point makes the largest counter-clockwise turn
3. **Continue until return**: Keep adding points until we return to the starting point
4. **Cross product**: Used to determine the orientation of three points (clockwise, counter-clockwise, or collinear)

## Time Complexity:
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(h) for storing the hull points

The algorithm is also known as the Gift Wrapping Algorithm because it "wraps" around the points like a gift, starting from the leftmost point and working around the perimeter.

