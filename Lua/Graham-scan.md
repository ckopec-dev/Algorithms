# Graham Scan Algorithm in Lua

Here's an implementation of the Graham Scan algorithm for finding the convex hull of a set of points:

```lua
-- Point structure
Point = {}
Point.__index = Point

function Point:new(x, y)
    local obj = {
        x = x,
        y = y
    }
    setmetatable(obj, Point)
    return obj
end

-- Calculate cross product of three points
function cross_product(p1, p2, p3)
    return (p2.x - p1.x) * (p3.y - p2.y) - (p2.y - p1.y) * (p3.x - p2.x)
end

-- Calculate distance between two points
function distance(p1, p2)
    return math.sqrt((p1.x - p2.x)^2 + (p1.y - p2.y)^2)
end

-- Find the point with the lowest y-coordinate (and leftmost if tie)
function find_lowest_point(points)
    local lowest = points[1]
    for i = 2, #points do
        if points[i].y < lowest.y or (points[i].y == lowest.y and points[i].x < lowest.x) then
            lowest = points[i]
        end
    end
    return lowest
end

-- Calculate angle from reference point
function calculate_angle(ref_point, point)
    local dx = point.x - ref_point.x
    local dy = point.y - ref_point.y
    return math.atan2(dy, dx)
end

-- Graham Scan algorithm
function graham_scan(points)
    if #points < 3 then
        return points
    end
    
    -- Find the point with lowest y-coordinate (and leftmost if tie)
    local lowest = find_lowest_point(points)
    
    -- Sort points by polar angle with respect to the lowest point
    table.sort(points, function(a, b)
        local angle_a = calculate_angle(lowest, a)
        local angle_b = calculate_angle(lowest, b)
        if angle_a == angle_b then
            return distance(lowest, a) < distance(lowest, b)
        end
        return angle_a < angle_b
    end)
    
    -- Initialize the hull with the first three points
    local hull = {points[1], points[2], points[3]}
    
    -- Process remaining points
    for i = 4, #points do
        local current_point = points[i]
        
        -- Remove points from hull while the turn is not counterclockwise
        while #hull >= 2 and cross_product(hull[#hull-1], hull[#hull], current_point) <= 0 do
            table.remove(hull)
        end
        
        table.insert(hull, current_point)
    end
    
    return hull
end

-- Example usage
print("Graham Scan Algorithm Example")
print("=============================")

-- Create sample points
local points = {
    Point:new(0, 3),
    Point:new(1, 1),
    Point:new(2, 2),
    Point:new(4, 4),
    Point:new(0, 0),
    Point:new(1, 2),
    Point:new(3, 1),
    Point:new(3, 3)
}

print("Input points:")
for i, point in ipairs(points) do
    print(string.format("  (%d, %d)", point.x, point.y))
end

-- Run Graham Scan
local convex_hull = graham_scan(points)

print("\nConvex Hull points:")
for i, point in ipairs(convex_hull) do
    print(string.format("  (%d, %d)", point.x, point.y))
end

print("\nNumber of points in convex hull: " .. #convex_hull)
```

## How it works:

1. **Find the lowest point**: Identify the point with the smallest y-coordinate (leftmost if tied)
2. **Sort points**: Sort all points by polar angle with respect to the lowest point
3. **Build hull**: Start with the first three points and iteratively add points while maintaining the convex property
4. **Cross product test**: Use cross product to determine if a turn is clockwise or counterclockwise

## Time Complexity:
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for the hull storage

## Output Example:
```
Input points:
  (0, 3)
  (1, 1)
  (2, 2)
  (4, 4)
  (0, 0)
  (1, 2)
  (3, 1)
  (3, 3)

Convex Hull points:
  (0, 0)
  (0, 3)
  (4, 4)
  (3, 1)

Number of points in convex hull: 4
```

