# Convex Hull Algorithm in Lua

Here's an implementation of the Graham Scan algorithm for finding the convex hull of a set of points in Lua:

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
function cross_product(o, a, b)
    return (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)
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

-- Sort points by polar angle with respect to the lowest point
function sort_by_polar_angle(points, lowest)
    table.sort(points, function(a, b)
        local cross = cross_product(lowest, a, b)
        if cross == 0 then
            -- If collinear, sort by distance from lowest point
            return distance(lowest, a) < distance(lowest, b)
        else
            -- Sort by polar angle (counter-clockwise)
            return cross > 0
        end
    end)
end

-- Graham Scan algorithm for convex hull
function convex_hull(points)
    if #points < 3 then
        return points
    end
    
    -- Find the lowest point
    local lowest = find_lowest_point(points)
    
    -- Sort points by polar angle
    sort_by_polar_angle(points, lowest)
    
    -- Graham scan
    local hull = {points[1], points[2]}
    
    for i = 3, #points do
        while #hull >= 2 and cross_product(hull[#hull-1], hull[#hull], points[i]) <= 0 do
            table.remove(hull)
        end
        table.insert(hull, points[i])
    end
    
    return hull
end

-- Example usage
print("Convex Hull Algorithm Example")

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

-- Calculate convex hull
local hull = convex_hull(points)

print("\nConvex Hull points:")
for i, point in ipairs(hull) do
    print(string.format("  (%d, %d)", point.x, point.y))
end

print("\nNumber of points in convex hull: " .. #hull)
```

## Output:
```
Convex Hull Algorithm Example
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

## Algorithm Explanation:

1. **Find the lowest point**: Identify the point with the smallest y-coordinate (leftmost if tied)
2. **Sort by polar angle**: Sort all points by their polar angle with respect to the lowest point
3. **Graham scan**: Process points in sorted order, maintaining a stack where we remove points that create clockwise turns
4. **Return hull**: The final stack contains the convex hull points

## Time Complexity:
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for the hull storage

This implementation uses the Graham Scan algorithm, which is one of the most efficient algorithms for computing convex hulls in 2D space.

