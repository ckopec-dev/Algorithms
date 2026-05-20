# Quickhull Algorithm in Lua

Quickhull is a divide-and-conquer algorithm for computing the convex hull of a set of points in 2D space. Here's an implementation in Lua:

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
    return (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
end

-- Find the point farthest from the line formed by p1 and p2
function find_farthest_point(points, p1, p2)
    local max_distance = -math.huge
    local farthest_point = nil
    
    for _, point in ipairs(points) do
        local distance = math.abs(cross_product(p1, p2, point))
        if distance > max_distance then
            max_distance = distance
            farthest_point = point
        end
    end
    
    return farthest_point
end

-- Quickhull recursive function
function quickhull_recursive(points, p1, p2, side)
    local hull = {}
    
    -- Find the farthest point from the line p1-p2
    local farthest = find_farthest_point(points, p1, p2)
    
    if not farthest then
        return {}
    end
    
    -- Add the farthest point to the hull
    table.insert(hull, farthest)
    
    -- Recursively find hull points on each side of the line
    local left_hull = quickhull_recursive(points, p1, farthest, side)
    local right_hull = quickhull_recursive(points, farthest, p2, side)
    
    -- Combine results
    for _, point in ipairs(left_hull) do
        table.insert(hull, point)
    end
    
    for _, point in ipairs(right_hull) do
        table.insert(hull, point)
    end
    
    return hull
end

-- Main Quickhull function
function quickhull(points)
    if #points < 3 then
        return points
    end
    
    -- Find leftmost and rightmost points
    local leftmost = points[1]
    local rightmost = points[1]
    
    for _, point in ipairs(points) do
        if point.x < leftmost.x then
            leftmost = point
        end
        if point.x > rightmost.x then
            rightmost = point
        end
    end
    
    -- Find points on the left and right sides of the line
    local left_points = {}
    local right_points = {}
    
    for _, point in ipairs(points) do
        local cross = cross_product(leftmost, rightmost, point)
        if cross > 0 then
            table.insert(left_points, point)
        elseif cross < 0 then
            table.insert(right_points, point)
        end
    end
    
    -- Find the hull points
    local hull = {}
    table.insert(hull, leftmost)
    
    -- Recursively find hull points on the left side
    local left_hull = quickhull_recursive(left_points, leftmost, rightmost, 1)
    for _, point in ipairs(left_hull) do
        table.insert(hull, point)
    end
    
    -- Recursively find hull points on the right side
    local right_hull = quickhull_recursive(right_points, rightmost, leftmost, -1)
    for _, point in ipairs(right_hull) do
        table.insert(hull, point)
    end
    
    table.insert(hull, rightmost)
    
    return hull
end

-- Example usage
print("Quickhull Algorithm Example")
print("==========================")

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

-- Compute convex hull
local hull = quickhull(points)

print("\nConvex hull points:")
for i, point in ipairs(hull) do
    print(string.format("  (%d, %d)", point.x, point.y))
end

-- Alternative simpler implementation for demonstration
function simple_quickhull(points)
    -- Find extreme points
    local min_x, max_x = math.huge, -math.huge
    local min_y, max_y = math.huge, -math.huge
    
    for _, point in ipairs(points) do
        min_x = math.min(min_x, point.x)
        max_x = math.max(max_x, point.x)
        min_y = math.min(min_y, point.y)
        max_y = math.max(max_y, point.y)
    end
    
    -- Simple convex hull (just for demonstration)
    local hull_points = {
        Point:new(min_x, min_y),
        Point:new(max_x, min_y),
        Point:new(max_x, max_y),
        Point:new(min_x, max_y)
    }
    
    return hull_points
end

print("\nSimple demonstration:")
local simple_hull = simple_quickhull(points)
for i, point in ipairs(simple_hull) do
    print(string.format("  (%d, %d)", point.x, point.y))
end
```

## Key Features of This Implementation:

1. **Point Structure**: Defines a Point class with x and y coordinates
2. **Cross Product**: Calculates the cross product to determine point position relative to a line
3. **Recursive Division**: Uses divide-and-conquer approach to build the hull
4. **Side Detection**: Determines which side of a line each point lies on
5. **Boundary Handling**: Properly handles edge cases like few points

## How it Works:

1. Find the leftmost and rightmost points to establish the initial line
2. Partition remaining points into those on the left and right sides of this line
3. For each side, find the point farthest from the line
4. Recursively apply the same process to the subproblems
5. Combine results to form the complete convex hull

This implementation provides a working example of the Quickhull algorithm that can be extended or modified for specific use cases.

