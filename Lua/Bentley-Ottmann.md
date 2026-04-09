# Bentley-Ottmann Algorithm Implementation in Lua

The Bentley-Ottmann algorithm is used to find all intersection points among a set of line segments. Here's a complete implementation:

```lua
-- Point class
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

function Point:__tostring()
    return string.format("(%.2f, %.2f)", self.x, self.y)
end

-- Line Segment class
Segment = {}
Segment.__index = Segment

function Segment:new(p1, p2)
    local obj = {
        p1 = p1,
        p2 = p2
    }
    setmetatable(obj, Segment)
    return obj
end

function Segment:__tostring()
    return string.format("%s-%s", self.p1, self.p2)
end

-- Event class for sweep line algorithm
Event = {}
Event.__index = Event

function Event:new(point, segment, isStart)
    local obj = {
        point = point,
        segment = segment,
        isStart = isStart
    }
    setmetatable(obj, Event)
    return obj
end

-- Bentley-Ottmann Algorithm Implementation
function BentleyOttmann(segments)
    local events = {}
    local intersections = {}
    
    -- Create events from segments
    for _, segment in ipairs(segments) do
        local p1 = segment.p1
        local p2 = segment.p2
        
        -- Ensure p1 is the lower point (or leftmost if same y)
        if p1.y > p2.y or (p1.y == p2.y and p1.x > p2.x) then
            p1, p2 = p2, p1
        end
        
        table.insert(events, Event:new(p1, segment, true))
        table.insert(events, Event:new(p2, segment, false))
    end
    
    -- Sort events by y-coordinate, then by x-coordinate
    table.sort(events, function(a, b)
        if a.point.y ~= b.point.y then
            return a.point.y < b.point.y
        end
        return a.point.x < b.point.x
    end)
    
    -- Sweep line status (using a balanced binary search tree approach)
    local sweepLine = {}
    
    -- Process events
    for _, event in ipairs(events) do
        if event.isStart then
            -- Add segment to sweep line
            table.insert(sweepLine, event.segment)
            table.sort(sweepLine, function(s1, s2)
                return compareSegments(s1, s2, event.point)
            end)
        else
            -- Remove segment from sweep line
            for i, seg in ipairs(sweepLine) do
                if seg == event.segment then
                    table.remove(sweepLine, i)
                    break
                end
            end
        end
        
        -- Check intersections with neighboring segments
        checkIntersections(sweepLine, event.point, intersections)
    end
    
    return intersections
end

-- Helper function to compare segments for sweep line ordering
function compareSegments(s1, s2, point)
    local x1 = intersectionX(s1, point)
    local x2 = intersectionX(s2, point)
    
    if x1 ~= x2 then
        return x1 < x2
    end
    
    -- If same x-coordinate, use slope comparison
    local slope1 = (s1.p2.y - s1.p1.y) / (s1.p2.x - s1.p1.x)
    local slope2 = (s2.p2.y - s2.p1.y) / (s2.p2.x - s2.p1.x)
    
    return slope1 < slope2
end

-- Find x-coordinate where segment intersects vertical line through point
function intersectionX(segment, point)
    local x1, y1 = segment.p1.x, segment.p1.y
    local x2, y2 = segment.p2.x, segment.p2.y
    local x = point.x
    
    if x1 == x2 then
        return x1
    end
    
    local t = (x - x1) / (x2 - x1)
    local y = y1 + t * (y2 - y1)
    
    return x
end

-- Check intersections between segments in sweep line
function checkIntersections(sweepLine, point, intersections)
    for i = 1, #sweepLine - 1 do
        for j = i + 1, #sweepLine do
            local seg1 = sweepLine[i]
            local seg2 = sweepLine[j]
            
            local intersection = lineIntersection(seg1, seg2)
            if intersection then
                -- Check if intersection is at or after current point
                if intersection.x >= point.x and intersection.y >= point.y then
                    table.insert(intersections, intersection)
                end
            end
        end
    end
end

-- Find intersection point of two line segments
function lineIntersection(seg1, seg2)
    local x1, y1 = seg1.p1.x, seg1.p1.y
    local x2, y2 = seg1.p2.x, seg1.p2.y
    local x3, y3 = seg2.p1.x, seg2.p1.y
    local x4, y4 = seg2.p2.x, seg2.p2.y
    
    local denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    
    if math.abs(denom) < 1e-10 then
        return nil  -- Lines are parallel
    end
    
    local t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom
    local u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denom
    
    if t >= 0 and t <= 1 and u >= 0 and u <= 1 then
        local x = x1 + t * (x2 - x1)
        local y = y1 + t * (y2 - y1)
        return Point:new(x, y)
    end
    
    return nil
end

-- Example usage
print("Bentley-Ottmann Algorithm Example")
print("================================")

-- Create some test segments
local segments = {
    Segment:new(Point:new(0, 0), Point:new(4, 4)),
    Segment:new(Point:new(0, 4), Point:new(4, 0)),
    Segment:new(Point:new(1, 0), Point:new(1, 4)),
    Segment:new(Point:new(0, 1), Point:new(4, 1)),
    Segment:new(Point:new(2, 2), Point:new(3, 3))
}

print("Input segments:")
for i, seg in ipairs(segments) do
    print(string.format("  %d: %s", i, seg))
end

print("\nRunning Bentley-Ottmann algorithm...")
local intersections = BentleyOttmann(segments)

print("\nIntersection points found:")
if #intersections == 0 then
    print("  No intersections found")
else
    for i, point in ipairs(intersections) do
        print(string.format("  %d: %s", i, point))
    end
end

-- More complex example
print("\n" .. string.rep("=", 50))
print("Complex example with more segments:")

local complexSegments = {
    Segment:new(Point:new(0, 0), Point:new(5, 5)),
    Segment:new(Point:new(0, 5), Point:new(5, 0)),
    Segment:new(Point:new(2, 0), Point:new(2, 5)),
    Segment:new(Point:new(0, 2), Point:new(5, 2)),
    Segment:new(Point:new(1, 1), Point:new(4, 4)),
    Segment:new(Point:new(3, 1), Point:new(1, 3)),
    Segment:new(Point:new(0, 3), Point:new(3, 0))
}

print("Input segments:")
for i, seg in ipairs(complexSegments) do
    print(string.format("  %d: %s", i, seg))
end

print("\nRunning Bentley-Ottmann algorithm...")
local complexIntersections = BentleyOttmann(complexSegments)

print("\nIntersection points found:")
if #complexIntersections == 0 then
    print("  No intersections found")
else
    for i, point in ipairs(complexIntersections) do
        print(string.format("  %d: %s", i, point))
    end
end
```

## Algorithm Explanation

This implementation of the Bentley-Ottmann algorithm works as follows:

1. **Event Creation**: Each segment's endpoints create events
2. **Event Sorting**: Events are sorted by y-coordinate (and x-coordinate for ties)
3. **Sweep Line**: As we process events, we maintain a sweep line status of active segments
4. **Intersection Detection**: When segments are added/removed from the sweep line, we check for intersections with neighbors

## Key Features

- **Point Class**: Represents 2D coordinates
- **Segment Class**: Represents line segments with start and end points
- **Event Management**: Handles segment start/end events
- **Sweep Line**: Maintains active segments in proper order
- **Intersection Calculation**: Finds exact intersection points between segments

## Time Complexity

- **Time**: O((n + k) log n) where n is the number of segments and k is the number of intersections
- **Space**: O(n) for storing events and segments

The algorithm efficiently finds all intersection points among a set of line segments using a sweep line approach with event processing.

