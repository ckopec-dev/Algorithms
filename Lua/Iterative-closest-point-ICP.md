# Iterative Closest Point (ICP) Algorithm in Lua

Here's a complete implementation of the ICP algorithm in Lua:

```lua
-- ICP Algorithm Implementation in Lua
-- This implementation finds the optimal transformation between two point clouds

-- Point class
Point = {}
Point.__index = Point

function Point:new(x, y, z)
    local obj = {
        x = x or 0,
        y = y or 0,
        z = z or 0
    }
    setmetatable(obj, Point)
    return obj
end

function Point:distance(other)
    local dx = self.x - other.x
    local dy = self.y - other.y
    local dz = self.z - other.z
    return math.sqrt(dx*dx + dy*dy + dz*dz)
end

function Point:transform(tx, ty, tz, rx, ry, rz)
    -- Simple translation and rotation transformation
    -- This is a simplified version - real ICP would use more sophisticated methods
    local new_x = self.x + tx
    local new_y = self.y + ty
    local new_z = self.z + tz
    
    -- Simple rotation (simplified for 2D case)
    if rx ~= nil and ry ~= nil then
        local cos_r = math.cos(rx)
        local sin_r = math.sin(rx)
        new_x = self.x * cos_r - self.y * sin_r + tx
        new_y = self.x * sin_r + self.y * cos_r + ty
    end
    
    return Point:new(new_x, new_y, new_z)
end

-- ICP Algorithm implementation
local ICP = {}

-- Find closest points between two point clouds
function ICP:find_closest_points(source_points, target_points)
    local correspondences = {}
    
    for i, source_point in ipairs(source_points) do
        local min_distance = math.huge
        local closest_index = 1
        
        for j, target_point in ipairs(target_points) do
            local distance = source_point:distance(target_point)
            if distance < min_distance then
                min_distance = distance
                closest_index = j
            end
        end
        
        table.insert(correspondences, {source = i, target = closest_index})
    end
    
    return correspondences
end

-- Calculate transformation matrix from correspondences
function ICP:calculate_transformation(source_points, target_points, correspondences)
    local source_mean = Point:new(0, 0, 0)
    local target_mean = Point:new(0, 0, 0)
    
    -- Calculate centroids
    for _, corr in ipairs(correspondences) do
        local source_point = source_points[corr.source]
        local target_point = target_points[corr.target]
        
        source_mean.x = source_mean.x + source_point.x
        source_mean.y = source_mean.y + source_point.y
        source_mean.z = source_mean.z + source_point.z
        
        target_mean.x = target_mean.x + target_point.x
        target_mean.y = target_mean.y + target_point.y
        target_mean.z = target_mean.z + target_point.z
    end
    
    local n = #correspondences
    source_mean.x = source_mean.x / n
    source_mean.y = source_mean.y / n
    source_mean.z = source_mean.z / n
    
    target_mean.x = target_mean.x / n
    target_mean.y = target_mean.y / n
    target_mean.z = target_mean.z / n
    
    -- Calculate covariance matrix
    local H = {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}
    
    for _, corr in ipairs(correspondences) do
        local source_point = source_points[corr.source]
        local target_point = target_points[corr.target]
        
        local source_x = source_point.x - source_mean.x
        local source_y = source_point.y - source_mean.y
        local source_z = source_point.z - source_mean.z
        
        local target_x = target_point.x - target_mean.x
        local target_y = target_point.y - target_mean.y
        local target_z = target_point.z - target_mean.z
        
        H[1][1] = H[1][1] + source_x * target_x
        H[1][2] = H[1][2] + source_x * target_y
        H[1][3] = H[1][3] + source_x * target_z
        
        H[2][1] = H[2][1] + source_y * target_x
        H[2][2] = H[2][2] + source_y * target_y
        H[2][3] = H[2][3] + source_y * target_z
        
        H[3][1] = H[3][1] + source_z * target_x
        H[3][2] = H[3][2] + source_z * target_y
        H[3][3] = H[3][3] + source_z * target_z
    end
    
    -- Simplified transformation - in real ICP, you'd use SVD
    -- Here we return a simple translation
    local tx = target_mean.x - source_mean.x
    local ty = target_mean.y - source_mean.y
    local tz = target_mean.z - source_mean.z
    
    return {tx = tx, ty = ty, tz = tz, rx = 0, ry = 0, rz = 0}
end

-- Apply transformation to point cloud
function ICP:transform_point_cloud(points, transformation)
    local transformed_points = {}
    
    for _, point in ipairs(points) do
        local new_point = point:transform(
            transformation.tx,
            transformation.ty,
            transformation.tz,
            transformation.rx,
            transformation.ry,
            transformation.rz
        )
        table.insert(transformed_points, new_point)
    end
    
    return transformed_points
end

-- Main ICP function
function ICP:icp(source_points, target_points, max_iterations, tolerance)
    local current_source = {}
    
    -- Make a copy of source points
    for _, point in ipairs(source_points) do
        table.insert(current_source, Point:new(point.x, point.y, point.z))
    end
    
    local total_error = math.huge
    
    for iteration = 1, max_iterations do
        -- Find correspondences
        local correspondences = ICP:find_closest_points(current_source, target_points)
        
        -- Calculate transformation
        local transformation = ICP:calculate_transformation(current_source, target_points, correspondences)
        
        -- Apply transformation
        current_source = ICP:transform_point_cloud(current_source, transformation)
        
        -- Calculate error (mean distance between correspondences)
        local total_distance = 0
        for _, corr in ipairs(correspondences) do
            local source_point = current_source[corr.source]
            local target_point = target_points[corr.target]
            total_distance = total_distance + source_point:distance(target_point)
        end
        
        local mean_distance = total_distance / #correspondences
        total_error = mean_distance
        
        print(string.format("Iteration %d: Error = %.6f", iteration, mean_distance))
        
        -- Check convergence
        if mean_distance < tolerance then
            print("Converged!")
            break
        end
    end
    
    return current_source, total_error
end

-- Example usage
print("ICP Algorithm Example")
print("=====================")

-- Create sample point clouds
local source_points = {
    Point:new(0, 0, 0),
    Point:new(1, 0, 0),
    Point:new(1, 1, 0),
    Point:new(0, 1, 0),
    Point:new(0.5, 0.5, 0)
}

local target_points = {
    Point:new(0.1, 0.1, 0),
    Point:new(1.1, 0.1, 0),
    Point:new(1.1, 1.1, 0),
    Point:new(0.1, 1.1, 0),
    Point:new(0.6, 0.6, 0)
}

-- Run ICP
print("Running ICP algorithm...")
local result_points, final_error = ICP:icp(source_points, target_points, 10, 0.001)

print("Final transformation error:", final_error)
print("Result points:")
for i, point in ipairs(result_points) do
    print(string.format("  Point %d: (%.3f, %.3f, %.3f)", i, point.x, point.y, point.z))
end
```

## Key Features of this ICP Implementation:

1. **Point Class**: Represents 3D points with basic operations
2. **Correspondence Finding**: Matches points between source and target clouds
3. **Transformation Calculation**: Computes optimal translation using centroid matching
4. **Iterative Process**: Repeats until convergence or maximum iterations
5. **Error Monitoring**: Tracks convergence using mean distance

## How it Works:

1. **Initialization**: Start with source and target point clouds
2. **Correspondence**: Find closest points between clouds
3. **Transformation**: Calculate optimal transformation using centroids
4. **Apply**: Transform source points using calculated transformation
5. **Repeat**: Continue until convergence or maximum iterations

## Usage Notes:

- This is a simplified implementation for demonstration
- Real ICP implementations use SVD for more accurate rotation calculation
- The algorithm assumes 3D points but can be easily extended
- Convergence criteria can be adjusted based on requirements

The algorithm will output iteration numbers and error values, showing how the point clouds converge toward each other.

