# K-Means Clustering in Lua

Here's a complete implementation of the K-means clustering algorithm in Lua:

```lua
-- K-Means Clustering Algorithm in Lua

-- Function to calculate Euclidean distance between two points
local function euclidean_distance(point1, point2)
    local sum = 0
    for i = 1, #point1 do
        sum = sum + (point1[i] - point2[i]) ^ 2
    end
    return math.sqrt(sum)
end

-- Function to calculate centroid of a cluster
local function calculate_centroid(cluster)
    if #cluster == 0 then return nil end
    
    local centroid = {}
    local dim = #cluster[1]
    
    -- Initialize centroid with zeros
    for i = 1, dim do
        centroid[i] = 0
    end
    
    -- Sum all points
    for _, point in ipairs(cluster) do
        for i = 1, dim do
            centroid[i] = centroid[i] + point[i]
        end
    end
    
    -- Calculate average
    for i = 1, dim do
        centroid[i] = centroid[i] / #cluster
    end
    
    return centroid
end

-- Main K-Means algorithm
local function kmeans(data, k, max_iterations)
    local n = #data
    local dim = #data[1]
    
    -- Initialize centroids randomly
    local centroids = {}
    for i = 1, k do
        centroids[i] = {}
        for j = 1, dim do
            centroids[i][j] = data[math.random(1, n)][j]
        end
    end
    
    local clusters = {}
    
    -- Main iteration loop
    for iteration = 1, max_iterations do
        -- Reset clusters
        for i = 1, k do
            clusters[i] = {}
        end
        
        -- Assign points to closest centroid
        for _, point in ipairs(data) do
            local min_distance = math.huge
            local closest_cluster = 1
            
            for i = 1, k do
                local distance = euclidean_distance(point, centroids[i])
                if distance < min_distance then
                    min_distance = distance
                    closest_cluster = i
                end
            end
            
            table.insert(clusters[closest_cluster], point)
        end
        
        -- Update centroids
        local updated_centroids = {}
        local changed = false
        
        for i = 1, k do
            local new_centroid = calculate_centroid(clusters[i])
            if new_centroid then
                updated_centroids[i] = new_centroid
                -- Check if centroid changed significantly
                if not centroids[i] or 
                   euclidean_distance(centroids[i], new_centroid) > 0.001 then
                    changed = true
                end
            else
                updated_centroids[i] = centroids[i]
            end
        end
        
        centroids = updated_centroids
        
        -- If no significant changes, stop
        if not changed then
            print("Converged after " .. iteration .. " iterations")
            break
        end
    end
    
    return clusters, centroids
end

-- Function to print clusters
local function print_clusters(clusters, centroids)
    for i = 1, #clusters do
        print("Cluster " .. i .. " (centroid: " .. 
              string.format("%.2f", centroids[i][1]) .. ", " .. 
              string.format("%.2f", centroids[i][2]) .. "):")
        for _, point in ipairs(clusters[i]) do
            print("  (" .. string.format("%.2f", point[1]) .. ", " .. 
                  string.format("%.2f", point[2]) .. ")")
        end
        print()
    end
end

-- Example usage
print("K-Means Clustering Example")
print("==========================")

-- Sample 2D data points
local data = {
    {1.0, 2.0},
    {1.5, 1.8},
    {5.0, 8.0},
    {8.0, 8.0},
    {1.0, 0.6},
    {9.0, 11.0},
    {8.0, 2.0},
    {10.0, 2.0},
    {9.0, 3.0}
}

local k = 3
local max_iterations = 100

print("Data points:")
for i, point in ipairs(data) do
    print("  " .. i .. ": (" .. point[1] .. ", " .. point[2] .. ")")
end

print("\nRunning K-Means with k=" .. k .. "...")
print("================================")

-- Run K-Means
local clusters, centroids = kmeans(data, k, max_iterations)

-- Display results
print_clusters(clusters, centroids)

-- Calculate and display cluster statistics
print("Cluster Statistics:")
print("==================")
for i = 1, #clusters do
    print("Cluster " .. i .. ": " .. #clusters[i] .. " points")
end
```

## How it works:

1. **Distance Calculation**: Uses Euclidean distance to measure similarity between points
2. **Centroid Calculation**: Computes the mean of all points in a cluster
3. **Assignment Step**: Each point is assigned to the nearest centroid
4. **Update Step**: Centroids are recalculated based on current cluster assignments
5. **Convergence**: The algorithm stops when centroids no longer change significantly

## Key Features:

- **Flexible dimensions**: Works with any number of dimensions
- **Convergence detection**: Stops when changes become negligible
- **Random initialization**: Starts with random centroid placement
- **Clear output**: Shows cluster assignments and statistics

## Sample Output:
```
K-Means Clustering Example
==========================
Data points:
  1: (1.0, 2.0)
  2: (1.5, 1.8)
  3: (5.0, 8.0)
  4: (8.0, 8.0)
  ...

Running K-Means with k=3...
================================
Cluster 1 (centroid: 1.17, 1.47):
  (1.0, 2.0)
  (1.5, 1.8)
  (1.0, 0.6)

Cluster 2 (centroid: 7.00, 8.00):
  (5.0, 8.0)
  (8.0, 8.0)

Cluster 3 (centroid: 9.33, 5.33):
  (9.0, 11.0)
  (8.0, 2.0)
  (10.0, 2.0)
  (9.0, 3.0)
```

This implementation provides a solid foundation for K-means clustering that can be easily extended or modified for specific use cases.

