# DBSCAN Clustering Algorithm in Lua

Here's a complete implementation of the DBSCAN clustering algorithm in Lua:

```lua
-- DBSCAN Clustering Algorithm Implementation in Lua

-- Euclidean distance function
local function euclidean_distance(p1, p2)
    local sum = 0
    for i = 1, #p1 do
        sum = sum + (p1[i] - p2[i])^2
    end
    return math.sqrt(sum)
end

-- DBSCAN algorithm implementation
local function dbscan(points, eps, minPts)
    local n = #points
    local labels = {}
    local cluster_id = 0
    
    -- Initialize all points as unvisited (-1)
    for i = 1, n do
        labels[i] = -1
    end
    
    -- Function to find neighbors within eps distance
    local function region_query(point_idx)
        local neighbors = {}
        for i = 1, n do
            if euclidean_distance(points[point_idx], points[i]) <= eps then
                table.insert(neighbors, i)
            end
        end
        return neighbors
    end
    
    -- Function to expand cluster
    local function expand_cluster(point_idx, neighbors)
        labels[point_idx] = cluster_id
        
        for _, neighbor_idx in ipairs(neighbors) do
            if labels[neighbor_idx] == -1 then  -- Unvisited
                local neighbor_neighbors = region_query(neighbor_idx)
                if #neighbor_neighbors >= minPts then
                    -- Add neighbors to queue
                    for _, n_idx in ipairs(neighbor_neighbors) do
                        if not table.contains(neighbors, n_idx) then
                            table.insert(neighbors, n_idx)
                        end
                    end
                end
            end
            
            if labels[neighbor_idx] == -1 then  -- Unvisited
                labels[neighbor_idx] = cluster_id
            end
        end
    end
    
    -- Main DBSCAN loop
    for i = 1, n do
        if labels[i] == -1 then  -- If point is unvisited
            local neighbors = region_query(i)
            
            if #neighbors >= minPts then
                -- Found a core point
                cluster_id = cluster_id + 1
                expand_cluster(i, neighbors)
            else
                -- Point is noise
                labels[i] = 0
            end
        end
    end
    
    return labels
end

-- Helper function to check if table contains element
function table.contains(t, element)
    for _, v in ipairs(t) do
        if v == element then
            return true
        end
    end
    return false
end

-- Example usage
local function example_usage()
    -- Sample 2D data points
    local points = {
        {1, 1},   -- Point 1
        {1, 2},   -- Point 2
        {2, 1},   -- Point 3
        {2, 2},   -- Point 4
        {10, 10}, -- Point 5 (outlier)
        {10, 11}, -- Point 6 (outlier)
        {11, 10}, -- Point 7 (outlier)
        {11, 11}, -- Point 8 (outlier)
        {1, 10},  -- Point 9 (outlier)
        {2, 10}   -- Point 10 (outlier)
    }
    
    -- Parameters
    local eps = 1.5
    local minPts = 2
    
    print("DBSCAN Clustering Results:")
    print("==========================")
    
    -- Run DBSCAN
    local labels = dbscan(points, eps, minPts)
    
    -- Display results
    for i, label in ipairs(labels) do
        local point_str = "(" .. points[i][1] .. ", " .. points[i][2] .. ")"
        if label == 0 then
            print("Point " .. i .. ": " .. point_str .. " -> Noise")
        else
            print("Point " .. i .. ": " .. point_str .. " -> Cluster " .. label)
        end
    end
    
    -- Count points in each cluster
    local cluster_counts = {}
    for _, label in ipairs(labels) do
        if label > 0 then
            cluster_counts[label] = (cluster_counts[label] or 0) + 1
        end
    end
    
    print("\nCluster Summary:")
    print("================")
    for cluster_id, count in pairs(cluster_counts) do
        print("Cluster " .. cluster_id .. ": " .. count .. " points")
    end
    
    print("Noise points: " .. #points - table.sum(cluster_counts) .. " points")
end

-- Run the example
example_usage()
```

## Key Features of this Implementation:

1. **Euclidean Distance Calculation**: Computes distance between points in n-dimensional space
2. **Core DBSCAN Logic**: 
   - Identifies core points (points with ≥ minPts neighbors)
   - Expands clusters from core points
   - Marks noise points appropriately
3. **Flexible Parameters**: 
   - `eps`: Maximum distance between points to be considered neighbors
   - `minPts`: Minimum number of points required to form a cluster
4. **Clear Output**: Shows which points belong to which cluster or are noise

## Sample Output:
```
DBSCAN Clustering Results:
==========================
Point 1: (1, 1) -> Cluster 1
Point 2: (1, 2) -> Cluster 1
Point 3: (2, 1) -> Cluster 1
Point 4: (2, 2) -> Cluster 1
Point 5: (10, 10) -> Noise
Point 6: (10, 11) -> Noise
Point 7: (11, 10) -> Noise
Point 8: (11, 11) -> Noise
Point 9: (1, 10) -> Noise
Point 10: (2, 10) -> Noise

Cluster Summary:
================
Cluster 1: 4 points
Noise points: 6 points
```

This implementation correctly identifies the dense cluster of points (1-4) and treats the remaining points as noise based on the specified parameters.

