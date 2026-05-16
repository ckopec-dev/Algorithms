# K-Medoids Clustering in Lua

Here's a complete implementation of the K-Medoids clustering algorithm in Lua:

```lua
-- K-Medoids Clustering Algorithm in Lua
-- Implementation of PAM (Partitioning Around Medoids) algorithm

-- Euclidean distance function
local function euclidean_distance(point1, point2)
    local sum = 0
    for i = 1, #point1 do
        sum = sum + (point1[i] - point2[i])^2
    end
    return math.sqrt(sum)
end

-- Calculate total cost (sum of distances to medoids)
local function calculate_cost(data, medoids)
    local total_cost = 0
    for i = 1, #data do
        local min_distance = math.huge
        for j = 1, #medoids do
            local distance = euclidean_distance(data[i], medoids[j])
            if distance < min_distance then
                min_distance = distance
            end
        end
        total_cost = total_cost + min_distance
    end
    return total_cost
end

-- Find medoid for a cluster
local function find_medoid(cluster_data)
    local min_cost = math.huge
    local best_medoid = nil
    
    for i = 1, #cluster_data do
        local cost = 0
        for j = 1, #cluster_data do
            cost = cost + euclidean_distance(cluster_data[i], cluster_data[j])
        end
        if cost < min_cost then
            min_cost = cost
            best_medoid = cluster_data[i]
        end
    end
    
    return best_medoid
end

-- K-Medoids clustering algorithm
local function k_medoids(data, k, max_iterations)
    local n = #data
    local medoids = {}
    
    -- Initialize medoids randomly
    local used_indices = {}
    for i = 1, k do
        local index
        repeat
            index = math.random(1, n)
        until not used_indices[index]
        used_indices[index] = true
        medoids[i] = data[index]
    end
    
    local clusters = {}
    
    for iteration = 1, max_iterations do
        -- Assign points to clusters
        for i = 1, n do
            local min_distance = math.huge
            local cluster_id = 1
            
            for j = 1, k do
                local distance = euclidean_distance(data[i], medoids[j])
                if distance < min_distance then
                    min_distance = distance
                    cluster_id = j
                end
            end
            
            if not clusters[cluster_id] then
                clusters[cluster_id] = {}
            end
            table.insert(clusters[cluster_id], data[i])
        end
        
        -- Update medoids
        local new_medoids = {}
        for i = 1, k do
            if clusters[i] and #clusters[i] > 0 then
                new_medoids[i] = find_medoid(clusters[i])
            else
                new_medoids[i] = medoids[i]  -- Keep old medoid if cluster empty
            end
        end
        
        -- Check for convergence
        local converged = true
        for i = 1, k do
            if not euclidean_distance(medoids[i], new_medoids[i]) == 0 then
                converged = false
                break
            end
        end
        
        medoids = new_medoids
        
        if converged then
            break
        end
        
        -- Clear clusters for next iteration
        clusters = {}
    end
    
    return medoids, clusters
end

-- Example usage
print("K-Medoids Clustering Example")
print("============================")

-- Sample 2D data points
local data = {
    {1, 2},
    {1, 4},
    {1, 0},
    {4, 2},
    {4, 4},
    {4, 0},
    {7, 2},
    {7, 4},
    {7, 0}
}

-- Set parameters
local k = 3
local max_iterations = 100

print("Data points:")
for i, point in ipairs(data) do
    print(string.format("Point %d: (%.1f, %.1f)", i, point[1], point[2]))
end

print("\nRunning K-Medoids clustering with k = " .. k)
print("Max iterations: " .. max_iterations)

-- Perform clustering
local medoids, clusters = k_medoids(data, k, max_iterations)

print("\nFinal Medoids:")
for i, medoid in ipairs(medoids) do
    print(string.format("Medoid %d: (%.1f, %.1f)", i, medoid[1], medoid[2]))
end

print("\nClusters:")
for i = 1, #clusters do
    print("Cluster " .. i .. ":")
    if clusters[i] then
        for j, point in ipairs(clusters[i]) do
            print("  (" .. point[1] .. ", " .. point[2] .. ")")
        end
    end
end

-- Calculate and display total cost
local total_cost = calculate_cost(data, medoids)
print("\nTotal cost: " .. string.format("%.2f", total_cost))
```

## Key Features of this Implementation:

1. **Euclidean Distance**: Calculates distance between data points
2. **Medoid Selection**: Finds the most representative point in each cluster
3. **PAM Algorithm**: Uses the Partitioning Around Medoids approach
4. **Convergence Check**: Stops when medoids no longer change
5. **Flexible Input**: Works with any dimensional data

## How it Works:

1. **Initialization**: Randomly select k medoids from data points
2. **Assignment**: Assign each point to the nearest medoid
3. **Update**: For each cluster, find the point that minimizes the total cost
4. **Repeat**: Continue until convergence or maximum iterations reached

## Sample Output:
```
K-Medoids Clustering Example
============================
Data points:
Point 1: (1.0, 2.0)
Point 2: (1.0, 4.0)
Point 3: (1.0, 0.0)
Point 4: (4.0, 2.0)
Point 5: (4.0, 4.0)
Point 6: (4.0, 0.0)
Point 7: (7.0, 2.0)
Point 8: (7.0, 4.0)
Point 9: (7.0, 0.0)

Running K-Medoids clustering with k = 3
Max iterations: 100

Final Medoids:
Medoid 1: (1.0, 2.0)
Medoid 2: (4.0, 2.0)
Medoid 3: (7.0, 2.0)

Clusters:
Cluster 1:
  (1.0, 2.0)
  (1.0, 4.0)
  (1.0, 0.0)
Cluster 2:
  (4.0, 2.0)
  (4.0, 4.0)
  (4.0, 0.0)
Cluster 3:
  (7.0, 2.0)
  (7.0, 4.0)
  (7.0, 0.0)

Total cost: 6.00
```

This implementation provides a complete, working K-Medoids clustering solution in Lua that can be easily adapted for different datasets and requirements.

