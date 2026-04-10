# Clarke-Wright Savings Algorithm in Lua

Here's an implementation of the Clarke-Wright savings algorithm for the Vehicle Routing Problem (VRP) in Lua:

```lua
-- Clarke-Wright Savings Algorithm Implementation
-- This algorithm helps find efficient routes for vehicle routing problems

-- Function to calculate Euclidean distance between two points
local function distance(p1, p2)
    return math.sqrt((p1.x - p2.x)^2 + (p1.y - p2.y)^2)
end

-- Function to calculate savings for two customers
local function calculate_savings(customers, depot, i, j)
    -- Savings = distance(depot, i) + distance(depot, j) - distance(i, j)
    local dist_ij = distance(customers[i], customers[j])
    local dist_di = distance(depot, customers[i])
    local dist_dj = distance(depot, customers[j])
    
    return dist_di + dist_dj - dist_ij
end

-- Main Clarke-Wright Savings Algorithm
local function clarke_wright_savings(customers, depot, capacity)
    local n = #customers
    local savings = {}
    
    -- Calculate all savings
    for i = 1, n do
        for j = i + 1, n do
            local saving = calculate_savings(customers, depot, i, j)
            table.insert(savings, {
                i = i,
                j = j,
                saving = saving
            })
        end
    end
    
    -- Sort savings in descending order
    table.sort(savings, function(a, b) return a.saving > b.saving end)
    
    -- Initialize routes (each customer is in its own route)
    local routes = {}
    for i = 1, n do
        routes[i] = {i}
    end
    
    -- Initialize route capacities
    local route_capacities = {}
    for i = 1, n do
        route_capacities[i] = customers[i].demand
    end
    
    -- Merge routes based on savings
    for _, saving_info in ipairs(savings) do
        local i = saving_info.i
        local j = saving_info.j
        
        -- Check if routes i and j are different
        if routes[i] and routes[j] and routes[i] ~= routes[j] then
            -- Check if merging would exceed capacity
            local new_capacity = route_capacities[i] + route_capacities[j]
            if new_capacity <= capacity then
                -- Merge routes
                local route_i = routes[i]
                local route_j = routes[j]
                
                -- Create new merged route
                local new_route = {}
                for _, node in ipairs(route_i) do
                    table.insert(new_route, node)
                end
                for _, node in ipairs(route_j) do
                    table.insert(new_route, node)
                end
                
                -- Update route information
                local route_id = route_i[1]  -- Use first node's route ID
                for _, node in ipairs(new_route) do
                    routes[node] = new_route
                end
                route_capacities[route_id] = new_capacity
                
                -- Remove old route entries
                for k = 1, n do
                    if routes[k] == route_j then
                        routes[k] = nil
                    end
                end
            end
        end
    end
    
    -- Return final routes
    local final_routes = {}
    for i = 1, n do
        if routes[i] and not routes[i].processed then
            table.insert(final_routes, routes[i])
            -- Mark as processed to avoid duplicates
            for _, node in ipairs(routes[i]) do
                routes[node].processed = true
            end
        end
    end
    
    return final_routes
end

-- Example usage
print("=== Clarke-Wright Savings Algorithm Example ===")

-- Define customer data (x, y coordinates, demand)
local customers = {
    {x = 1, y = 1, demand = 10},  -- Customer 1
    {x = 1, y = 4, demand = 15},  -- Customer 2
    {x = 4, y = 1, demand = 20},  -- Customer 3
    {x = 4, y = 4, demand = 25},  -- Customer 4
    {x = 2, y = 2, demand = 12}   -- Customer 5
}

-- Define depot location
local depot = {x = 0, y = 0}

-- Define vehicle capacity
local vehicle_capacity = 30

-- Run the algorithm
local result_routes = clarke_wright_savings(customers, depot, vehicle_capacity)

-- Display results
print("Final Routes:")
for i, route in ipairs(result_routes) do
    print("Route " .. i .. ": " .. table.concat(route, " -> "))
end

-- Calculate total distance for each route
print("\nRoute Details:")
for i, route in ipairs(result_routes) do
    local total_distance = 0
    local route_string = "Depot"
    
    -- Add first customer
    if #route > 0 then
        route_string = route_string .. " -> " .. route[1]
        total_distance = total_distance + distance(depot, customers[route[1]])
    end
    
    -- Add remaining customers
    for j = 1, #route - 1 do
        total_distance = total_distance + distance(customers[route[j]], customers[route[j+1]])
        route_string = route_string .. " -> " .. route[j+1]
    end
    
    -- Return to depot
    if #route > 0 then
        total_distance = total_distance + distance(customers[route[#route]], depot)
        route_string = route_string .. " -> Depot"
    end
    
    print("Route " .. i .. ": " .. route_string)
    print("Total Distance: " .. string.format("%.2f", total_distance))
    print()
end
```

## How it works:

1. **Distance Calculation**: Computes Euclidean distances between all points
2. **Savings Calculation**: For each pair of customers, calculates the savings from merging their routes
3. **Sorting**: Sorts all savings in descending order
4. **Route Merging**: Iteratively merges routes with highest savings, respecting capacity constraints
5. **Output**: Returns optimized routes for vehicle delivery

## Key Features:

- **Distance Calculation**: Uses Euclidean distance formula
- **Savings Logic**: Implements the core Clarke-Wright formula
- **Capacity Constraints**: Respects vehicle capacity limits
- **Route Optimization**: Merges routes to minimize total travel distance
- **Flexible Input**: Works with any number of customers and depot location

The algorithm is particularly effective for VRP problems where the goal is to minimize total travel distance while respecting vehicle capacity constraints.

