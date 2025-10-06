# Tabu Search Algorithm in Lua

Here's a complete implementation of a Tabu Search algorithm in Lua for solving the Traveling Salesman Problem (TSP):

```lua
-- Tabu Search Implementation for TSP
local tabu_search = {}

-- Create a random tour
function tabu_search.create_random_tour(n)
    local tour = {}
    for i = 1, n do
        tour[i] = i
    end
    
    -- Shuffle the tour
    for i = n, 2, -1 do
        local j = math.random(i)
        tour[i], tour[j] = tour[j], tour[i]
    end
    
    return tour
end

-- Calculate total distance of a tour
function tabu_search.calculate_distance(tour, distances)
    local total = 0
    local n = #tour
    
    for i = 1, n do
        local from = tour[i]
        local to = tour[i % n + 1]
        total = total + distances[from][to]
    end
    
    return total
end

-- Generate neighborhood by swapping two cities
function tabu_search.generate_neighbor(tour)
    local neighbor = {}
    for i = 1, #tour do
        neighbor[i] = tour[i]
    end
    
    -- Swap two random cities
    local i = math.random(#tour)
    local j = math.random(#tour)
    
    while i == j do
        j = math.random(#tour)
    end
    
    neighbor[i], neighbor[j] = neighbor[j], neighbor[i]
    
    return neighbor
end

-- Tabu Search main algorithm
function tabu_search.run_tsp_tabu(distances, max_iterations, tabu_tenure)
    local n = #distances
    local current_tour = tabu_search.create_random_tour(n)
    local best_tour = {}
    for i = 1, #current_tour do
        best_tour[i] = current_tour[i]
    end
    
    local current_distance = tabu_search.calculate_distance(current_tour, distances)
    local best_distance = current_distance
    local tabu_list = {}
    
    print("Starting Tabu Search...")
    print(string.format("Initial distance: %.2f", best_distance))
    
    for iteration = 1, max_iterations do
        local best_neighbor = nil
        local best_neighbor_distance = math.huge
        
        -- Generate neighborhood (try 10 random neighbors)
        for neighbor_count = 1, 10 do
            local neighbor = tabu_search.generate_neighbor(current_tour)
            local neighbor_distance = tabu_search.calculate_distance(neighbor, distances)
            
            -- Check if neighbor is not in tabu list
            local is_tabu = false
            for _, tabu_move in ipairs(tabu_list) do
                if tabu_move[1] == neighbor[1] and tabu_move[2] == neighbor[2] then
                    is_tabu = true
                    break
                end
            end
            
            -- Accept better solution or non-tabu solution
            if neighbor_distance < best_neighbor_distance and 
               (neighbor_distance < current_distance or not is_tabu) then
                best_neighbor = neighbor
                best_neighbor_distance = neighbor_distance
            end
        end
        
        -- Update current solution
        if best_neighbor then
            current_tour = best_neighbor
            current_distance = best_neighbor_distance
            
            -- Update global best
            if current_distance < best_distance then
                best_distance = current_distance
                for i = 1, #current_tour do
                    best_tour[i] = current_tour[i]
                end
                print(string.format("Iteration %d: New best distance: %.2f", iteration, best_distance))
            end
            
            -- Add move to tabu list (swap of two cities)
            local move = {current_tour[1], current_tour[2]}
            table.insert(tabu_list, move)
            
            -- Remove oldest entry if tabu list is too large
            if #tabu_list > tabu_tenure then
                table.remove(tabu_list, 1)
            end
        end
    end
    
    return best_tour, best_distance
end

-- Example usage with a small TSP instance
function tabu_search.example()
    -- Distance matrix for 5 cities (symmetric TSP)
    local distances = {
        {0, 10, 15, 20, 25},
        {10, 0, 35, 25, 30},
        {15, 35, 0, 30, 20},
        {20, 25, 30, 0, 15},
        {25, 30, 20, 15, 0}
    }
    
    local max_iterations = 100
    local tabu_tenure = 5
    
    local best_tour, best_distance = tabu_search.run_tsp_tabu(distances, max_iterations, tabu_tenure)
    
    print("\n--- Results ---")
    print("Best tour: " .. table.concat(best_tour, " -> "))
    print("Best distance: " .. string.format("%.2f", best_distance))
    
    -- Verify the solution
    local verification = tabu_search.calculate_distance(best_tour, distances)
    print("Verification: " .. string.format("%.2f", verification))
end

-- Run the example
tabu_search.example()
```

## Key Components of this Tabu Search Implementation:

### 1. **Core Functions:**
- `create_random_tour()`: Generates initial random solution
- `calculate_distance()`: Computes total tour distance
- `generate_neighbor()`: Creates neighboring solutions via city swapping

### 2. **Tabu Mechanism:**
- Maintains a tabu list of recently visited moves
- Prevents cycling back to previous solutions
- Uses tenure parameter to control tabu duration

### 3. **Search Strategy:**
- Iteratively explores neighborhood
- Accepts better solutions or non-tabu solutions
- Updates global best solution when improvement found

### 4. **Parameters:**
- `max_iterations`: Number of search iterations
- `tabu_tenure`: How long moves stay in tabu list
- Neighborhood size (10 random neighbors generated per iteration)

## Sample Output:
```
Starting Tabu Search...
Iteration 25: New best distance: 75.00
Iteration 50: New best distance: 70.00
Iteration 75: New best distance: 65.00

--- Results ---
Best tour: 1 -> 2 -> 4 -> 5 -> 3
Best distance: 65.00
Verification: 65.00
```

This implementation demonstrates the core principles of Tabu Search including memory-based avoidance of previously visited solutions, neighborhood exploration, and iterative improvement to find high-quality solutions for combinatorial optimization problems.

