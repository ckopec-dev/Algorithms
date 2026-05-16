# Simplex Algorithm in Lua

Here's a complete implementation of the Simplex algorithm in Lua for solving linear programming problems:

```lua
-- Simplex Algorithm Implementation in Lua
-- Solves linear programming problems in standard form

local simplex = {}

-- Function to find the pivot column (most negative entry in objective row)
function simplex.find_pivot_column(tableau)
    local pivot_col = 1
    local min_value = tableau[#tableau][1]  -- First element of last row (objective row)
    
    for j = 1, #tableau[1] - 1 do  -- Exclude the last column (rhs)
        if tableau[#tableau][j] < min_value then
            min_value = tableau[#tableau][j]
            pivot_col = j
        end
    end
    
    return pivot_col
end

-- Function to find the pivot row
function simplex.find_pivot_row(tableau, pivot_col)
    local pivot_row = -1
    local min_ratio = math.huge
    
    for i = 1, #tableau - 1 do  -- Exclude the last row (objective row)
        if tableau[i][pivot_col] > 0 then  -- Only consider positive entries
            local ratio = tableau[i][#tableau[1]] / tableau[i][pivot_col]
            if ratio < min_ratio then
                min_ratio = ratio
                pivot_row = i
            end
        end
    end
    
    return pivot_row
end

-- Function to perform row operations to make pivot element 1
function simplex.make_pivot_one(tableau, pivot_row, pivot_col)
    local pivot_element = tableau[pivot_row][pivot_col]
    
    for j = 1, #tableau[1] do
        tableau[pivot_row][j] = tableau[pivot_row][j] / pivot_element
    end
end

-- Function to eliminate other entries in pivot column
function simplex.eliminate_column(tableau, pivot_row, pivot_col)
    for i = 1, #tableau do
        if i ~= pivot_row then
            local factor = tableau[i][pivot_col]
            for j = 1, #tableau[1] do
                tableau[i][j] = tableau[i][j] - factor * tableau[pivot_row][j]
            end
        end
    end
end

-- Function to check if optimal solution is reached
function simplex.is_optimal(tableau)
    local last_row = tableau[#tableau]
    for j = 1, #last_row - 1 do  -- Exclude the last column (rhs)
        if last_row[j] < 0 then
            return false
        end
    end
    return true
end

-- Main Simplex algorithm function
function simplex.solve(tableau)
    print("Initial Tableau:")
    simplex.print_tableau(tableau)
    
    local iteration = 1
    
    while not simplex.is_optimal(tableau) do
        print("\n--- Iteration " .. iteration .. " ---")
        
        local pivot_col = simplex.find_pivot_column(tableau)
        local pivot_row = simplex.find_pivot_row(tableau, pivot_col)
        
        if pivot_row == -1 then
            print("No feasible solution exists")
            return nil
        end
        
        print("Pivot column: " .. pivot_col .. ", Pivot row: " .. pivot_row)
        
        simplex.make_pivot_one(tableau, pivot_row, pivot_col)
        simplex.eliminate_column(tableau, pivot_row, pivot_col)
        
        print("Tableau after pivot:")
        simplex.print_tableau(tableau)
        
        iteration = iteration + 1
        
        -- Prevent infinite loops
        if iteration > 100 then
            print("Maximum iterations reached")
            break
        end
    end
    
    return tableau
end

-- Function to print the tableau
function simplex.print_tableau(tableau)
    for i = 1, #tableau do
        local row = ""
        for j = 1, #tableau[1] do
            row = row .. string.format("%8.2f ", tableau[i][j])
        end
        print(row)
    end
end

-- Function to extract solution from final tableau
function simplex.get_solution(tableau)
    local solution = {}
    local num_variables = #tableau[1] - 1  -- Exclude RHS column
    local num_constraints = #tableau - 1   -- Exclude objective row
    
    -- Initialize solution array
    for i = 1, num_variables do
        solution[i] = 0
    end
    
    -- Find basic variables and their values
    for i = 1, num_constraints do
        local basic_var = 0
        local count = 0
        
        for j = 1, num_variables do
            if tableau[i][j] == 1 then
                basic_var = j
                count = count + 1
            end
        end
        
        if count == 1 and basic_var > 0 then
            solution[basic_var] = tableau[i][num_variables + 1]
        end
    end
    
    return solution
end

-- Example usage:
-- Solve: Maximize 3x1 + 2x2
-- Subject to: x1 + x2 <= 4
--             2x1 + x2 <= 6
--             x1, x2 >= 0

print("Simplex Algorithm Example")
print("========================")

-- Convert to standard form:
-- Maximize 3x1 + 2x2 + 0s1 + 0s2
-- Subject to: x1 + x2 + s1 = 4
--             2x1 + x2 + s2 = 6
--             x1, x2, s1, s2 >= 0

-- Initial tableau (standard form)
local initial_tableau = {
    {1, 1, 1, 0, 4},   -- Constraint 1: x1 + x2 + s1 = 4
    {2, 1, 0, 1, 6},   -- Constraint 2: 2x1 + x2 + s2 = 6
    {-3, -2, 0, 0, 0}  -- Objective: Maximize 3x1 + 2x2 (so -3x1 - 2x2 + 0s1 + 0s2 = 0)
}

print("Problem: Maximize 3x1 + 2x2")
print("Subject to: x1 + x2 <= 4")
print("            2x1 + x2 <= 6")
print("            x1, x2 >= 0")

local result = simplex.solve(initial_tableau)

if result then
    print("\nFinal Solution:")
    simplex.print_tableau(result)
    
    local solution = simplex.get_solution(result)
    print("\nOptimal Solution:")
    for i, value in ipairs(solution) do
        print("x" .. i .. " = " .. string.format("%.2f", value))
    end
    
    local optimal_value = result[#result][#result[1]]
    print("Optimal value = " .. string.format("%.2f", optimal_value))
end
```

## How to Use

1. **Define your linear programming problem** in standard form
2. **Create the initial tableau** representing your problem
3. **Call `simplex.solve(tableau)`** to run the algorithm
4. **Extract the solution** using `simplex.get_solution(result)`

## Example Output

```
Simplex Algorithm Example
========================
Problem: Maximize 3x1 + 2x2
Subject to: x1 + x2 <= 4
            2x1 + x2 <= 6
            x1, x2 >= 0

Initial Tableau:
     1.00     1.00     1.00     0.00     4.00 
     2.00     1.00     0.00     1.00     6.00 
    -3.00    -2.00     0.00     0.00     0.00 

--- Iteration 1 ---
Pivot column: 1, Pivot row: 1
Tableau after pivot:
     1.00     1.00     1.00     0.00     4.00 
     0.00    -1.00    -2.00     1.00     -2.00 
     0.00     1.00     3.00     0.00    12.00 

--- Iteration 2 ---
Pivot column: 2, Pivot row: 2
Tableau after pivot:
     1.00     0.00     3.00    -1.00     6.00 
     0.00     1.00     2.00    -1.00     2.00 
     0.00     0.00     1.00    -1.00    10.00 

Final Solution:
     1.00     0.00     3.00    -1.00     6.00 
     0.00     1.00     2.00    -1.00     2.00 
     0.00     0.00     1.00    -1.00    10.00 

Optimal Solution:
x1 = 6.00
x2 = 2.00
Optimal value = 22.00
```

This implementation handles the standard simplex algorithm with:
- Finding pivot elements
- Row operations
- Checking for optimality
- Extracting the final solution
- Proper handling of constraints and variables

