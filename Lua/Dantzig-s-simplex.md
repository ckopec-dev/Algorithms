# Dantzig's Simplex Algorithm in Lua

Here's a complete implementation of Dantzig's Simplex algorithm in Lua for solving linear programming problems:

```lua
-- Dantzig's Simplex Algorithm Implementation in Lua

-- Simplex solver class
local Simplex = {}
Simplex.__index = Simplex

function Simplex:new()
    local obj = {
        tableau = {},
        rows = 0,
        cols = 0,
        basic_vars = {},
        non_basic_vars = {}
    }
    return setmetatable(obj, Simplex)
end

-- Parse and solve linear programming problem
function Simplex:solve(c, A, b)
    -- c: cost coefficients
    -- A: constraint matrix
    -- b: right-hand side values
    
    self.rows = #A
    self.cols = #c
    
    -- Create initial tableau
    self:initialize_tableau(c, A, b)
    
    -- Print initial tableau
    print("Initial Tableau:")
    self:print_tableau()
    
    -- Simplex iterations
    local iteration = 1
    while not self:is_optimal() do
        print("\n--- Iteration " .. iteration .. " ---")
        
        local pivot_col = self:find_pivot_column()
        if pivot_col == -1 then
            print("No pivot column found - problem is unbounded")
            return nil
        end
        
        local pivot_row = self:find_pivot_row(pivot_col)
        if pivot_row == -1 then
            print("No pivot row found - problem is unbounded")
            return nil
        end
        
        print("Pivot element: row " .. pivot_row .. ", column " .. pivot_col)
        
        -- Perform pivot operation
        self:pivot(pivot_row, pivot_col)
        
        print("Tableau after pivot:")
        self:print_tableau()
        
        iteration = iteration + 1
        
        -- Prevent infinite loops
        if iteration > 100 then
            print("Maximum iterations reached")
            break
        end
    end
    
    return self:get_solution()
end

-- Initialize the initial tableau
function Simplex:initialize_tableau(c, A, b)
    -- Create tableau with slack variables
    local tableau = {}
    
    -- Number of slack variables = number of constraints
    local slack_vars = #A
    
    -- Initialize tableau dimensions
    self.rows = #A
    self.cols = #c + slack_vars + 1  -- +1 for RHS
    
    -- Create tableau rows
    for i = 1, self.rows do
        tableau[i] = {}
        for j = 1, self.cols do
            if j <= #c then
                tableau[i][j] = A[i][j]
            elseif j <= #c + slack_vars and j - #c == i then
                tableau[i][j] = 1  -- Slack variable coefficient
            else
                tableau[i][j] = 0  -- Fill remaining with zeros
            end
        end
        tableau[i][self.cols] = b[i]  -- RHS values
    end
    
    -- Add objective function row (negative costs for maximization)
    local obj_row = {}
    for j = 1, #c do
        obj_row[j] = -c[j]
    end
    for j = #c + 1, self.cols - 1 do
        obj_row[j] = 0
    end
    obj_row[self.cols] = 0  -- Objective value
    
    -- Add objective row to tableau
    table.insert(tableau, obj_row)
    
    self.tableau = tableau
    
    -- Initialize basic variables (slack variables)
    self.basic_vars = {}
    for i = 1, slack_vars do
        self.basic_vars[i] = #c + i
    end
end

-- Check if current solution is optimal
function Simplex:is_optimal()
    local obj_row = self.tableau[#self.tableau]
    for j = 1, #obj_row - 1 do  -- Exclude RHS
        if obj_row[j] < 0 then
            return false
        end
    end
    return true
end

-- Find pivot column (most negative coefficient in objective row)
function Simplex:find_pivot_column()
    local obj_row = self.tableau[#self.tableau]
    local min_val = 0
    local pivot_col = -1
    
    for j = 1, #obj_row - 1 do  -- Exclude RHS
        if obj_row[j] < min_val then
            min_val = obj_row[j]
            pivot_col = j
        end
    end
    
    return pivot_col
end

-- Find pivot row using minimum ratio test
function Simplex:find_pivot_row(pivot_col)
    local min_ratio = math.huge
    local pivot_row = -1
    
    for i = 1, self.rows do
        local element = self.tableau[i][pivot_col]
        if element > 0 then  -- Only consider positive elements
            local ratio = self.tableau[i][self.cols] / element
            if ratio < min_ratio then
                min_ratio = ratio
                pivot_row = i
            end
        end
    end
    
    return pivot_row
end

-- Perform pivot operation
function Simplex:pivot(pivot_row, pivot_col)
    local pivot_element = self.tableau[pivot_row][pivot_col]
    
    -- Normalize pivot row
    for j = 1, self.cols do
        self.tableau[pivot_row][j] = self.tableau[pivot_row][j] / pivot_element
    end
    
    -- Eliminate other elements in pivot column
    for i = 1, self.rows + 1 do  -- Include objective row
        if i ~= pivot_row then
            local factor = self.tableau[i][pivot_col]
            for j = 1, self.cols do
                self.tableau[i][j] = self.tableau[i][j] - factor * self.tableau[pivot_row][j]
            end
        end
    end
    
    -- Update basic variables
    self.basic_vars[pivot_row] = pivot_col
end

-- Print current tableau
function Simplex:print_tableau()
    local width = 10
    local format_str = string.format("%%%ds", width)
    
    -- Print header
    io.write("    ")
    for j = 1, self.cols do
        io.write(string.format(format_str, "x" .. j))
    end
    io.write("\n")
    
    -- Print tableau
    for i = 1, #self.tableau do
        if i <= self.rows then
            io.write(string.format(format_str, "s" .. i))
        else
            io.write(string.format(format_str, "z"))
        end
        
        for j = 1, self.cols do
            io.write(string.format(format_str, string.format("%.2f", self.tableau[i][j])))
        end
        io.write("\n")
    end
end

-- Get final solution
function Simplex:get_solution()
    local solution = {}
    local obj_value = self.tableau[#self.tableau][self.cols]
    
    -- Initialize solution vector
    for i = 1, self.cols - 1 do
        solution[i] = 0
    end
    
    -- Extract basic variables
    for i = 1, self.rows do
        local basic_var = self.basic_vars[i]
        if basic_var <= #solution then
            solution[basic_var] = self.tableau[i][self.cols]
        end
    end
    
    return {
        solution = solution,
        objective_value = obj_value
    }
end

-- Example usage
print("=== Linear Programming Problem ===")
print("Maximize: 3x1 + 2x2")
print("Subject to:")
print("  x1 + x2 <= 4")
print("  2x1 + x2 <= 6")
print("  x1, x2 >= 0")
print()

-- Define problem data
local c = {3, 2}        -- Objective coefficients
local A = {{1, 1},     -- Constraint matrix
           {2, 1}}
local b = {4, 6}       -- Right-hand side values

-- Solve using simplex algorithm
local simplex = Simplex:new()
local result = simplex:solve(c, A, b)

if result then
    print("\n=== Solution ===")
    print("Optimal value: " .. string.format("%.2f", result.objective_value))
    print("Solution vector:")
    for i = 1, #result.solution do
        print("  x" .. i .. " = " .. string.format("%.2f", result.solution[i]))
    end
end

-- Another example: Minimize 2x1 + 3x2
print("\n=== Another Example ===")
print("Minimize: 2x1 + 3x2")
print("Subject to:")
print("  x1 + x2 >= 3")
print("  2x1 + x2 >= 4")
print("  x1, x2 >= 0")
print()

-- Convert to maximization by negating coefficients
local c2 = {-2, -3}
local A2 = {{-1, -1},  -- Convert >= to <= by negating
            {-2, -1}}
local b2 = {-3, -4}

local simplex2 = Simplex:new()
local result2 = simplex2:solve(c2, A2, b2)

if result2 then
    print("\n=== Solution ===")
    print("Optimal value: " .. string.format("%.2f", -result2.objective_value))
    print("Solution vector:")
    for i = 1, #result2.solution do
        print("  x" .. i .. " = " .. string.format("%.2f", result2.solution[i]))
    end
end
```

## Key Features of this Implementation:

1. **Complete Tableau Management**: Handles the creation and manipulation of simplex tableaus
2. **Pivot Operations**: Implements the core pivot operation for simplex method
3. **Optimality Check**: Determines when the optimal solution is reached
4. **Minimum Ratio Test**: Performs the necessary ratio test for pivot selection
5. **Solution Extraction**: Properly extracts the final solution and objective value

## Example Problem Solved:

The code demonstrates solving two linear programming problems:
1. **Maximize** 3x₁ + 2x₂ subject to constraints
2. **Minimize** 2x₁ + 3x₂ subject to constraints

## Output Format:

The algorithm prints:
- Initial tableau
- Each iteration with pivot information
- Final solution with optimal value
- Solution vector with variable values

This implementation provides a clear demonstration of Dantzig's Simplex algorithm in action, showing the step-by-step process of finding optimal solutions to linear programming problems.

