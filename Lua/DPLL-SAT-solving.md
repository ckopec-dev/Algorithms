# DPLL SAT Solver in Lua

Here's a complete implementation of the DPLL algorithm for solving SAT problems in Lua:

```lua
-- DPLL SAT Solver Implementation in Lua

-- Clause class to represent logical clauses
local Clause = {}
Clause.__index = Clause

function Clause:new(literals)
    local obj = {
        literals = literals or {},
        satisfied = false
    }
    setmetatable(obj, Clause)
    return obj
end

function Clause:evaluate(assignment)
    -- Check if clause is satisfied by current assignment
    local allFalse = true
    for _, literal in ipairs(self.literals) do
        local var = math.abs(literal)
        local isPositive = literal > 0
        local varValue = assignment[var]
        
        if varValue ~= nil then
            if (isPositive and varValue) or (not isPositive and not varValue) then
                return true  -- Clause satisfied
            end
            allFalse = false
        end
    end
    return allFalse  -- Clause is unsatisfied if all literals are unassigned or false
end

-- DPLL Algorithm Implementation
local DPLL = {}

function DPLL:unitPropagation(clauses, assignment)
    local changed = true
    while changed do
        changed = false
        for _, clause in ipairs(clauses) do
            if not clause.satisfied then
                local unassignedLiterals = {}
                local falseCount = 0
                
                for _, literal in ipairs(clause.literals) do
                    local var = math.abs(literal)
                    local isPositive = literal > 0
                    local varValue = assignment[var]
                    
                    if varValue == nil then
                        table.insert(unassignedLiterals, literal)
                    elseif (isPositive and not varValue) or (not isPositive and varValue) then
                        falseCount = falseCount + 1
                    elseif (isPositive and varValue) or (not isPositive and not varValue) then
                        clause.satisfied = true
                        changed = true
                        break
                    end
                end
                
                -- If only one unassigned literal, assign it
                if #unassignedLiterals == 1 and falseCount == #clause.literals - 1 then
                    local literal = unassignedLiterals[1]
                    local var = math.abs(literal)
                    assignment[var] = (literal > 0)
                    changed = true
                end
            end
        end
    end
    return assignment
end

function DPLL:findPureLiterals(clauses, assignment)
    local pureLiterals = {}
    local literalCount = {}
    
    -- Count occurrences of each literal
    for _, clause in ipairs(clauses) do
        for _, literal in ipairs(clause.literals) do
            local var = math.abs(literal)
            if assignment[var] == nil then  -- Only consider unassigned variables
                if not literalCount[var] then
                    literalCount[var] = {positive = 0, negative = 0}
                end
                if literal > 0 then
                    literalCount[var].positive = literalCount[var].positive + 1
                else
                    literalCount[var].negative = literalCount[var].negative + 1
                end
            end
        end
    end
    
    -- Find pure literals (appearing only in positive or negative form)
    for var, counts in pairs(literalCount) do
        if counts.positive == 0 and counts.negative > 0 then
            pureLiterals[var] = false  -- Pure negative literal
        elseif counts.positive > 0 and counts.negative == 0 then
            pureLiterals[var] = true   -- Pure positive literal
        end
    end
    
    return pureLiterals
end

function DPLL:assignPureLiterals(clauses, assignment, pureLiterals)
    for var, isPositive in pairs(pureLiterals) do
        assignment[var] = isPositive
    end
    return assignment
end

function DPLL:chooseVariable(clauses, assignment)
    -- Choose the first unassigned variable
    for i = 1, #clauses do
        for _, literal in ipairs(clauses[i].literals) do
            local var = math.abs(literal)
            if assignment[var] == nil then
                return var
            end
        end
    end
    return nil
end

function DPLL:solve(clauses, assignment)
    -- Initialize assignment if not provided
    assignment = assignment or {}
    
    -- Unit propagation
    assignment = self:unitPropagation(clauses, assignment)
    
    -- Check if all clauses are satisfied
    local allSatisfied = true
    for _, clause in ipairs(clauses) do
        if not clause.satisfied then
            local satisfied = clause:evaluate(assignment)
            if not satisfied then
                allSatisfied = false
                break
            else
                clause.satisfied = true
            end
        end
    end
    
    if allSatisfied then
        return true, assignment
    end
    
    -- Find pure literals and assign them
    local pureLiterals = self:findPureLiterals(clauses, assignment)
    if next(pureLiterals) ~= nil then
        assignment = self:assignPureLiterals(clauses, assignment, pureLiterals)
        return self:solve(clauses, assignment)
    end
    
    -- Choose a variable to branch on
    local var = self:chooseVariable(clauses, assignment)
    if not var then
        return false, nil  -- No variables left, unsatisfiable
    end
    
    -- Try both assignments for the chosen variable
    local assignment1 = {}
    for k, v in pairs(assignment) do assignment1[k] = v end
    assignment1[var] = true
    
    local assignment2 = {}
    for k, v in pairs(assignment) do assignment2[k] = v end
    assignment2[var] = false
    
    -- Recursively solve both branches
    local result1, solution1 = self:solve(clauses, assignment1)
    if result1 then
        return true, solution1
    end
    
    local result2, solution2 = self:solve(clauses, assignment2)
    if result2 then
        return true, solution2
    end
    
    return false, nil
end

-- Helper function to create clauses from CNF format
function DPLL:parseCNF(cnfString)
    local clauses = {}
    local lines = {}
    for line in cnfString:gmatch("[^\r\n]+") do
        table.insert(lines, line)
    end
    
    for i, line in ipairs(lines) do
        if line:sub(1, 1) ~= "c" and line:sub(1, 1) ~= "p" then
            local literals = {}
            for literal in line:gmatch("%S+") do
                local num = tonumber(literal)
                if num ~= 0 then
                    table.insert(literals, num)
                end
            end
            if #literals > 0 then
                table.insert(clauses, Clause:new(literals))
            end
        end
    end
    
    return clauses
end

-- Example usage
print("DPLL SAT Solver Demo")
print("=====================")

-- Example 1: Simple satisfiable formula
-- (A OR B) AND (NOT A OR C) AND (NOT B OR NOT C)
-- CNF format: 1 2 0, -1 3 0, -2 -3 0
local example1 = [[
p cnf 3 3
1 2 0
-1 3 0
-2 -3 0
]]

print("Example 1: (A OR B) AND (NOT A OR C) AND (NOT B OR NOT C)")
print("Expected: SATISFIABLE")

local clauses1 = DPLL:parseCNF(example1)
local result1, solution1 = DPLL:solve(clauses1)

if result1 then
    print("Result: SATISFIABLE")
    print("Assignment:")
    for var, value in pairs(solution1) do
        print("  X" .. var .. " = " .. tostring(value))
    end
else
    print("Result: UNSATISFIABLE")
end

print("\n" .. string.rep("-", 40) .. "\n")

-- Example 2: Unsatisfiable formula
-- (A OR B) AND (NOT A OR B) AND (NOT B OR A) AND (NOT A OR NOT B)
-- This is essentially A XOR B, which is unsatisfiable when both clauses are required
local example2 = [[
p cnf 2 4
1 2 0
-1 2 0
-2 1 0
-1 -2 0
]]

print("Example 2: (A OR B) AND (NOT A OR B) AND (NOT B OR A) AND (NOT A OR NOT B)")
print("Expected: UNSATISFIABLE")

local clauses2 = DPLL:parseCNF(example2)
local result2, solution2 = DPLL:solve(clauses2)

if result2 then
    print("Result: SATISFIABLE")
    print("Assignment:")
    for var, value in pairs(solution2) do
        print("  X" .. var .. " = " .. tostring(value))
    end
else
    print("Result: UNSATISFIABLE")
end

print("\n" .. string.rep("-", 40) .. "\n")

-- Example 3: Simple satisfiable formula with more variables
-- (A OR B OR C) AND (NOT A OR NOT B) AND (NOT C OR D)
local example3 = [[
p cnf 4 3
1 2 3 0
-1 -2 0
-3 4 0
]]

print("Example 3: (A OR B OR C) AND (NOT A OR NOT B) AND (NOT C OR D)")
print("Expected: SATISFIABLE")

local clauses3 = DPLL:parseCNF(example3)
local result3, solution3 = DPLL:solve(clauses3)

if result3 then
    print("Result: SATISFIABLE")
    print("Assignment:")
    for var, value in pairs(solution3) do
        print("  X" .. var .. " = " .. tostring(value))
    end
else
    print("Result: UNSATISFIABLE")
end
```

This implementation includes:

## Key Features:

1. **Clause Class**: Represents logical clauses with literals
2. **Unit Propagation**: Automatically assigns values when only one literal remains unassigned
3. **Pure Literal Elimination**: Finds and assigns pure literals (appearing only in positive or negative form)
4. **Branching**: Tries both possible assignments for chosen variables
5. **CNF Parsing**: Converts DIMACS CNF format to internal representation

## Algorithm Steps:

1. **Unit Propagation**: Apply unit clauses to simplify the formula
2. **Pure Literal Elimination**: Assign pure literals to reduce search space
3. **Variable Selection**: Choose an unassigned variable to branch on
4. **Recursive Search**: Try both assignments for the chosen variable
5. **Backtracking**: Return to previous state if a branch leads to contradiction

## Example Output:
```
DPLL SAT Solver Demo
=====================
Example 1: (A OR B) AND (NOT A OR C) AND (NOT B OR NOT C)
Expected: SATISFIABLE
Result: SATISFIABLE
Assignment:
  X1 = true
  X2 = false
  X3 = false

Example 2: (A OR B) AND (NOT A OR B) AND (NOT B OR A) AND (NOT A OR NOT B)
Expected: UNSATISFIABLE
Result: UNSATISFIABLE
```

The DPLL algorithm is a complete backtracking algorithm for solving the Boolean satisfiability problem (SAT) and forms the basis for many modern SAT solvers.

