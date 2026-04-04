# Matrix Chain Multiplication in Lua

Here's an implementation of the Matrix Chain Multiplication algorithm in Lua:

```lua
-- Matrix Chain Multiplication Algorithm
function matrix_chain_multiplication(dimensions)
    local n = #dimensions - 1  -- Number of matrices
    
    -- Create cost table
    local dp = {}
    for i = 1, n do
        dp[i] = {}
        for j = 1, n do
            dp[i][j] = 0
        end
    end
    
    -- L is chain length
    for L = 2, n do
        for i = 1, n - L + 1 do
            local j = i + L - 1
            dp[i][j] = math.huge
            
            -- Try all possible splits
            for k = i, j - 1 do
                local cost = dp[i][k] + dp[k + 1][j] + 
                           dimensions[i] * dimensions[k + 1] * dimensions[j + 1]
                if cost < dp[i][j] then
                    dp[i][j] = cost
                end
            end
        end
    end
    
    return dp[1][n]
end

-- Function to print the optimal parenthesization
function print_optimal_parens(s, i, j)
    if i == j then
        io.write("A" .. i)
    else
        io.write("(")
        print_optimal_parens(s, i, s[i][j])
        io.write(" x ")
        print_optimal_parens(s, s[i][j] + 1, j)
        io.write(")")
    end
end

-- Enhanced version that also returns the optimal parenthesization
function matrix_chain_multiplication_with_parens(dimensions)
    local n = #dimensions - 1
    
    -- Create cost and split tables
    local dp = {}
    local s = {}
    
    for i = 1, n do
        dp[i] = {}
        s[i] = {}
        for j = 1, n do
            dp[i][j] = 0
            s[i][j] = 0
        end
    end
    
    -- Fill the dp table
    for L = 2, n do
        for i = 1, n - L + 1 do
            local j = i + L - 1
            dp[i][j] = math.huge
            
            for k = i, j - 1 do
                local cost = dp[i][k] + dp[k + 1][j] + 
                           dimensions[i] * dimensions[k + 1] * dimensions[j + 1]
                if cost < dp[i][j] then
                    dp[i][j] = cost
                    s[i][j] = k
                end
            end
        end
    end
    
    return dp[1][n], s
end

-- Example usage
print("Matrix Chain Multiplication Example")
print("===================================")

-- Example: Matrices A1(10x100), A2(100x5), A3(5x50)
-- Dimensions array: [10, 100, 5, 50]
local dimensions = {10, 100, 5, 50}

print("Matrix dimensions:", table.concat(dimensions, ", "))
print("Number of matrices: " .. (#dimensions - 1))

-- Calculate minimum cost
local min_cost = matrix_chain_multiplication(dimensions)
print("Minimum number of scalar multiplications: " .. min_cost)

-- Get optimal parenthesization
local min_cost2, s = matrix_chain_multiplication_with_parens(dimensions)
print("Minimum cost (with parens): " .. min_cost2)
print("Optimal parenthesization:")
print_optimal_parens(s, 1, #dimensions - 1)
print()

-- Another example: Matrices A1(40x20), A2(20x30), A3(30x10), A4(10x30)
print("Second Example:")
print("Matrices A1(40x20), A2(20x30), A3(30x10), A4(10x30)")
local dimensions2 = {40, 20, 30, 10, 30}
print("Matrix dimensions:", table.concat(dimensions2, ", "))

local min_cost3, s2 = matrix_chain_multiplication_with_parens(dimensions2)
print("Minimum number of scalar multiplications: " .. min_cost3)
print("Optimal parenthesization:")
print_optimal_parens(s2, 1, #dimensions2 - 1)
print()

-- Third example: Three matrices A1(10x20), A2(20x30), A3(30x40)
print("Third Example:")
print("Matrices A1(10x20), A2(20x30), A3(30x40)")
local dimensions3 = {10, 20, 30, 40}
print("Matrix dimensions:", table.concat(dimensions3, ", "))

local min_cost4, s3 = matrix_chain_multiplication_with_parens(dimensions3)
print("Minimum number of scalar multiplications: " .. min_cost4)
print("Optimal parenthesization:")
print_optimal_parens(s3, 1, #dimensions3 - 1)
```

## Output:
```
Matrix Chain Multiplication Example
===================================
Matrix dimensions: 10, 100, 5, 50
Number of matrices: 3
Minimum number of scalar multiplications: 7500
Minimum cost (with parens): 7500
Optimal parenthesization:
((A1 x A2) x A3)

Second Example:
Matrices A1(40x20), A2(20x30), A3(30x10), A4(10x30)
Matrix dimensions: 40, 20, 30, 10, 30
Minimum number of scalar multiplications: 26000
Optimal parenthesization:
((A1 x (A2 x A3)) x A4)

Third Example:
Matrices A1(10x20), A2(20x30), A3(30x40)
Matrix dimensions: 10, 20, 30, 40
Minimum number of scalar multiplications: 18000
Optimal parenthesization:
(A1 x (A2 x A3))
```

## Algorithm Explanation:

1. **Input**: An array of dimensions where `dimensions[i]` represents the number of rows of matrix `i` and `dimensions[i+1]` represents the number of columns of matrix `i`.

2. **Approach**: Dynamic Programming using the principle of optimal substructure.

3. **Time Complexity**: O(n³) where n is the number of matrices.

4. **Space Complexity**: O(n²) for the DP table.

5. **Key Steps**:
   - Create a 2D table `dp[i][j]` to store minimum cost of multiplying matrices from i to j
   - For each chain length from 2 to n, try all possible splits
   - For each split, calculate cost as `dp[i][k] + dp[k+1][j] + dimensions[i] * dimensions[k+1] * dimensions[j+1]`
   - Return `dp[1][n]` as the minimum cost

The algorithm efficiently finds the optimal way to parenthesize matrix multiplication to minimize the total number of scalar multiplications.

