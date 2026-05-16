# Cholesky Decomposition in Lua

Here's an implementation of the Cholesky decomposition algorithm in Lua:

```lua
-- Cholesky Decomposition Algorithm in Lua
function cholesky_decomposition(matrix)
    local n = #matrix
    local L = {}
    
    -- Initialize L matrix with zeros
    for i = 1, n do
        L[i] = {}
        for j = 1, n do
            L[i][j] = 0
        end
    end
    
    -- Cholesky decomposition algorithm
    for i = 1, n do
        for j = 1, i do
            local sum = 0
            for k = 1, j - 1 do
                sum = sum + L[i][k] * L[j][k]
            end
            
            if i == j then
                -- Diagonal elements
                L[i][j] = math.sqrt(matrix[i][i] - sum)
            else
                -- Off-diagonal elements
                L[i][j] = (matrix[i][j] - sum) / L[j][j]
            end
        end
    end
    
    return L
end

-- Helper function to print matrix
function print_matrix(matrix)
    for i = 1, #matrix do
        local row = ""
        for j = 1, #matrix[i] do
            row = row .. string.format("%8.4f ", matrix[i][j])
        end
        print(row)
    end
    print()
end

-- Example usage
local A = {
    {4, 12, -16},
    {12, 37, -43},
    {-16, -43, 98}
}

print("Original matrix A:")
print_matrix(A)

local L = cholesky_decomposition(A)

print("Lower triangular matrix L:")
print_matrix(L)

-- Verify the decomposition: L * L^T should equal A
local L_transpose = {}
local n = #L
for i = 1, n do
    L_transpose[i] = {}
    for j = 1, n do
        L_transpose[i][j] = L[j][i]
    end
end

-- Matrix multiplication L * L^T
local verification = {}
for i = 1, n do
    verification[i] = {}
    for j = 1, n do
        local sum = 0
        for k = 1, n do
            sum = sum + L[i][k] * L_transpose[k][j]
        end
        verification[i][j] = sum
    end
end

print("Verification (L * L^T):")
print_matrix(verification)
```

## Output:
```
Original matrix A:
   4.0000  12.0000 -16.0000 
  12.0000  37.0000 -43.0000 
 -16.0000 -43.0000  98.0000 

Lower triangular matrix L:
   2.0000   0.0000   0.0000 
   6.0000   1.0000   0.0000 
  -8.0000   5.0000   3.0000 

Verification (L * L^T):
   4.0000  12.0000 -16.0000 
  12.0000  37.0000 -43.0000 
 -16.0000 -43.0000  98.0000 
```

## Algorithm Explanation:

The Cholesky decomposition factorizes a symmetric positive-definite matrix A into the product of a lower triangular matrix L and its transpose L^T, such that A = L * L^T.

**Key steps:**
1. Initialize the lower triangular matrix L with zeros
2. For each diagonal element L[i][i], compute: L[i][i] = √(A[i][i] - Σ(L[i][k] * L[i][k]))
3. For each off-diagonal element L[i][j] (where i > j), compute: L[i][j] = (A[i][j] - Σ(L[i][k] * L[j][k])) / L[j][j]
4. The algorithm processes elements column by column, ensuring numerical stability

**Time Complexity:** O(n³)
**Space Complexity:** O(n²)

This implementation assumes the input matrix is symmetric and positive-definite, which are the requirements for a valid Cholesky decomposition.

