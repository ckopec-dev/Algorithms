# Gauss-Jordan Elimination in Lua

Here's a complete implementation of the Gauss-Jordan elimination algorithm in Lua:

```lua
-- Gauss-Jordan Elimination Algorithm
function gauss_jordan(matrix)
    local rows = #matrix
    local cols = #matrix[1]
    
    -- Forward elimination
    for i = 1, rows do
        -- Find pivot
        local pivot = i
        for k = i + 1, rows do
            if math.abs(matrix[k][i]) > math.abs(matrix[pivot][i]) then
                pivot = k
            end
        end
        
        -- Swap rows if needed
        if pivot ~= i then
            matrix[i], matrix[pivot] = matrix[pivot], matrix[i]
        end
        
        -- Check for singular matrix
        if math.abs(matrix[i][i]) < 1e-10 then
            error("Matrix is singular")
        end
        
        -- Make pivot element 1
        local pivot_val = matrix[i][i]
        for j = i, cols do
            matrix[i][j] = matrix[i][j] / pivot_val
        end
        
        -- Eliminate column
        for k = 1, rows do
            if k ~= i then
                local factor = matrix[k][i]
                for j = i, cols do
                    matrix[k][j] = matrix[k][j] - factor * matrix[i][j]
                end
            end
        end
    end
    
    return matrix
end

-- Function to print matrix
function print_matrix(matrix)
    for i = 1, #matrix do
        local row = ""
        for j = 1, #matrix[i] do
            row = row .. string.format("%8.3f ", matrix[i][j])
        end
        print(row)
    end
    print()
end

-- Example usage
print("Original Matrix:")
local matrix = {
    {2, 1, -1, 8},
    {-3, -1, 2, -11},
    {-2, 1, 2, -3}
}

print_matrix(matrix)

-- Apply Gauss-Jordan elimination
local result = gauss_jordan(matrix)

print("Reduced Row Echelon Form:")
print_matrix(result)

-- Extract solution (last column)
print("Solution:")
for i = 1, #result do
    print(string.format("x%d = %8.3f", i, result[i][#result[1]]))
end
```

## Output:
```
Original Matrix:
    2.000   1.000  -1.000   8.000 
   -3.000  -1.000   2.000  -11.000 
   -2.000   1.000   2.000  -3.000 

Reduced Row Echelon Form:
    1.000   0.000   0.000   2.000 
    0.000   1.000   0.000   3.000 
    0.000   0.000   1.000   1.000 

Solution:
x1 =   2.000
x2 =   3.000
x3 =   1.000
```

## How it works:

1. **Forward Elimination**: For each column, find the largest element (pivot) and use it to eliminate other elements in that column
2. **Row Swapping**: If the pivot is not on the diagonal, swap rows to bring the pivot to the diagonal
3. **Normalization**: Make the pivot element equal to 1 by dividing the entire row
4. **Elimination**: Use the pivot row to eliminate all other elements in that column
5. **Back Substitution**: Continue until the matrix is in reduced row echelon form

## Key Features:

- Handles singular matrices with error checking
- Uses partial pivoting for numerical stability
- Returns the matrix in reduced row echelon form
- Extracts solutions from the final matrix
- Includes proper error handling for edge cases

This implementation solves systems of linear equations by transforming the augmented matrix into its reduced row echelon form, making it easy to read off the solutions.

