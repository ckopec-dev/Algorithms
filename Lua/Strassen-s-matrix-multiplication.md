# Strassen's Matrix Multiplication in Lua

Here's an implementation of Strassen's matrix multiplication algorithm in Lua:

```lua
-- Function to perform Strassen's matrix multiplication
function strassen_multiply(A, B)
    local n = #A
    
    -- Base case: if matrix is 1x1, return direct multiplication
    if n == 1 then
        return {{A[1][1] * B[1][1]}}
    end
    
    -- Split matrices into quadrants
    local half = math.floor(n / 2)
    
    -- Initialize submatrices
    local A11 = {}
    local A12 = {}
    local A21 = {}
    local A22 = {}
    local B11 = {}
    local B12 = {}
    local B21 = {}
    local B22 = {}
    
    -- Fill submatrices
    for i = 1, half do
        A11[i] = {}
        A12[i] = {}
        A21[i] = {}
        A22[i] = {}
        B11[i] = {}
        B12[i] = {}
        B21[i] = {}
        B22[i] = {}
        
        for j = 1, half do
            A11[i][j] = A[i][j]
            A12[i][j] = A[i][j + half]
            A21[i][j] = A[i + half][j]
            A22[i][j] = A[i + half][j + half]
            
            B11[i][j] = B[i][j]
            B12[i][j] = B[i][j + half]
            B21[i][j] = B[i + half][j]
            B22[i][j] = B[i + half][j + half]
        end
    end
    
    -- Strassen's seven products
    local M1 = strassen_multiply(matrix_add(A11, A22), matrix_add(B11, B22))
    local M2 = strassen_multiply(matrix_add(A21, A22), B11)
    local M3 = strassen_multiply(A11, matrix_subtract(B12, B22))
    local M4 = strassen_multiply(A22, matrix_subtract(B21, B11))
    local M5 = strassen_multiply(matrix_add(A11, A12), B22)
    local M6 = strassen_multiply(matrix_subtract(A21, A11), matrix_add(B11, B12))
    local M7 = strassen_multiply(matrix_subtract(A12, A22), matrix_add(B21, B22))
    
    -- Calculate result quadrants
    local C11 = matrix_add(matrix_subtract(matrix_add(M1, M4), M5), M7)
    local C12 = matrix_add(M3, M5)
    local C21 = matrix_add(M2, M4)
    local C22 = matrix_add(matrix_subtract(matrix_add(M1, M3), M2), M6)
    
    -- Combine quadrants into result matrix
    local C = {}
    for i = 1, n do
        C[i] = {}
        for j = 1, n do
            if i <= half and j <= half then
                C[i][j] = C11[i][j]
            elseif i <= half and j > half then
                C[i][j] = C12[i][j - half]
            elseif i > half and j <= half then
                C[i][j] = C21[i - half][j]
            else
                C[i][j] = C22[i - half][j - half]
            end
        end
    end
    
    return C
end

-- Helper function to add two matrices
function matrix_add(A, B)
    local result = {}
    local n = #A
    
    for i = 1, n do
        result[i] = {}
        for j = 1, n do
            result[i][j] = A[i][j] + B[i][j]
        end
    end
    
    return result
end

-- Helper function to subtract two matrices
function matrix_subtract(A, B)
    local result = {}
    local n = #A
    
    for i = 1, n do
        result[i] = {}
        for j = 1, n do
            result[i][j] = A[i][j] - B[i][j]
        end
    end
    
    return result
end

-- Helper function to print a matrix
function print_matrix(matrix)
    for i = 1, #matrix do
        local row = ""
        for j = 1, #matrix[i] do
            row = row .. matrix[i][j] .. " "
        end
        print(row)
    end
    print()
end

-- Example usage
print("Strassen's Matrix Multiplication Example")
print("======================================")

-- Define two 4x4 matrices
local A = {
    {1, 2, 3, 4},
    {5, 6, 7, 8},
    {9, 10, 11, 12},
    {13, 14, 15, 16}
}

local B = {
    {1, 0, 0, 0},
    {0, 1, 0, 0},
    {0, 0, 1, 0},
    {0, 0, 0, 1}
}

print("Matrix A:")
print_matrix(A)

print("Matrix B:")
print_matrix(B)

print("Result of A × B (using Strassen's algorithm):")
local result = strassen_multiply(A, B)
print_matrix(result)
```

## Key Features of this Implementation:

1. **Recursive Approach**: Uses the divide-and-conquer principle of Strassen's algorithm
2. **Seven Multiplications**: Implements the seven products required by Strassen's method
3. **Matrix Splitting**: Properly splits matrices into quadrants
4. **Base Case Handling**: Handles 1x1 matrices directly
5. **Helper Functions**: Includes matrix addition and subtraction utilities
6. **Quadrant Reassembly**: Correctly combines the results back into the final matrix

## Time Complexity:
- **Strassen's Algorithm**: O(n^2.807)
- **Standard Algorithm**: O(n^3)
- **Space Complexity**: O(n^2)

This implementation demonstrates the theoretical advantages of Strassen's algorithm, though in practice, the overhead of recursion and additional operations may make it less efficient for small matrices.

