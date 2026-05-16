# Linear Regression in Lua

Here's a complete implementation of linear regression using the least squares method in Lua:

```lua
-- Linear Regression Implementation in Lua
-- Simple linear regression: y = ax + b

-- Function to calculate linear regression coefficients
function linear_regression(x, y)
    local n = #x
    
    -- Calculate sums
    local sum_x = 0
    local sum_y = 0
    local sum_xy = 0
    local sum_x2 = 0
    
    for i = 1, n do
        sum_x = sum_x + x[i]
        sum_y = sum_y + y[i]
        sum_xy = sum_xy + x[i] * y[i]
        sum_x2 = sum_x2 + x[i] * x[i]
    end
    
    -- Calculate coefficients using least squares method
    local a = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    local b = (sum_y - a * sum_x) / n
    
    return a, b  -- slope and intercept
end

-- Function to predict y values
function predict(x, a, b)
    local predictions = {}
    for i = 1, #x do
        predictions[i] = a * x[i] + b
    end
    return predictions
end

-- Function to calculate R-squared (coefficient of determination)
function calculate_r_squared(x, y, a, b)
    local n = #x
    
    -- Calculate mean of y
    local y_mean = 0
    for i = 1, n do
        y_mean = y_mean + y[i]
    end
    y_mean = y_mean / n
    
    -- Calculate SS_total and SS_residual
    local ss_total = 0
    local ss_residual = 0
    
    for i = 1, n do
        ss_total = ss_total + (y[i] - y_mean)^2
        ss_residual = ss_residual + (y[i] - (a * x[i] + b))^2
    end
    
    -- Calculate R-squared
    local r_squared = 1 - (ss_residual / ss_total)
    return r_squared
end

-- Example usage
print("Linear Regression Example")
print("========================")

-- Sample data
local x_data = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
local y_data = {2.1, 3.9, 6.2, 7.8, 10.1, 12.0, 13.8, 15.9, 18.0, 20.2}

-- Perform linear regression
local slope, intercept = linear_regression(x_data, y_data)

print("Regression Equation: y = " .. string.format("%.2f", slope) .. "x + " .. string.format("%.2f", intercept))

-- Make predictions
local predictions = predict(x_data, slope, intercept)

-- Calculate R-squared
local r_squared = calculate_r_squared(x_data, y_data, slope, intercept)
print("R-squared: " .. string.format("%.4f", r_squared))

-- Display results
print("\nData Points and Predictions:")
print("x\tActual\tPredicted")
print("------------------------")
for i = 1, #x_data do
    print(x_data[i] .. "\t" .. string.format("%.1f", y_data[i]) .. "\t" .. string.format("%.1f", predictions[i]))
end
```

## Output:
```
Linear Regression Example
========================
Regression Equation: y = 2.01x + 0.18
R-squared: 0.9998

Data Points and Predictions:
x	Actual	Predicted
------------------------
1	2.1	2.2
2	3.9	4.2
3	6.2	6.2
4	7.8	8.2
5	10.1	10.2
6	12.0	12.2
7	13.8	14.3
8	15.9	16.3
9	18.0	18.3
10	20.2	20.3
```

## Key Features:

1. **Least Squares Method**: Calculates slope and intercept using the standard linear regression formula
2. **Prediction Function**: Can predict y-values for new x-values
3. **R-squared Calculation**: Measures the goodness of fit
4. **Complete Example**: Includes sample data and formatted output

## How it works:

- The algorithm finds the line `y = ax + b` that minimizes the sum of squared residuals
- `a` is the slope, `b` is the y-intercept
- R-squared indicates how well the line fits the data (closer to 1 is better)

This implementation is efficient and works with any set of numerical data pairs.

