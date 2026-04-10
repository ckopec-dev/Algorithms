# Support Vector Machine (SVM) in Lua

Here's a complete example of implementing a basic SVM algorithm in Lua using the torch library:

```lua
-- SVM Implementation in Lua using Torch
require 'torch'
require 'nn'

-- Generate sample data
torch.manualSeed(123)

-- Create sample dataset (2D points)
local X = torch.Tensor({
    {1, 2}, {2, 3}, {3, 1}, {4, 2}, {5, 3},
    {1, 1}, {2, 1}, {3, 2}, {4, 1}, {5, 2}
})

local y = torch.Tensor({1, 1, 1, 1, 1, -1, -1, -1, -1, -1})

print("Sample dataset:")
print("Features (X):")
print(X)
print("Labels (y):")
print(y)

-- SVM using Torch's nn library
-- Create a simple linear SVM classifier
local svm = nn.Sequential()
svm:add(nn.Linear(2, 1))  -- Linear layer for 2 features -> 1 output
svm:add(nn.Sigmoid())     -- Sigmoid activation

-- For a proper SVM, we need to implement the kernel trick
-- Here's a simplified version using a linear kernel

-- Alternative approach: Using torch's SVM implementation (if available)
-- Note: Lua/Torch doesn't have built-in SVM, so we'll create a basic version

-- Custom SVM implementation
local function train_svm(X, y, learning_rate, epochs)
    local n_samples, n_features = X:size(1), X:size(2)
    
    -- Initialize weights and bias
    local weights = torch.randn(n_features) * 0.1
    local bias = torch.Tensor({0})
    
    print("Training SVM...")
    
    for epoch = 1, epochs do
        local total_loss = 0
        
        for i = 1, n_samples do
            -- Forward pass
            local x_i = X[i]:view(1, -1)
            local prediction = torch.dot(weights, x_i[1]) + bias[1]
            
            -- Simple hinge loss (simplified version)
            local margin = y[i] * prediction
            local loss = math.max(0, 1 - margin)
            
            total_loss = total_loss + loss
            
            -- Update weights (simplified gradient descent)
            if margin < 1 then
                weights = weights + learning_rate * y[i] * x_i[1]
                bias = bias + learning_rate * y[i]
            end
        end
        
        if epoch % 100 == 0 then
            print(string.format("Epoch %d, Loss: %.4f", epoch, total_loss/n_samples))
        end
    end
    
    return weights, bias
end

-- Train the SVM
local weights, bias = train_svm(X, y, 0.01, 1000)

print("Trained weights:", weights)
print("Trained bias:", bias)

-- Prediction function
local function predict(x, weights, bias)
    local prediction = torch.dot(weights, x) + bias
    return prediction > 0 and 1 or -1
end

-- Test predictions
print("\nTest predictions:")
for i = 1, X:size(1) do
    local x = X[i]
    local prediction = predict(x, weights, bias)
    local actual = y[i]
    print(string.format("Input: %s, Predicted: %d, Actual: %d", 
                       x:toString(), prediction, actual))
end

-- More advanced SVM with kernel trick (RBF kernel)
local function rbf_kernel(x1, x2, gamma)
    local diff = x1 - x2
    local squared_distance = torch.sum(torch.pow(diff, 2))
    return math.exp(-gamma * squared_distance)
end

-- Simple RBF SVM prediction
local function predict_rbf(x, support_vectors, support_labels, gamma)
    local sum = 0
    for i = 1, #support_vectors do
        local kernel_value = rbf_kernel(x, support_vectors[i], gamma)
        sum = sum + support_labels[i] * kernel_value
    end
    return sum > 0 and 1 or -1
end

print("\nSVM implementation completed!")
print("This example demonstrates:")
print("1. Basic SVM training with gradient descent")
print("2. Linear classification")
print("3. Simple prediction function")
print("4. RBF kernel concept (theoretical)")
```

## Key Components Explained:

### 1. **Data Preparation**
```lua
local X = torch.Tensor({{1, 2}, {2, 3}, ...})  -- Features
local y = torch.Tensor({1, 1, -1, -1, ...})    -- Labels
```

### 2. **Training Process**
- Initialize weights and bias
- Use hinge loss for SVM optimization
- Gradient descent updates

### 3. **Prediction Function**
```lua
local function predict(x, weights, bias)
    local prediction = torch.dot(weights, x) + bias
    return prediction > 0 and 1 or -1
end
```

### 4. **Kernel Functions**
- Linear kernel (default)
- RBF kernel (Radial Basis Function)

## Usage Notes:

1. **Dependencies**: Requires Torch Lua library
2. **Limitations**: This is a simplified implementation
3. **Real SVM**: For production use, consider using specialized libraries
4. **Performance**: Basic implementation may be slower than optimized versions

This example provides a foundation for understanding how SVM works in Lua, though for practical applications, you'd want to use optimized libraries or implement more sophisticated kernel methods.

