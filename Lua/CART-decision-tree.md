# CART Decision Tree Algorithm in Lua

Here's a complete implementation of a Classification and Regression Tree (CART) algorithm in Lua:

```lua
-- CART Decision Tree Implementation in Lua

-- Node structure for the decision tree
local Node = {}
Node.__index = Node

function Node:new(feature, threshold, left, right, prediction)
    local node = {
        feature = feature,      -- feature index for splitting
        threshold = threshold,  -- threshold value for splitting
        left = left,            -- left child node
        right = right,          -- right child node
        prediction = prediction -- prediction value (for leaf nodes)
    }
    return setmetatable(node, Node)
end

-- CART Decision Tree class
local CART = {}
CART.__index = CART

function CART:new()
    local tree = {
        root = nil,
        max_depth = 10,
        min_samples_split = 2,
        min_samples_leaf = 1
    }
    return setmetatable(tree, CART)
end

-- Calculate gini impurity
function CART:calculate_gini(labels)
    if #labels == 0 then return 0 end
    
    local counts = {}
    local total = #labels
    
    -- Count occurrences of each label
    for _, label in ipairs(labels) do
        counts[label] = (counts[label] or 0) + 1
    end
    
    local gini = 1.0
    for _, count in pairs(counts) do
        local probability = count / total
        gini = gini - probability * probability
    end
    
    return gini
end

-- Find the best split for a dataset
function CART:find_best_split(X, y)
    local best_gini = math.huge
    local best_feature = nil
    local best_threshold = nil
    local best_left_indices = {}
    local best_right_indices = {}
    
    local n_features = #X[1]
    local n_samples = #X
    
    -- Try each feature
    for feature = 1, n_features do
        local feature_values = {}
        for i = 1, n_samples do
            table.insert(feature_values, X[i][feature])
        end
        
        -- Sort values to find potential thresholds
        table.sort(feature_values)
        
        -- Try each unique value as threshold
        local unique_values = {}
        for _, val in ipairs(feature_values) do
            unique_values[val] = true
        end
        
        local thresholds = {}
        for val in pairs(unique_values) do
            table.insert(thresholds, val)
        end
        table.sort(thresholds)
        
        for i = 1, #thresholds - 1 do
            local threshold = (thresholds[i] + thresholds[i + 1]) / 2
            
            local left_labels = {}
            local right_labels = {}
            
            for j = 1, n_samples do
                if X[j][feature] <= threshold then
                    table.insert(left_labels, y[j])
                else
                    table.insert(right_labels, y[j])
                end
            end
            
            -- Calculate weighted gini
            local left_gini = self:calculate_gini(left_labels)
            local right_gini = self:calculate_gini(right_labels)
            
            local weight_left = #left_labels / n_samples
            local weight_right = #right_labels / n_samples
            
            local weighted_gini = weight_left * left_gini + weight_right * right_gini
            
            if weighted_gini < best_gini then
                best_gini = weighted_gini
                best_feature = feature
                best_threshold = threshold
                best_left_indices = left_labels
                best_right_indices = right_labels
            end
        end
    end
    
    return best_feature, best_threshold, best_gini
end

-- Build the decision tree recursively
function CART:build_tree(X, y, depth)
    local n_samples = #X
    local n_features = #X[1]
    
    -- Stopping criteria
    if depth >= self.max_depth or n_samples < self.min_samples_split or n_samples <= self.min_samples_leaf then
        -- Create leaf node with most common label
        local label_counts = {}
        for _, label in ipairs(y) do
            label_counts[label] = (label_counts[label] or 0) + 1
        end
        
        local max_count = 0
        local prediction = nil
        for label, count in pairs(label_counts) do
            if count > max_count then
                max_count = count
                prediction = label
            end
        end
        
        return Node:new(nil, nil, nil, nil, prediction)
    end
    
    -- Find best split
    local best_feature, best_threshold, best_gini = self:find_best_split(X, y)
    
    if best_feature == nil then
        -- No good split found, create leaf node
        local label_counts = {}
        for _, label in ipairs(y) do
            label_counts[label] = (label_counts[label] or 0) + 1
        end
        
        local max_count = 0
        local prediction = nil
        for label, count in pairs(label_counts) do
            if count > max_count then
                max_count = count
                prediction = label
            end
        end
        
        return Node:new(nil, nil, nil, nil, prediction)
    end
    
    -- Split data
    local left_X = {}
    local left_y = {}
    local right_X = {}
    local right_y = {}
    
    for i = 1, n_samples do
        if X[i][best_feature] <= best_threshold then
            table.insert(left_X, X[i])
            table.insert(left_y, y[i])
        else
            table.insert(right_X, X[i])
            table.insert(right_y, y[i])
        end
    end
    
    -- Recursively build left and right subtrees
    local left_node = self:build_tree(left_X, left_y, depth + 1)
    local right_node = self:build_tree(right_X, right_y, depth + 1)
    
    return Node:new(best_feature, best_threshold, left_node, right_node, nil)
end

-- Train the decision tree
function CART:fit(X, y)
    self.root = self:build_tree(X, y, 0)
end

-- Make predictions
function CART:predict_sample(x)
    local current = self.root
    
    while current.prediction == nil do
        if x[current.feature] <= current.threshold then
            current = current.left
        else
            current = current.right
        end
    end
    
    return current.prediction
end

-- Predict multiple samples
function CART:predict(X)
    local predictions = {}
    for i = 1, #X do
        table.insert(predictions, self:predict_sample(X[i]))
    end
    return predictions
end

-- Print tree structure (for debugging)
function CART:print_tree(node, depth)
    if node == nil then return end
    
    local indent = string.rep("  ", depth)
    
    if node.prediction then
        print(indent .. "Predict: " .. tostring(node.prediction))
    else
        print(indent .. "Feature " .. tostring(node.feature) .. " <= " .. tostring(node.threshold))
        print(indent .. "Left:")
        self:print_tree(node.left, depth + 1)
        print(indent .. "Right:")
        self:print_tree(node.right, depth + 1)
    end
end

-- Example usage
print("CART Decision Tree Example")

-- Sample dataset (features: [age, income], labels: [0=No, 1=Yes])
local X = {
    {25, 50000},
    {35, 80000},
    {45, 120000},
    {23, 40000},
    {33, 70000},
    {55, 150000},
    {28, 60000},
    {41, 100000}
}

local y = {0, 1, 1, 0, 1, 1, 0, 1}

-- Create and train the model
local model = CART:new()
model.max_depth = 5
model.min_samples_split = 2

print("Training the CART model...")
model:fit(X, y)

print("\nDecision Tree Structure:")
model:print_tree(model.root, 0)

print("\nMaking predictions:")
local test_data = {
    {30, 65000},
    {50, 130000},
    {20, 35000}
}

local predictions = model:predict(test_data)
for i, pred in ipairs(predictions) do
    print("Sample " .. i .. ": Predicted = " .. pred)
end
```

## Key Features of this Implementation:

1. **Node Structure**: Represents tree nodes with feature, threshold, and child nodes
2. **Gini Impurity**: Used for measuring the quality of splits
3. **Recursive Tree Building**: Builds the tree by finding optimal splits
4. **Stopping Criteria**: Prevents overfitting with depth limits and sample requirements
5. **Prediction**: Makes predictions by traversing the tree
6. **Visualization**: Includes tree printing functionality for debugging

## Usage Example:

The example demonstrates training a CART model on a small dataset with age and income features to predict whether someone will buy a product (0=No, 1=Yes). The model learns to make decisions based on feature thresholds and can make predictions on new data.

This implementation provides a solid foundation for a CART decision tree that can be extended with additional features like pruning, handling of continuous variables, or different splitting criteria.

