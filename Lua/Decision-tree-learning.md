# Decision Tree Learning Algorithm in Lua

Here's a complete implementation of a decision tree learning algorithm in Lua:

```lua
-- Decision Tree Learning Algorithm in Lua

-- Node structure for the decision tree
local Node = {}
Node.__index = Node

function Node:new(attribute, value, is_leaf, prediction)
    local node = {
        attribute = attribute,  -- attribute to split on
        value = value,          -- value of the attribute
        is_leaf = is_leaf,      -- boolean indicating if it's a leaf node
        prediction = prediction, -- prediction value for leaf nodes
        children = {}           -- child nodes
    }
    setmetatable(node, Node)
    return node
end

-- Decision Tree class
local DecisionTree = {}
DecisionTree.__index = DecisionTree

function DecisionTree:new()
    local tree = {
        root = nil
    }
    setmetatable(tree, DecisionTree)
    return tree
end

-- Calculate entropy of a dataset
function calculate_entropy(dataset, target_attribute)
    local counts = {}
    local total = #dataset
    
    -- Count occurrences of each target value
    for _, row in ipairs(dataset) do
        local target_value = row[target_attribute]
        counts[target_value] = (counts[target_value] or 0) + 1
    end
    
    local entropy = 0
    for _, count in pairs(counts) do
        local probability = count / total
        entropy = entropy - probability * math.log2(probability)
    end
    
    return entropy
end

-- Calculate information gain
function calculate_information_gain(dataset, attribute, target_attribute)
    local total_entropy = calculate_entropy(dataset, target_attribute)
    local attribute_values = {}
    local attribute_counts = {}
    
    -- Group data by attribute values
    for _, row in ipairs(dataset) do
        local attr_value = row[attribute]
        if not attribute_values[attr_value] then
            attribute_values[attr_value] = {}
        end
        table.insert(attribute_values[attr_value], row)
        attribute_counts[attr_value] = (attribute_counts[attr_value] or 0) + 1
    end
    
    local weighted_entropy = 0
    local total = #dataset
    
    for attr_value, subset in pairs(attribute_values) do
        local subset_entropy = calculate_entropy(subset, target_attribute)
        local weight = attribute_counts[attr_value] / total
        weighted_entropy = weighted_entropy + weight * subset_entropy
    end
    
    return total_entropy - weighted_entropy
end

-- Find the best attribute to split on
function find_best_attribute(dataset, attributes, target_attribute)
    local best_attribute = nil
    local best_gain = -1
    
    for _, attribute in ipairs(attributes) do
        if attribute ~= target_attribute then
            local gain = calculate_information_gain(dataset, attribute, target_attribute)
            if gain > best_gain then
                best_gain = gain
                best_attribute = attribute
            end
        end
    end
    
    return best_attribute
end

-- Get unique values of an attribute
function get_unique_values(dataset, attribute)
    local values = {}
    for _, row in ipairs(dataset) do
        values[row[attribute]] = true
    end
    return table.keys(values)
end

-- Get subset of dataset where attribute equals value
function get_subset(dataset, attribute, value)
    local subset = {}
    for _, row in ipairs(dataset) do
        if row[attribute] == value then
            table.insert(subset, row)
        end
    end
    return subset
end

-- Check if all instances have the same target value
function all_same_class(dataset, target_attribute)
    if #dataset == 0 then return nil end
    
    local first_class = dataset[1][target_attribute]
    for _, row in ipairs(dataset) do
        if row[target_attribute] ~= first_class then
            return nil
        end
    end
    
    return first_class
end

-- ID3 Algorithm implementation
function id3(dataset, attributes, target_attribute, default_class)
    -- Base cases
    if #dataset == 0 then
        return Node:new(nil, nil, true, default_class)
    end
    
    local class = all_same_class(dataset, target_attribute)
    if class then
        return Node:new(nil, nil, true, class)
    end
    
    if #attributes == 1 then  -- Only target attribute left
        local most_common = get_most_common_class(dataset, target_attribute)
        return Node:new(nil, nil, true, most_common)
    end
    
    -- Find best attribute to split on
    local best_attribute = find_best_attribute(dataset, attributes, target_attribute)
    
    if not best_attribute then
        local most_common = get_most_common_class(dataset, target_attribute)
        return Node:new(nil, nil, true, most_common)
    end
    
    -- Create root node
    local root = Node:new(best_attribute, nil, false, nil)
    
    -- Get unique values of best attribute
    local unique_values = get_unique_values(dataset, best_attribute)
    
    -- Recursively build subtrees
    for _, value in ipairs(unique_values) do
        local subset = get_subset(dataset, best_attribute, value)
        local new_attributes = {}
        for _, attr in ipairs(attributes) do
            if attr ~= best_attribute then
                table.insert(new_attributes, attr)
            end
        end
        
        local subtree = id3(subset, new_attributes, target_attribute, 
                           get_most_common_class(dataset, target_attribute))
        subtree.value = value
        table.insert(root.children, subtree)
    end
    
    return root
end

-- Get the most common class in dataset
function get_most_common_class(dataset, target_attribute)
    local counts = {}
    for _, row in ipairs(dataset) do
        local class = row[target_attribute]
        counts[class] = (counts[class] or 0) + 1
    end
    
    local max_count = 0
    local most_common = nil
    for class, count in pairs(counts) do
        if count > max_count then
            max_count = count
            most_common = class
        end
    end
    
    return most_common
end

-- Train the decision tree
function DecisionTree:train(dataset, attributes, target_attribute)
    self.root = id3(dataset, attributes, target_attribute, 
                   get_most_common_class(dataset, target_attribute))
end

-- Make prediction for a single instance
function DecisionTree:predict(instance)
    local current = self.root
    
    while not current.is_leaf do
        local attr_value = instance[current.attribute]
        
        -- Find matching child
        local child = nil
        for _, c in ipairs(current.children) do
            if c.value == attr_value then
                child = c
                break
            end
        end
        
        if not child then
            -- No matching child, return default prediction
            return current.prediction
        end
        
        current = child
    end
    
    return current.prediction
end

-- Print the decision tree
function print_tree(node, depth)
    if not node then return end
    
    local indent = string.rep("  ", depth)
    
    if node.is_leaf then
        print(indent .. "Predict: " .. tostring(node.prediction))
    else
        print(indent .. "Attribute: " .. tostring(node.attribute))
        for _, child in ipairs(node.children) do
            print(indent .. "  Value: " .. tostring(child.value))
            print_tree(child, depth + 2)
        end
    end
end

-- Example usage
print("=== Decision Tree Learning Example ===")

-- Sample dataset: Play Tennis
local dataset = {
    {outlook = "sunny", temperature = "hot", humidity = "high", windy = "false", play = "no"},
    {outlook = "sunny", temperature = "hot", humidity = "high", windy = "true", play = "no"},
    {outlook = "overcast", temperature = "hot", humidity = "high", windy = "false", play = "yes"},
    {outlook = "rain", temperature = "mild", humidity = "high", windy = "false", play = "yes"},
    {outlook = "rain", temperature = "cool", humidity = "normal", windy = "false", play = "yes"},
    {outlook = "rain", temperature = "cool", humidity = "normal", windy = "true", play = "no"},
    {outlook = "overcast", temperature = "cool", humidity = "normal", windy = "true", play = "yes"},
    {outlook = "sunny", temperature = "mild", humidity = "high", windy = "false", play = "no"},
    {outlook = "sunny", temperature = "cool", humidity = "normal", windy = "false", play = "yes"},
    {outlook = "rain", temperature = "mild", humidity = "normal", windy = "false", play = "yes"},
    {outlook = "sunny", temperature = "mild", humidity = "normal", windy = "true", play = "yes"},
    {outlook = "overcast", temperature = "mild", humidity = "high", windy = "true", play = "yes"},
    {outlook = "overcast", temperature = "hot", humidity = "normal", windy = "false", play = "yes"},
    {outlook = "rain", temperature = "mild", humidity = "high", windy = "true", play = "no"}
}

local attributes = {"outlook", "temperature", "humidity", "windy", "play"}
local target_attribute = "play"

-- Create and train the decision tree
local tree = DecisionTree:new()
tree:train(dataset, attributes, target_attribute)

-- Print the tree structure
print("Decision Tree Structure:")
print_tree(tree.root, 0)

-- Make predictions
print("\n=== Predictions ===")
local test_instance = {outlook = "sunny", temperature = "cool", humidity = "normal", windy = "false"}
local prediction = tree:predict(test_instance)
print("Test instance: " .. 
      "outlook=" .. test_instance.outlook .. ", " ..
      "temperature=" .. test_instance.temperature .. ", " ..
      "humidity=" .. test_instance.humidity .. ", " ..
      "windy=" .. test_instance.windy)
print("Prediction: " .. prediction)

local test_instance2 = {outlook = "rain", temperature = "mild", humidity = "high", windy = "false"}
local prediction2 = tree:predict(test_instance2)
print("Test instance: " .. 
      "outlook=" .. test_instance2.outlook .. ", " ..
      "temperature=" .. test_instance2.temperature .. ", " ..
      "humidity=" .. test_instance2.humidity .. ", " ..
      "windy=" .. test_instance2.windy)
print("Prediction: " .. prediction2)
```

## Key Features of this Implementation:

1. **Node Structure**: Represents tree nodes with attributes, values, and children
2. **Entropy Calculation**: Computes entropy for information gain calculation
3. **Information Gain**: Measures the reduction in entropy when splitting
4. **ID3 Algorithm**: Implements the classic ID3 decision tree learning algorithm
5. **Training Method**: Builds the decision tree from training data
6. **Prediction Method**: Makes predictions on new instances
7. **Tree Visualization**: Prints the tree structure for debugging

## How it works:

1. **Entropy Calculation**: Measures the impurity of data
2. **Information Gain**: Determines the best attribute to split on
3. **Recursive Splitting**: Continues splitting until all instances have the same class
4. **Leaf Creation**: Creates leaf nodes with predictions
5. **Prediction**: Traverses the tree to make predictions

This implementation follows the ID3 algorithm and can be extended with additional features like pruning, handling continuous attributes, or using different splitting criteria like Gini impurity.

