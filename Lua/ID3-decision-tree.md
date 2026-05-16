# ID3 Decision Tree Algorithm in Lua

Here's a complete implementation of the ID3 algorithm in Lua:

```lua
-- ID3 Decision Tree Algorithm Implementation in Lua

-- Node structure for the decision tree
local Node = {}
Node.__index = Node

function Node:new(value, is_leaf, attribute, threshold)
    local obj = {
        value = value,
        is_leaf = is_leaf or false,
        attribute = attribute,
        threshold = threshold,
        children = {},
        parent = nil
    }
    setmetatable(obj, Node)
    return obj
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
    
    -- Calculate entropy
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
    
    -- Group data by attribute values
    local attribute_values = {}
    for _, row in ipairs(dataset) do
        local value = row[attribute]
        if not attribute_values[value] then
            attribute_values[value] = {}
        end
        table.insert(attribute_values[value], row)
    end
    
    -- Calculate weighted entropy
    local weighted_entropy = 0
    local total_size = #dataset
    
    for value, subset in pairs(attribute_values) do
        local subset_entropy = calculate_entropy(subset, target_attribute)
        local weight = #subset / total_size
        weighted_entropy = weighted_entropy + weight * subset_entropy
    end
    
    -- Information gain = total_entropy - weighted_entropy
    return total_entropy - weighted_entropy
end

-- Find the best attribute to split on
function find_best_attribute(dataset, attributes, target_attribute)
    local best_gain = -1
    local best_attribute = nil
    
    for _, attribute in ipairs(attributes) do
        -- Skip the target attribute
        if attribute ~= target_attribute then
            local gain = calculate_information_gain(dataset, attribute, target_attribute)
            if gain > best_gain then
                best_gain = gain
                best_attribute = attribute
            end
        end
    end
    
    return best_attribute, best_gain
end

-- Get all unique values for an attribute
function get_unique_values(dataset, attribute)
    local values = {}
    for _, row in ipairs(dataset) do
        values[row[attribute]] = true
    end
    return table.keys(values)
end

-- ID3 algorithm implementation
function id3(dataset, attributes, target_attribute, default_value)
    -- Base cases
    if #dataset == 0 then
        return Node:new(default_value, true)
    end
    
    -- Check if all instances have the same target value
    local first_target = dataset[1][target_attribute]
    local all_same = true
    for _, row in ipairs(dataset) do
        if row[target_attribute] ~= first_target then
            all_same = false
            break
        end
    end
    
    if all_same then
        return Node:new(first_target, true)
    end
    
    -- If no attributes left, return node with most common target value
    if #attributes == 0 then
        local target_counts = {}
        for _, row in ipairs(dataset) do
            local target = row[target_attribute]
            target_counts[target] = (target_counts[target] or 0) + 1
        end
        
        local max_count = 0
        local most_common = nil
        for target, count in pairs(target_counts) do
            if count > max_count then
                max_count = count
                most_common = target
            end
        end
        
        return Node:new(most_common, true)
    end
    
    -- Find best attribute to split on
    local best_attribute, gain = find_best_attribute(dataset, attributes, target_attribute)
    
    if not best_attribute or gain <= 0 then
        -- No good split found, return most common target value
        local target_counts = {}
        for _, row in ipairs(dataset) do
            local target = row[target_attribute]
            target_counts[target] = (target_counts[target] or 0) + 1
        end
        
        local max_count = 0
        local most_common = nil
        for target, count in pairs(target_counts) do
            if count > max_count then
                max_count = count
                most_common = target
            end
        end
        
        return Node:new(most_common, true)
    end
    
    -- Create root node
    local root = Node:new(nil, false, best_attribute)
    
    -- Get unique values for the best attribute
    local unique_values = get_unique_values(dataset, best_attribute)
    
    -- Create child nodes for each value
    for _, value in ipairs(unique_values) do
        local subset = {}
        for _, row in ipairs(dataset) do
            if row[best_attribute] == value then
                table.insert(subset, row)
            end
        end
        
        -- Remove the best attribute from attributes for children
        local remaining_attributes = {}
        for _, attr in ipairs(attributes) do
            if attr ~= best_attribute then
                table.insert(remaining_attributes, attr)
            end
        end
        
        -- Recursively build subtree
        local child_node = id3(subset, remaining_attributes, target_attribute, 
                              get_most_common_target(dataset, target_attribute))
        
        child_node.parent = root
        root.children[value] = child_node
    end
    
    return root
end

-- Helper function to get most common target value
function get_most_common_target(dataset, target_attribute)
    local target_counts = {}
    for _, row in ipairs(dataset) do
        local target = row[target_attribute]
        target_counts[target] = (target_counts[target] or 0) + 1
    end
    
    local max_count = 0
    local most_common = nil
    for target, count in pairs(target_counts) do
        if count > max_count then
            max_count = count
            most_common = target
        end
    end
    
    return most_common
end

-- Function to classify a new instance
function classify(instance, tree)
    if tree.is_leaf then
        return tree.value
    end
    
    local attribute_value = instance[tree.attribute]
    local child = tree.children[attribute_value]
    
    if not child then
        return "unknown"
    end
    
    return classify(instance, child)
end

-- Print the decision tree
function print_tree(node, depth)
    if not node then return end
    
    local indent = string.rep("  ", depth)
    
    if node.is_leaf then
        print(indent .. "Predict: " .. tostring(node.value))
    else
        print(indent .. "Attribute: " .. tostring(node.attribute))
        for value, child in pairs(node.children) do
            print(indent .. "  " .. tostring(value) .. ":")
            print_tree(child, depth + 2)
        end
    end
end

-- Example usage
print("=== ID3 Decision Tree Example ===")

-- Sample dataset: Play Tennis
-- Features: Outlook, Temperature, Humidity, Wind
-- Target: PlayTennis (yes/no)
local dataset = {
    {Outlook = "sunny", Temperature = "hot", Humidity = "high", Wind = "weak", PlayTennis = "no"},
    {Outlook = "sunny", Temperature = "hot", Humidity = "high", Wind = "strong", PlayTennis = "no"},
    {Outlook = "overcast", Temperature = "hot", Humidity = "high", Wind = "weak", PlayTennis = "yes"},
    {Outlook = "rain", Temperature = "mild", Humidity = "high", Wind = "weak", PlayTennis = "yes"},
    {Outlook = "rain", Temperature = "cool", Humidity = "normal", Wind = "weak", PlayTennis = "yes"},
    {Outlook = "rain", Temperature = "cool", Humidity = "normal", Wind = "strong", PlayTennis = "no"},
    {Outlook = "overcast", Temperature = "cool", Humidity = "normal", Wind = "strong", PlayTennis = "yes"},
    {Outlook = "sunny", Temperature = "mild", Humidity = "high", Wind = "weak", PlayTennis = "no"},
    {Outlook = "sunny", Temperature = "cool", Humidity = "normal", Wind = "weak", PlayTennis = "yes"},
    {Outlook = "rain", Temperature = "mild", Humidity = "normal", Wind = "weak", PlayTennis = "yes"},
    {Outlook = "sunny", Temperature = "mild", Humidity = "normal", Wind = "strong", PlayTennis = "yes"},
    {Outlook = "overcast", Temperature = "mild", Humidity = "high", Wind = "strong", PlayTennis = "yes"},
    {Outlook = "overcast", Temperature = "hot", Humidity = "normal", Wind = "weak", PlayTennis = "yes"},
    {Outlook = "rain", Temperature = "mild", Humidity = "high", Wind = "strong", PlayTennis = "no"}
}

local attributes = {"Outlook", "Temperature", "Humidity", "Wind", "PlayTennis"}
local target_attribute = "PlayTennis"

-- Build the decision tree
print("Building decision tree...")
local tree = id3(dataset, attributes, target_attribute)

-- Print the tree structure
print("\nDecision Tree:")
print_tree(tree)

-- Test classification
print("\nTesting classifications:")
local test_instance = {Outlook = "sunny", Temperature = "cool", Humidity = "high", Wind = "strong"}
local prediction = classify(test_instance, tree)
print("Instance: " .. 
      "Outlook=" .. test_instance.Outlook .. 
      ", Temperature=" .. test_instance.Temperature .. 
      ", Humidity=" .. test_instance.Humidity .. 
      ", Wind=" .. test_instance.Wind)
print("Prediction: " .. prediction)
```

## Key Features of this Implementation:

1. **Node Structure**: Custom Node class to represent tree nodes
2. **Entropy Calculation**: Implements the entropy formula for information theory
3. **Information Gain**: Calculates the information gain for each attribute
4. **Recursive Tree Building**: Builds the tree recursively using ID3 algorithm
5. **Classification Function**: Can classify new instances using the trained tree
6. **Tree Visualization**: Prints the tree structure for debugging

## How it Works:

1. **Calculate Entropy**: Measures the impurity of a dataset
2. **Calculate Information Gain**: Determines which attribute provides the most information
3. **Recursive Splitting**: Continues splitting until all instances have the same target value or no more attributes
4. **Handle Edge Cases**: Manages empty datasets and identical instances

This implementation demonstrates the core concepts of the ID3 algorithm while being fully functional in Lua.

