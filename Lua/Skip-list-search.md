# Skip List Search Algorithm in Lua

```lua
-- Skip List Node structure
local Node = {}
Node.__index = Node

function Node:new(value, level)
    local obj = {
        value = value,
        forward = {} -- Array of pointers to next nodes at each level
    }
    setmetatable(obj, Node)
    return obj
end

-- Skip List implementation
local SkipList = {}
SkipList.__index = SkipList

function SkipList:new(maxLevel, p)
    local obj = {
        header = Node:new(nil, maxLevel),
        level = 1,
        maxLevel = maxLevel,
        p = p -- Probability factor (usually 0.5)
    }
    setmetatable(obj, SkipList)
    return obj
end

-- Generate random level for new node
function SkipList:randomLevel()
    local level = 1
    while math.random() < self.p and level < self.maxLevel do
        level = level + 1
    end
    return level
end

-- Search algorithm
function SkipList:search(value)
    local current = self.header
    
    -- Start from the highest level and move down
    for i = self.level, 1, -1 do
        -- Move forward while the next node's value is less than target
        while current.forward[i] ~= nil and 
              current.forward[i].value < value do
            current = current.forward[i]
        end
    end
    
    -- Move one step forward to get the actual node
    current = current.forward[1]
    
    -- Check if we found the value
    if current ~= nil and current.value == value then
        return current.value
    else
        return nil -- Value not found
    end
end

-- Insert method for completeness
function SkipList:insert(value)
    local update = {}
    local current = self.header
    
    -- Find the position where value should be inserted
    for i = self.level, 1, -1 do
        while current.forward[i] ~= nil and 
              current.forward[i].value < value do
            current = current.forward[i]
        end
        update[i] = current
    end
    
    current = current.forward[1]
    
    -- If value already exists, don't insert
    if current ~= nil and current.value == value then
        return false
    end
    
    -- Create new node with random level
    local newLevel = self:randomLevel()
    
    -- Update level if new node is higher
    if newLevel > self.level then
        for i = self.level + 1, newLevel do
            update[i] = self.header
        end
        self.level = newLevel
    end
    
    -- Create new node
    local newNode = Node:new(value, newLevel)
    
    -- Insert node at all levels
    for i = 1, newLevel do
        newNode.forward[i] = update[i].forward[i]
        update[i].forward[i] = newNode
    end
    
    return true
end

-- Example usage
print("=== Skip List Search Example ===")

-- Create a skip list with max level 4 and probability 0.5
local skipList = SkipList:new(4, 0.5)

-- Insert some values
local values = {3, 6, 7, 9, 12, 19, 21, 25, 30}
for _, value in ipairs(values) do
    skipList:insert(value)
end

-- Search for values
local searchValues = {7, 15, 25, 35}
for _, value in ipairs(searchValues) do
    local result = skipList:search(value)
    if result then
        print("Found " .. value)
    else
        print("Value " .. value .. " not found")
    end
end

-- Visual representation of search path for value 19
print("\n=== Search Path for value 19 ===")
local current = skipList.header
print("Starting at header")

-- Show search path for value 19
for i = skipList.level, 1, -1 do
    local nextNode = current.forward[i]
    while nextNode ~= nil and nextNode.value < 19 do
        current = nextNode
        nextNode = current.forward[i]
    end
    print("Level " .. i .. ": visiting value " .. (current.value or "header"))
end

print("Final position: " .. (current.forward[1] and current.forward[1].value or "nil"))
```

## Algorithm Explanation

The skip list search algorithm works as follows:

1. **Start from the highest level** of the skip list
2. **Traverse forward** at each level while the next node's value is less than the target
3. **Move down** to the next lower level when the target value is reached or exceeded
4. **Continue** until reaching level 1
5. **Check the final position** to see if the target value was found

## Time Complexity
- **Average case**: O(log n)
- **Worst case**: O(n)
- **Space complexity**: O(n log n)

## Key Features
- Uses multiple levels of linked lists for faster searching
- Probabilistic approach to maintain balance
- Efficient search, insertion, and deletion operations
- Easy to implement compared to balanced trees

