# Ukkonen's Suffix Tree Algorithm in Lua

Here's a complete implementation of Ukkonen's algorithm for building suffix trees in Lua:

```lua
-- Ukkonen's Suffix Tree Algorithm Implementation in Lua

local SuffixTree = {}

function SuffixTree:new(text)
    local obj = {
        text = text,
        root = { children = {}, suffix_link = nil },
        active_node = nil,
        active_edge = 0,
        active_length = 0,
        remaining_suffix_count = 0,
        leaf_end = 0,
        root_end = nil,
        split_end = nil,
        size = #text
    }
    
    setmetatable(obj, { __index = SuffixTree })
    
    -- Initialize the tree with first character
    obj:build_tree()
    
    return obj
end

function SuffixTree:build_tree()
    self.active_node = self.root
    
    for i = 1, self.size do
        self:extend_suffix_tree(i)
    end
end

function SuffixTree:extend_suffix_tree(pos)
    self.leaf_end = pos
    self.remaining_suffix_count = self.remaining_suffix_count + 1
    
    local last_created_internal_node = nil
    
    while self.remaining_suffix_count > 0 do
        if self.active_length == 0 then
            self.active_edge = pos
        end
        
        -- Check if edge exists
        local current_node = self.active_node.children[self.text:sub(self.active_edge, self.active_edge)]
        
        if not current_node then
            -- Create new leaf node
            local leaf = {
                start = pos,
                end = self.leaf_end,
                suffix_index = -1
            }
            
            self.active_node.children[self.text:sub(self.active_edge, self.active_edge)] = leaf
            
            -- Check for suffix link
            if last_created_internal_node ~= nil then
                last_created_internal_node.suffix_link = self.active_node
                last_created_internal_node = nil
            end
        else
            -- Walk down the edge
            local edge_length = current_node.end - current_node.start + 1
            
            if self.active_length >= edge_length then
                self.active_edge = self.active_edge + edge_length
                self.active_length = self.active_length - edge_length
                self.active_node = current_node
                goto continue
            end
            
            -- Check if we're at the end of the edge
            if self.text:sub(current_node.start + self.active_length, 
                             current_node.start + self.active_length) == 
               self.text:sub(pos, pos) then
                
                -- We found a match - move to next character
                if last_created_internal_node ~= nil and self.active_node ~= self.root then
                    last_created_internal_node.suffix_link = self.active_node
                    last_created_internal_node = nil
                end
                
                self.active_length = self.active_length + 1
                goto continue
            end
            
            -- Split the edge
            local split_end = current_node.start + self.active_length - 1
            
            local split_node = {
                start = current_node.start,
                end = split_end,
                children = {}
            }
            
            local new_internal = {
                start = current_node.start + self.active_length,
                end = current_node.end,
                children = {}
            }
            
            -- Update the original node
            current_node.start = current_node.start + self.active_length
            
            -- Add new internal node to split_node's children
            split_node.children[self.text:sub(current_node.start, current_node.start)] = new_internal
            
            -- Add the leaf
            local leaf = {
                start = pos,
                end = self.leaf_end,
                suffix_index = -1
            }
            
            split_node.children[self.text:sub(pos, pos)] = leaf
            
            -- Update the parent's child reference
            if self.active_node ~= self.root then
                self.active_node.children[self.text:sub(self.active_edge, self.active_edge)] = split_node
                
                last_created_internal_node = split_node
            else
                self.active_node.children[self.text:sub(self.active_edge, self.active_edge)] = split_node
                last_created_internal_node = split_node
            end
            
            -- Update the new internal node's children
            new_internal.children[self.text:sub(current_node.start, current_node.start)] = current_node
            
            if last_created_internal_node ~= nil and self.active_node ~= self.root then
                last_created_internal_node.suffix_link = split_node
                last_created_internal_node = nil
            end
            
            current_node.end = split_end
        end
        
        self.remaining_suffix_count = self.remaining_suffix_count - 1
        
        if self.active_node == self.root and self.active_length > 0 then
            self.active_length = self.active_length - 1
            self.active_edge = pos - self.remaining_suffix_count + 1
        elseif self.active_node ~= self.root then
            self.active_node = self.active_node.suffix_link or self.root
        end
        
        ::continue::
    end
end

function SuffixTree:print_tree(node, prefix, is_last)
    if not node then return end
    
    if not prefix then prefix = "" end
    if not is_last then is_last = true end
    
    -- Print current node
    if node.start and node.end then
        local edge_label = self.text:sub(node.start, node.end)
        print(prefix .. (is_last and "└── " or "├── ") .. edge_label)
        
        if node.children then
            local children = {}
            for k, v in pairs(node.children) do
                table.insert(children, {key = k, value = v})
            end
            
            -- Sort children by key for consistent output
            table.sort(children, function(a, b) return a.key < b.key end)
            
            for i, child in ipairs(children) do
                local is_last_child = (i == #children)
                local new_prefix = prefix .. (is_last and "    " or "│   ")
                self:print_tree(child.value, new_prefix, is_last_child)
            end
        end
    else
        print(prefix .. (is_last and "└── " or "├── ") .. "(root)")
        
        if node.children then
            local children = {}
            for k, v in pairs(node.children) do
                table.insert(children, {key = k, value = v})
            end
            
            -- Sort children by key for consistent output
            table.sort(children, function(a, b) return a.key < b.key end)
            
            for i, child in ipairs(children) do
                local is_last_child = (i == #children)
                local new_prefix = prefix .. (is_last and "    " or "│   ")
                self:print_tree(child.value, new_prefix, is_last_child)
            end
        end
    end
end

-- Example usage
local function example()
    print("Building suffix tree for 'banana$'")
    
    local text = "banana$"
    local st = SuffixTree:new(text)
    
    print("\nSuffix Tree Structure:")
    st:print_tree(st.root)
    
    print("\nOriginal text:", text)
    print("Text length:", #text)
    
    -- Show all suffixes
    print("\nAll suffixes:")
    for i = 1, #text do
        local suffix = text:sub(i)
        print(i .. ": " .. suffix)
    end
end

-- Run the example
example()

-- More comprehensive example with string matching
local function advanced_example()
    print("\n" .. string.rep("=", 50))
    print("Advanced Example with 'abcabxabcd$'")
    
    local text = "abcabxabcd$"
    local st = SuffixTree:new(text)
    
    print("\nSuffix Tree Structure:")
    st:print_tree(st.root)
    
    -- Show suffixes
    print("\nAll suffixes in order:")
    for i = 1, #text do
        local suffix = text:sub(i)
        print("Suffix " .. (i-1) .. ": " .. suffix)
    end
    
    print("\nTree built successfully!")
end

-- Run advanced example
advanced_example()
```

## Key Features of this Implementation:

### **Core Components:**
1. **Node Structure**: Each node contains start/end positions, children, and suffix links
2. **Active Point Management**: Tracks the current position in the algorithm
3. **Edge Splitting**: Handles splitting edges when new characters are added
4. **Suffix Links**: Implements efficient navigation between nodes

### **Algorithm Steps:**
1. Initialize with first character
2. For each subsequent character:
   - Extend existing suffixes
   - Create new leaves when needed
   - Handle edge splits appropriately
   - Maintain suffix links for optimization

### **Time Complexity:**
- **Construction**: O(n) where n is the length of input string
- **Space**: O(n) space complexity

### **Usage Example:**
```lua
-- Create a suffix tree
local st = SuffixTree:new("banana$")

-- Visualize the tree structure
st:print_tree(st.root)
```

This implementation follows Ukkonen's algorithm closely while being adapted to Lua's syntax and data structures. The tree can be visualized with the `print_tree` method, showing the hierarchical structure of suffixes.

The algorithm efficiently builds a suffix tree in linear time, making it suitable for practical applications like string matching, pattern searching, and bioinformatics applications.

