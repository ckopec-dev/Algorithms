# Trie Algorithm Implementation in Lua

Here's a complete implementation of a Trie (Prefix Tree) data structure in Lua:

```lua
-- Trie Node structure
local TrieNode = {}
TrieNode.__index = TrieNode

function TrieNode:new()
    local node = {
        children = {},  -- Map of character to child nodes
        isEndOfWord = false  -- Flag to mark end of word
    }
    return setmetatable(node, TrieNode)
end

-- Trie class
local Trie = {}
Trie.__index = Trie

function Trie:new()
    local trie = {
        root = TrieNode:new()
    }
    return setmetatable(trie, Trie)
end

-- Insert a word into the trie
function Trie:insert(word)
    local current = self.root
    
    -- Traverse through each character of the word
    for i = 1, #word do
        local char = word:sub(i, i)
        
        -- If character doesn't exist, create new node
        if not current.children[char] then
            current.children[char] = TrieNode:new()
        end
        
        -- Move to next node
        current = current.children[char]
    end
    
    -- Mark end of word
    current.isEndOfWord = true
end

-- Search for a complete word
function Trie:search(word)
    local current = self.root
    
    -- Traverse through each character
    for i = 1, #word do
        local char = word:sub(i, i)
        
        -- If character doesn't exist, word not found
        if not current.children[char] then
            return false
        end
        
        current = current.children[char]
    end
    
    -- Return true only if it's end of a word
    return current.isEndOfWord
end

-- Check if any word starts with the given prefix
function Trie:startsWith(prefix)
    local current = self.root
    
    -- Traverse through each character of prefix
    for i = 1, #prefix do
        local char = prefix:sub(i, i)
        
        -- If character doesn't exist, prefix not found
        if not current.children[char] then
            return false
        end
        
        current = current.children[char]
    end
    
    -- If we reach here, prefix exists
    return true
end

-- Delete a word from the trie
function Trie:delete(word)
    local function deleteHelper(node, word, index)
        if index > #word then
            -- Base case: reached end of word
            if not node.isEndOfWord then
                return false  -- Word doesn't exist
            end
            
            node.isEndOfWord = false
            
            -- Return true if node has no children (can be deleted)
            return next(node.children) == nil
        end
        
        local char = word:sub(index, index)
        local childNode = node.children[char]
        
        if not childNode then
            return false  -- Word doesn't exist
        end
        
        local shouldDeleteChild = deleteHelper(childNode, word, index + 1)
        
        -- If child should be deleted and has no other children
        if shouldDeleteChild then
            node.children[char] = nil
            -- Return true if current node is not end of another word
            return not node.isEndOfWord and next(node.children) == nil
        end
        
        return false
    end
    
    deleteHelper(self.root, word, 1)
end

-- Example usage
local function example()
    local trie = Trie:new()
    
    -- Insert words
    trie:insert("apple")
    trie:insert("app")
    trie:insert("application")
    trie:insert("apply")
    
    print("Search results:")
    print("Search 'app': " .. tostring(trie:search("app")))        -- true
    print("Search 'apple': " .. tostring(trie:search("apple")))    -- true
    print("Search 'appl': " .. tostring(trie:search("appl")))      -- false
    
    print("\nPrefix search:")
    print("Starts with 'app': " .. tostring(trie:startsWith("app")))    -- true
    print("Starts with 'appl': " .. tostring(trie:startsWith("appl")))   -- true
    print("Starts with 'xyz': " .. tostring(trie:startsWith("xyz")))      -- false
    
    -- Delete a word
    trie:delete("app")
    print("\nAfter deleting 'app':")
    print("Search 'app': " .. tostring(trie:search("app")))        -- false
    print("Search 'apple': " .. tostring(trie:search("apple")))    -- true
    print("Starts with 'app': " .. tostring(trie:startsWith("app")))    -- true
end

-- Run example
example()
```

## Key Features of this Trie Implementation:

### **Core Operations:**
- **Insert**: O(m) time complexity where m is the length of the word
- **Search**: O(m) time complexity where m is the length of the word
- **StartsWith**: O(m) time complexity where m is the length of the prefix
- **Delete**: O(m) time complexity where m is the length of the word

### **Data Structure:**
- Uses a hash table (Lua table) for children nodes
- Each node tracks whether it's the end of a word
- Supports efficient prefix-based searches

### **Time Complexity:**
- **Insert**: O(m) where m is the length of the word
- **Search**: O(m) where m is the length of the word
- **Prefix Search**: O(m) where m is the length of the prefix

### **Space Complexity:**
- O(ALPHABET_SIZE × N × M) where N is the number of words and M is the average length

This implementation demonstrates how tries are useful for autocomplete features, spell checkers, and prefix-based searching applications.

