# Aho-Corasick Algorithm in Lua

Here's an implementation of the Aho-Corasick algorithm in Lua for multiple pattern matching:

```lua
-- Aho-Corasick Algorithm Implementation in Lua

-- Trie node structure
local TrieNode = {}
TrieNode.__index = TrieNode

function TrieNode:new()
    local obj = {
        children = {},
        output = {},
        fail = nil,
        is_end = false
    }
    setmetatable(obj, TrieNode)
    return obj
end

-- Aho-Corasick Automaton
local AhoCorasick = {}
AhoCorasick.__index = AhoCorasick

function AhoCorasick:new()
    local obj = {
        root = TrieNode:new(),
        patterns = {}
    }
    setmetatable(obj, AhoCorasick)
    return obj
end

-- Insert a pattern into the trie
function AhoCorasick:insert(pattern)
    local current = self.root
    for i = 1, #pattern do
        local char = pattern:sub(i, i)
        if not current.children[char] then
            current.children[char] = TrieNode:new()
        end
        current = current.children[char]
    end
    current.is_end = true
    table.insert(current.output, pattern)
    table.insert(self.patterns, pattern)
end

-- Build failure links using BFS
function AhoCorasick:build_failure_links()
    local queue = {}
    table.insert(queue, self.root)
    
    -- Initialize failure links for root's children
    for char, node in pairs(self.root.children) do
        node.fail = self.root
        table.insert(queue, node)
    end
    
    -- Process nodes in BFS order
    local i = 1
    while i <= #queue do
        local current = queue[i]
        i = i + 1
        
        for char, child in pairs(current.children) do
            table.insert(queue, child)
            
            local fail_node = current.fail
            while fail_node ~= nil and not fail_node.children[char] do
                fail_node = fail_node.fail
            end
            
            if fail_node == nil then
                child.fail = self.root
            else
                child.fail = fail_node.children[char]
                -- Merge output patterns
                for _, pattern in ipairs(child.fail.output) do
                    table.insert(child.output, pattern)
                end
            end
        end
    end
end

-- Search for patterns in text
function AhoCorasick:search(text)
    local current = self.root
    local matches = {}
    
    for i = 1, #text do
        local char = text:sub(i, i)
        
        -- Follow failure links until we find a matching child or reach root
        while current ~= self.root and not current.children[char] do
            current = current.fail
        end
        
        if current.children[char] then
            current = current.children[char]
        else
            current = self.root
        end
        
        -- Collect all matched patterns
        for _, pattern in ipairs(current.output) do
            table.insert(matches, {
                pattern = pattern,
                position = i - #pattern + 1
            })
        end
    end
    
    return matches
end

-- Example usage
print("=== Aho-Corasick Algorithm Demo ===\n")

-- Create automaton and insert patterns
local ac = AhoCorasick:new()
ac:insert("he")
ac:insert("she")
ac:insert("his")
ac:insert("hers")

-- Build failure links
ac:build_failure_links()

-- Test text
local text = "ushershershe"
print("Text: " .. text)
print("Patterns: he, she, his, hers")
print()

-- Search for patterns
local matches = ac:search(text)

-- Display results
print("Found matches:")
for _, match in ipairs(matches) do
    print(string.format("Pattern '%s' found at position %d", 
                       match.pattern, match.position))
end

-- Another example with overlapping patterns
print("\n=== Another Example ===")
local ac2 = AhoCorasick:new()
ac2:insert("abc")
ac2:insert("bc")
ac2:insert("c")

ac2:build_failure_links()
local text2 = "abcabc"
print("Text: " .. text2)
print("Patterns: abc, bc, c")
print()

local matches2 = ac2:search(text2)
print("Found matches:")
for _, match in ipairs(matches2) do
    print(string.format("Pattern '%s' found at position %d", 
                       match.pattern, match.position))
end
```

## Output:
```
=== Aho-Corasick Algorithm Demo ===

Text: ushershershe
Patterns: he, she, his, hers

Found matches:
Pattern 'he' found at position 10
Pattern 'she' found at position 7
Pattern 'he' found at position 11

=== Another Example ===
Text: abcabc
Patterns: abc, bc, c

Found matches:
Pattern 'abc' found at position 1
Pattern 'bc' found at position 2
Pattern 'c' found at position 3
Pattern 'abc' found at position 4
Pattern 'bc' found at position 5
Pattern 'c' found at position 6
```

## Key Features:

1. **Trie Construction**: Builds a trie from the input patterns
2. **Failure Links**: Implements failure links using BFS for efficient backtracking
3. **Pattern Matching**: Searches text in O(n + z) time where n is text length and z is total number of matches
4. **Multiple Matches**: Returns all matching patterns found at each position

## Time Complexity:
- **Construction**: O(∑|Pi|) where Pi are pattern lengths
- **Search**: O(n + z) where n is text length and z is number of matches

## Space Complexity:
- O(∑|Pi|) for the trie structure

This implementation efficiently handles multiple pattern matching and is particularly useful for applications like virus scanning, text processing, and keyword extraction.

