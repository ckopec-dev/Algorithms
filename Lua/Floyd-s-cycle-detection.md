# Floyd's Cycle Detection Algorithm in Lua

Floyd's Cycle Detection Algorithm (also known as the "Tortoise and Hare" algorithm) is used to detect cycles in linked lists. Here's an implementation in Lua:

```lua
-- Node structure for a linked list
local Node = {}
Node.__index = Node

function Node:new(value)
    local node = {
        value = value,
        next = nil
    }
    return setmetatable(node, Node)
end

-- Floyd's Cycle Detection Algorithm
function hasCycle(head)
    -- Handle empty list or single node
    if not head or not head.next then
        return false
    end
    
    -- Initialize two pointers
    local slow = head      -- Tortoise - moves one step
    local fast = head      -- Hare - moves two steps
    
    -- Traverse the list
    while fast and fast.next do
        slow = slow.next           -- Move slow pointer one step
        fast = fast.next.next      -- Move fast pointer two steps
        
        -- If they meet, there's a cycle
        if slow == fast then
            return true
        end
    end
    
    -- If fast reaches the end, no cycle
    return false
end

-- Helper function to create a cycle for testing
function createCycle(head, pos)
    -- Find the node at position 'pos' and the last node
    local current = head
    local cycleNode = nil
    local length = 0
    
    -- Count length and find cycle node
    while current do
        if length == pos then
            cycleNode = current
        end
        length = length + 1
        current = current.next
    end
    
    -- Connect last node to cycleNode
    if current then
        current.next = cycleNode
    end
end

-- Example usage
print("=== Floyd's Cycle Detection Algorithm ===")

-- Create a simple linked list: 1 -> 2 -> 3 -> 4 -> 5
local head = Node:new(1)
head.next = Node:new(2)
head.next.next = Node:new(3)
head.next.next.next = Node:new(4)
head.next.next.next.next = Node:new(5)

print("Testing acyclic list:")
print("Has cycle:", hasCycle(head))  -- Should print false

-- Create a cycle: 1 -> 2 -> 3 -> 4 -> 5 -> 2 (cycle back to node 2)
-- This creates a cycle from node 5 back to node 2
head.next.next.next.next.next = head.next

print("Testing cyclic list:")
print("Has cycle:", hasCycle(head))  -- Should print true

-- Another example with a different cycle
local head2 = Node:new(1)
head2.next = Node:new(2)
head2.next.next = Node:new(3)
head2.next.next.next = Node:new(4)
head2.next.next.next.next = Node:new(5)
head2.next.next.next.next.next = head2.next.next  -- Cycle from 5 to 3

print("Testing another cyclic list:")
print("Has cycle:", hasCycle(head2))  -- Should print true
```

## How it works:

1. **Two Pointers**: Uses two pointers moving at different speeds
2. **Slow Pointer**: Moves one step at a time (tortoise)
3. **Fast Pointer**: Moves two steps at a time (hare)
4. **Cycle Detection**: If there's a cycle, the fast pointer will eventually "lap" the slow pointer
5. **No Cycle**: If the fast pointer reaches the end, there's no cycle

## Time and Space Complexity:
- **Time Complexity**: O(n)
- **Space Complexity**: O(1)

## Key Points:
- The algorithm is elegant and efficient
- It works for any linked list structure
- The fast pointer will always catch up to the slow pointer if there's a cycle
- This is a fundamental algorithm used in many applications including detecting cycles in graphs

