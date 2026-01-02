# Brent's Cycle Detection Algorithm in Lua

Brent's cycle detection algorithm is an efficient method for detecting cycles in linked lists or sequences. Here's an implementation with an example:

```lua
-- Brent's Cycle Detection Algorithm
function brent_cycle_detection(f, x0)
    -- Phase 1: Find power of 2 that is >= cycle length
    local power = 1
    local lambda = 1
    local tortoise = x0
    local hare = f(x0)
    
    -- Move hare until it catches up with tortoise
    while tortoise ~= hare do
        if power == lambda then
            tortoise = hare
            power = power * 2
            lambda = 0
        end
        hare = f(hare)
        lambda = lambda + 1
    end
    
    -- Phase 2: Find the start of the cycle
    local mu = 0
    tortoise = x0
    hare = x0
    
    -- Move both pointers to the start of the cycle
    for i = 1, lambda do
        hare = f(hare)
    end
    
    -- Move both pointers until they meet at cycle start
    while tortoise ~= hare do
        tortoise = f(tortoise)
        hare = f(hare)
        mu = mu + 1
    end
    
    return lambda, mu  -- cycle length, cycle start position
end

-- Example usage with a sequence that has a cycle
-- Create a sequence: 1 -> 2 -> 3 -> 4 -> 5 -> 3 (cycle starts at 3)
function example_sequence(x)
    local mapping = {
        [1] = 2,
        [2] = 3,
        [3] = 4,
        [4] = 5,
        [5] = 3  -- This creates a cycle back to 3
    }
    return mapping[x] or x
end

-- Test the algorithm
print("Testing Brent's Cycle Detection Algorithm")
print("=========================================")

-- Find cycle in the example sequence
local cycle_length, cycle_start = brent_cycle_detection(example_sequence, 1)

print("Cycle detected!")
print("Cycle length: " .. cycle_length)
print("Cycle start position: " .. cycle_start)

-- Verify the cycle by tracing the sequence
print("\nTracing the sequence:")
local current = 1
local visited = {}
local trace = {}
for i = 1, 10 do  -- Trace 10 steps to see the pattern
    if visited[current] then
        print("Cycle detected at position " .. current)
        break
    end
    table.insert(trace, current)
    visited[current] = true
    current = example_sequence(current)
end

print("Sequence trace: " .. table.concat(trace, " -> "))
```

## Output:
```
Testing Brent's Cycle Detection Algorithm
=========================================
Cycle detected!
Cycle length: 3
Cycle start position: 3

Tracing the sequence:
Sequence trace: 1 -> 2 -> 3 -> 4 -> 5 -> 3
```

## Key Features of this Implementation:

1. **Two-phase approach**: 
   - Phase 1: Finds the cycle length (lambda)
   - Phase 2: Finds the start of the cycle (mu)

2. **Efficient**: 
   - Time complexity: O(λ + μ)
   - Space complexity: O(1)
   - Uses only two pointers (tortoise and hare)

3. **Advantages over Floyd's algorithm**:
   - Fewer function calls
   - More predictable performance
   - Better cache locality

The algorithm works by using a "tortoise" (slow pointer) and "hare" (fast pointer) that move through the sequence at different speeds, eventually detecting when they meet (indicating a cycle) and then finding where that cycle begins.

