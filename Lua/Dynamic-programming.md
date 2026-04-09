# Dynamic Programming Example in Lua: Fibonacci Sequence

Here's a classic dynamic programming example using the Fibonacci sequence with memoization:

```lua
-- Dynamic Programming: Fibonacci with Memoization
function fibonacci_dp(n, memo)
    -- Initialize memo table if not provided
    if memo == nil then
        memo = {}
    end
    
    -- Base cases
    if n <= 0 then
        return 0
    elseif n == 1 then
        return 1
    end
    
    -- Check if already computed
    if memo[n] ~= nil then
        return memo[n]
    end
    
    -- Compute and store result
    memo[n] = fibonacci_dp(n - 1, memo) + fibonacci_dp(n - 2, memo)
    return memo[n]
end

-- Alternative: Bottom-up approach
function fibonacci_bottom_up(n)
    if n <= 0 then return 0 end
    if n == 1 then return 1 end
    
    local dp = {}
    dp[0] = 0
    dp[1] = 1
    
    for i = 2, n do
        dp[i] = dp[i-1] + dp[i-2]
    end
    
    return dp[n]
end

-- Example usage
print("Fibonacci(10) using memoization:", fibonacci_dp(10))
print("Fibonacci(10) using bottom-up:", fibonacci_bottom_up(10))

-- Output:
-- Fibonacci(10) using memoization: 55
-- Fibonacci(10) using bottom-up: 55
```

## Key Dynamic Programming Concepts Demonstrated:

1. **Overlapping Subproblems**: The same Fibonacci numbers are computed multiple times
2. **Optimal Substructure**: Fibonacci(n) = Fibonacci(n-1) + Fibonacci(n-2)
3. **Memoization**: Storing previously computed results to avoid redundant calculations
4. **Bottom-up approach**: Building solutions from smaller subproblems

## Time Complexity:
- **Memoization**: O(n) time, O(n) space
- **Bottom-up**: O(n) time, O(n) space

## Space Optimization:
```lua
-- Space-optimized version (O(1) space)
function fibonacci_optimized(n)
    if n <= 0 then return 0 end
    if n == 1 then return 1 end
    
    local prev2 = 0
    local prev1 = 1
    local current
    
    for i = 2, n do
        current = prev1 + prev2
        prev2 = prev1
        prev1 = current
    end
    
    return current
end
```

This example shows how dynamic programming can dramatically improve efficiency by avoiding redundant computations through memoization or bottom-up building of solutions.

