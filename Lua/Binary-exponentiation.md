# Binary Exponentiation Algorithm in Lua

Binary exponentiation is an efficient algorithm to compute large powers of a number. Here's an implementation in Lua:

```lua
-- Binary Exponentiation Algorithm
function binary_exponentiation(base, exponent)
    -- Handle edge cases
    if exponent == 0 then
        return 1
    end
    
    if exponent == 1 then
        return base
    end
    
    -- Handle negative exponents
    if exponent < 0 then
        return 1 / binary_exponentiation(base, -exponent)
    end
    
    local result = 1
    local current_base = base
    
    -- Binary exponentiation loop
    while exponent > 0 do
        -- If exponent is odd, multiply result by current base
        if exponent % 2 == 1 then
            result = result * current_base
        end
        
        -- Square the base and halve the exponent
        current_base = current_base * current_base
        exponent = math.floor(exponent / 2)
    end
    
    return result
end

-- Example usage
print("Binary Exponentiation Examples:")
print("2^10 =", binary_exponentiation(2, 10))    -- Output: 1024
print("3^5 =", binary_exponentiation(3, 5))      -- Output: 243
print("5^0 =", binary_exponentiation(5, 0))      -- Output: 1
print("2^-3 =", binary_exponentiation(2, -3))    -- Output: 0.125
print("7^4 =", binary_exponentiation(7, 4))      -- Output: 2401
```

## How it works:

1. **Base cases**: Handle when exponent is 0 (return 1) or 1 (return base)
2. **Negative exponents**: Convert to positive and take reciprocal
3. **Main loop**: 
   - If current exponent is odd, multiply result by current base
   - Square the base and halve the exponent
   - Continue until exponent becomes 0

## Time Complexity:
- **Time**: O(log n) where n is the exponent
- **Space**: O(1) for iterative version

This algorithm is much more efficient than the naive approach of multiplying the base by itself 'exponent' times, especially for large exponents.

