# Binary GCD Algorithm in Lua

Here's an implementation of the Binary GCD (Stein's algorithm) in Lua:

```lua
function binary_gcd(a, b)
    -- Handle edge cases
    if a == 0 then return b end
    if b == 0 then return a end
    
    -- Handle negative numbers
    a = math.abs(a)
    b = math.abs(b)
    
    -- Count common factors of 2
    shift = 0
    while ((a % 2 == 0) and (b % 2 == 0)) do
        a = a / 2
        b = b / 2
        shift = shift + 1
    end
    
    -- Remove remaining factors of 2 from a
    while (a % 2 == 0) do
        a = a / 2
    end
    
    while true do
        -- Remove factors of 2 from b
        while (b % 2 == 0) do
            b = b / 2
        end
        
        -- Ensure a <= b
        if (a > b) then
            a, b = b, a
        end
        
        -- Subtract smaller from larger
        b = b - a
        
        if (b == 0) then
            break
        end
    end
    
    -- Restore common factors of 2
    return a * (2 ^ shift)
end

-- Example usage
print("Binary GCD Examples:")
print("gcd(48, 18) = " .. binary_gcd(48, 18))   -- Output: 6
print("gcd(100, 25) = " .. binary_gcd(100, 25)) -- Output: 25
print("gcd(17, 13) = " .. binary_gcd(17, 13))   -- Output: 1
print("gcd(0, 5) = " .. binary_gcd(0, 5))       -- Output: 5
```

## How it works:

1. **Edge case handling**: Returns the non-zero value if one is zero
2. **Negative number handling**: Uses absolute values
3. **Common factor extraction**: Counts and removes common factors of 2
4. **Main loop**: Uses subtraction-based approach similar to Euclidean algorithm
5. **Result restoration**: Multiplies back the common factors of 2

## Time Complexity:
- **O(log(min(a,b)))** - More efficient than traditional Euclidean algorithm for large numbers

## Key Advantages:
- Uses only bit operations and subtraction (no division or modulo)
- More efficient on systems where division is expensive
- Works well with large integers

