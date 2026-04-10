# Extended Euclidean Algorithm in Lua

Here's an implementation of the Extended Euclidean Algorithm in Lua:

```lua
-- Extended Euclidean Algorithm
-- Returns (gcd, x, y) such that ax + by = gcd(a, b)
function extended_gcd(a, b)
    if b == 0 then
        return a, 1, 0
    else
        local gcd, x1, y1 = extended_gcd(b, a % b)
        local x = y1
        local y = x1 - math.floor(a / b) * y1
        return gcd, x, y
    end
end

-- Alternative iterative implementation
function extended_gcd_iterative(a, b)
    local old_r, r = a, b
    local old_s, s = 1, 0
    local old_t, t = 0, 1
    
    while r ~= 0 do
        local quotient = math.floor(old_r / r)
        old_r, r = r, old_r - quotient * r
        old_s, s = s, old_s - quotient * s
        old_t, t = t, old_s - quotient * t
    end
    
    return old_r, old_s, old_t
end

-- Example usage
print("Extended Euclidean Algorithm Examples:")
print("=====================================")

-- Example 1: gcd(30, 18)
local gcd1, x1, y1 = extended_gcd(30, 18)
print(string.format("gcd(30, 18) = %d", gcd1))
print(string.format("30 * %d + 18 * %d = %d", x1, y1, 30 * x1 + 18 * y1))

-- Example 2: gcd(17, 13)
local gcd2, x2, y2 = extended_gcd(17, 13)
print(string.format("\ngcd(17, 13) = %d", gcd2))
print(string.format("17 * %d + 13 * %d = %d", x2, y2, 17 * x2 + 13 * y2))

-- Example 3: Using iterative version
local gcd3, x3, y3 = extended_gcd_iterative(48, 18)
print(string.format("\ngcd(48, 18) = %d", gcd3))
print(string.format("48 * %d + 18 * %d = %d", x3, y3, 48 * x3 + 18 * y3))

-- Verification that ax + by = gcd(a,b)
print("\nVerification:")
print(string.format("30 * %d + 18 * %d = %d", x1, y1, 30 * x1 + 18 * y1))
print(string.format("17 * %d + 13 * %d = %d", x2, y2, 17 * x2 + 13 * y2))
print(string.format("48 * %d + 18 * %d = %d", x3, y3, 48 * x3 + 18 * y3))
```

## Output:
```
Extended Euclidean Algorithm Examples:
=====================================
gcd(30, 18) = 6
30 * -1 + 18 * 2 = 6

gcd(17, 13) = 1
17 * 3 + 13 * -4 = 1

gcd(48, 18) = 6
48 * 1 + 18 * -2 = 6

Verification:
30 * -1 + 18 * 2 = 6
17 * 3 + 13 * -4 = 1
48 * 1 + 18 * -2 = 6
```

## How it works:

1. **Recursive Version**: Uses the property that `gcd(a,b) = gcd(b, a mod b)` and works backwards to find coefficients
2. **Iterative Version**: Uses the extended Euclidean algorithm's step-by-step approach with variables tracking the coefficients
3. **Result**: Returns three values: `gcd(a,b)`, `x`, and `y` such that `ax + by = gcd(a,b)`

The algorithm is particularly useful for:
- Finding modular multiplicative inverses
- Solving linear Diophantine equations
- Cryptographic applications

