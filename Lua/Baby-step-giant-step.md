# Baby-step Giant-step Algorithm in Lua

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: finding `x` such that `g^x ≡ h (mod p)`.

```lua
function baby_step_giant_step(g, h, p)
    -- Calculate the square root of p (rounded up)
    local m = math.ceil(math.sqrt(p - 1))
    
    -- Baby steps: compute g^j mod p for j = 0, 1, 2, ..., m-1
    local baby_steps = {}
    local power = 1
    
    for j = 0, m - 1 do
        baby_steps[power] = j
        power = (power * g) % p
    end
    
    -- Calculate g^(-m) mod p
    local g_inv_m = mod_inverse(power, p)
    
    -- Giant steps: compute h * (g^(-m))^i mod p for i = 0, 1, 2, ..., m-1
    local current = h
    for i = 0, m - 1 do
        if baby_steps[current] then
            -- Found a match!
            local x = i * m + baby_steps[current]
            return x
        end
        current = (current * g_inv_m) % p
    end
    
    return nil  -- No solution found
end

-- Helper function to compute modular inverse using extended Euclidean algorithm
function mod_inverse(a, m)
    local function extended_gcd(a, b)
        if b == 0 then
            return a, 1, 0
        else
            local gcd, x1, y1 = extended_gcd(b, a % b)
            return gcd, y1, x1 - math.floor(a / b) * y1
        end
    end
    
    local gcd, x, _ = extended_gcd(a, m)
    if gcd ~= 1 then
        error("Modular inverse does not exist")
    else
        return ((x % m) + m) % m
    end
end

-- Example usage
print("Solving g^x ≡ h (mod p)")
print("Example: 3^x ≡ 13 (mod 17)")

local g = 3
local h = 13
local p = 17

local result = baby_step_giant_step(g, h, p)

if result then
    print("Solution found: x = " .. result)
    
    -- Verify the solution
    local verification = (math.pow(g, result) % p)
    print("Verification: " .. g .. "^" .. result .. " ≡ " .. verification .. " (mod " .. p .. ")")
else
    print("No solution found")
end

-- Another example
print("\nExample: 2^x ≡ 11 (mod 13)")
local g2 = 2
local h2 = 11
local p2 = 13

local result2 = baby_step_giant_step(g2, h2, p2)

if result2 then
    print("Solution found: x = " .. result2)
    
    -- Verify the solution
    local verification2 = (math.pow(g2, result2) % p2)
    print("Verification: " .. g2 .. "^" .. result2 .. " ≡ " .. verification2 .. " (mod " .. p2 .. ")")
else
    print("No solution found")
end
```

## Output:
```
Solving g^x ≡ h (mod p)
Example: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 ≡ 13 (mod 17)

Example: 2^x ≡ 11 (mod 13)
Solution found: x = 7
Verification: 2^7 ≡ 11 (mod 13)
```

## How it works:

1. **Baby Steps**: Compute and store `g^j mod p` for `j = 0, 1, ..., √p`
2. **Giant Steps**: Compute `h * (g^(-m))^i mod p` for `i = 0, 1, ..., √p`
3. **Match**: When a value from giant steps matches a value from baby steps, we have found the solution
4. **Solution**: `x = i * m + j` where `j` is the index from baby steps

The algorithm has time complexity O(√p) and space complexity O(√p), making it much more efficient than brute force for large values of p.

