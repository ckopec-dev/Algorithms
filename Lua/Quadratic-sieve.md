# Quadratic Sieve Algorithm in Lua

```lua
-- Quadratic Sieve Algorithm Implementation in Lua

function gcd(a, b)
    while b ~= 0 do
        a, b = b, a % b
    end
    return a
end

function is_prime(n)
    if n < 2 then return false end
    if n == 2 then return true end
    if n % 2 == 0 then return false end
    
    for i = 3, math.sqrt(n), 2 do
        if n % i == 0 then return false end
    end
    return true
end

function factorize(n)
    -- Simple trial division for small numbers
    local factors = {}
    local d = 2
    
    while d * d <= n do
        while n % d == 0 do
            table.insert(factors, d)
            n = n / d
        end
        d = d + 1
    end
    
    if n > 1 then
        table.insert(factors, n)
    end
    
    return factors
end

function quadratic_sieve(n)
    print("Factoring " .. n .. " using Quadratic Sieve")
    
    -- Step 1: Find smooth numbers
    local smooth_numbers = {}
    local primes = {}
    
    -- Generate small primes
    for i = 2, 100 do
        if is_prime(i) then
            table.insert(primes, i)
        end
    end
    
    -- Step 2: Find numbers that are smooth (factorable by small primes)
    local bound = math.floor(math.sqrt(n)) + 100
    local sieve_range = 2 * bound
    
    print("Searching for smooth numbers...")
    
    -- Simple approach: check numbers around sqrt(n)
    for i = -bound, bound do
        local x = math.sqrt(n) + i
        if x > 0 then
            local x_squared = x * x
            local diff = x_squared - n
            
            -- Check if diff is smooth
            local factors = factorize(math.abs(diff))
            local is_smooth = true
            
            -- Check if all factors are small primes
            for _, factor in ipairs(factors) do
                if factor > 100 then  -- Use small bound for simplicity
                    is_smooth = false
                    break
                end
            end
            
            if is_smooth then
                table.insert(smooth_numbers, {x = x, diff = diff})
                print("Found smooth number: x=" .. x .. ", x²-n=" .. diff)
            end
        end
    end
    
    -- Step 3: Try to find linear dependencies
    print("Found " .. #smooth_numbers .. " smooth numbers")
    
    if #smooth_numbers >= 2 then
        -- Simple approach: try to combine first two
        local x1 = smooth_numbers[1].x
        local x2 = smooth_numbers[2].x
        local d1 = smooth_numbers[1].diff
        local d2 = smooth_numbers[2].diff
        
        print("Attempting to combine smooth numbers...")
        
        -- Try to find a combination that gives a perfect square
        local test = d1 * d2
        local sqrt_test = math.sqrt(math.abs(test))
        
        if math.abs(sqrt_test - math.floor(sqrt_test)) < 0.0001 then
            print("Found factorization!")
            local a = math.floor(sqrt_test)
            local b = math.floor((x1 + x2) / 2)
            
            local factor1 = gcd(a - b, n)
            local factor2 = gcd(a + b, n)
            
            print("Factors found: " .. factor1 .. " and " .. factor2)
            
            return factor1, factor2
        end
    end
    
    -- Fallback to simple factorization
    print("Using simple factorization...")
    local factors = factorize(n)
    if #factors >= 2 then
        return factors[1], factors[2]
    end
    
    return nil, nil
end

-- Example usage
print("=== Quadratic Sieve Demo ===")

-- Test with a small composite number
local number = 1343
print("Factoring " .. number)

local factor1, factor2 = quadratic_sieve(number)

if factor1 and factor2 then
    print("Factors found: " .. factor1 .. " × " .. factor2 .. " = " .. (factor1 * factor2))
    print("Verification: " .. (factor1 * factor2 == number))
else
    print("Could not find factors")
end

print("\n--- Another example ---")
local number2 = 1537
print("Factoring " .. number2)

local factor1_2, factor2_2 = quadratic_sieve(number2)

if factor1_2 and factor2_2 then
    print("Factors found: " .. factor1_2 .. " × " .. factor2_2 .. " = " .. (factor1_2 * factor2_2))
    print("Verification: " .. (factor1_2 * factor2_2 == number2))
else
    print("Could not find factors")
end
```

## How This Implementation Works

This is a simplified version of the Quadratic Sieve algorithm that demonstrates the core concepts:

1. **Smooth Number Finding**: Looks for numbers x where x² - n is smooth (factorable by small primes)
2. **Linear Dependency**: Attempts to find combinations of smooth numbers that create perfect squares
3. **Factor Extraction**: Uses the relationship to extract factors of n

## Key Components

- **GCD Function**: Computes greatest common divisor for factor extraction
- **Prime Testing**: Identifies small primes for factorization
- **Smooth Number Detection**: Finds numbers whose difference from n is smooth
- **Factorization**: Simple trial division for verification

## Limitations

This is a simplified implementation that:
- Uses a small bound for smooth numbers
- Doesn't implement the full sieve process
- Has limited optimization
- Is suitable for demonstration but not production use

For actual large-scale factorization, a full implementation would include:
- Proper sieving with large prime handling
- Matrix operations for linear dependency finding
- More sophisticated smoothness testing
- Better optimization strategies

