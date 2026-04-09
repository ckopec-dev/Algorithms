# Sieve of Eratosthenes in Lua

Here's an implementation of the Sieve of Eratosthenes algorithm in Lua to find all prime numbers up to a given limit:

```lua
function sieve_of_eratosthenes(n)
    -- Create a boolean array and initialize all entries as true
    local is_prime = {}
    for i = 1, n do
        is_prime[i] = true
    end
    
    -- 0 and 1 are not prime numbers
    if n >= 1 then
        is_prime[1] = false
    end
    if n >= 2 then
        is_prime[2] = true
    end
    
    -- Start with the first prime number, 2
    local p = 2
    while p * p <= n do
        -- If is_prime[p] is not changed, then it's a prime
        if is_prime[p] then
            -- Update all multiples of p
            for i = p * p, n, p do
                is_prime[i] = false
            end
        end
        p = p + 1
    end
    
    -- Collect all prime numbers
    local primes = {}
    for i = 2, n do
        if is_prime[i] then
            table.insert(primes, i)
        end
    end
    
    return primes
end

-- Example usage
local limit = 30
local primes = sieve_of_eratosthenes(limit)

print("Prime numbers up to " .. limit .. ":")
for i, prime in ipairs(primes) do
    print(prime)
end

-- Output:
-- Prime numbers up to 30:
-- 2
-- 3
-- 5
-- 7
-- 11
-- 13
-- 17
-- 19
-- 23
-- 29
```

## How it works:

1. **Initialization**: Create a boolean array where each index represents whether that number is prime (true) or not (false)
2. **Mark non-primes**: Starting from 2, mark all multiples of each prime number as not prime
3. **Optimization**: Only check up to √n since larger factors would have been found already
4. **Collect results**: Gather all numbers that remain marked as prime

## Time Complexity: O(n log log n)
## Space Complexity: O(n)

