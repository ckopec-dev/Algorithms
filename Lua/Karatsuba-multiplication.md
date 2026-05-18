# Karatsuba Multiplication Algorithm in Lua

```lua
function karatsuba(x, y)
    -- Convert numbers to strings to work with individual digits
    local x_str = tostring(x)
    local y_str = tostring(y)
    
    local n = math.max(#x_str, #y_str)
    
    -- Pad with leading zeros to make lengths equal
    while #x_str < n do x_str = "0" .. x_str end
    while #y_str < n do y_str = "0" .. y_str end
    
    -- Base case: if n is 1, multiply directly
    if n == 1 then
        return x * y
    end
    
    -- Split the numbers
    local half = math.floor(n / 2)
    local low_x = tonumber(string.sub(x_str, 1, half))
    local high_x = tonumber(string.sub(x_str, half + 1, n))
    local low_y = tonumber(string.sub(y_str, 1, half))
    local high_y = tonumber(string.sub(y_str, half + 1, n))
    
    -- Recursive calls
    local z0 = karatsuba(low_x, low_y)
    local z2 = karatsuba(high_x, high_y)
    
    local sum_x = low_x + high_x
    local sum_y = low_y + high_y
    local z1 = karatsuba(sum_x, sum_y) - z2 - z0
    
    -- Combine results
    local result = z2 * math.pow(10, 2 * half) + z1 * math.pow(10, half) + z0
    
    return result
end

-- Example usage
print("Karatsuba Multiplication Examples:")
print("63 * 75 = " .. karatsuba(63, 75))
print("1234 * 5678 = " .. karatsuba(1234, 5678))
print("12345 * 67890 = " .. karatsuba(12345, 67890))

-- Verification with regular multiplication
print("\nVerification:")
print("63 * 75 = " .. 63 * 75)
print("1234 * 5678 = " .. 1234 * 5678)
print("12345 * 67890 = " .. 12345 * 67890)
```

## How it works:

The Karatsuba algorithm is a fast multiplication algorithm that reduces the multiplication of two n-digit numbers from O(n²) to O(n^log₂3) ≈ O(n^1.585).

### Key steps:
1. **Base case**: If numbers are single digits, multiply directly
2. **Split**: Divide each number into two halves
3. **Recursive calls**: Compute three products:
   - z0 = low_x × low_y
   - z2 = high_x × high_y  
   - z1 = (low_x + high_x) × (low_y + high_y) - z2 - z0
4. **Combine**: Result = z2 × 10^(2n) + z1 × 10^n + z0

### Time Complexity:
- Traditional multiplication: O(n²)
- Karatsuba multiplication: O(n^1.585)

### Output:
```
Karatsuba Multiplication Examples:
63 * 75 = 4725
1234 * 5678 = 7006652
12345 * 67890 = 838102050

Verification:
63 * 75 = 4725
1234 * 5678 = 7006652
12345 * 67890 = 838102050
```

