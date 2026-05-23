# Booth's Multiplication Algorithm in Lua

Here's an implementation of Booth's multiplication algorithm in Lua:

```lua
function booths_multiplication(a, b)
    -- Booth's multiplication algorithm implementation
    -- a and b are integers (can be negative)
    
    -- Handle special cases
    if a == 0 or b == 0 then
        return 0
    end
    
    -- Determine if result should be negative
    local negative = (a < 0) ~= (b < 0)
    
    -- Work with absolute values
    a = math.abs(a)
    b = math.abs(b)
    
    local result = 0
    local bit_position = 0
    
    -- Booth's algorithm uses the principle that we can multiply by 
    -- adding/subtracting shifted versions of the multiplicand
    while b > 0 do
        local bit = b % 2
        b = math.floor(b / 2)
        
        if bit == 1 then
            result = result + (a << bit_position)
        end
        
        bit_position = bit_position + 1
    end
    
    -- Apply sign
    if negative then
        result = -result
    end
    
    return result
end

-- Alternative implementation using Booth's recoding approach
function booths_multiplication_advanced(a, b)
    -- More accurate Booth's algorithm implementation
    local multiplicand = a
    local multiplier = b
    
    -- Handle sign
    local sign = 1
    if multiplicand < 0 then
        multiplicand = -multiplicand
        sign = -sign
    end
    if multiplier < 0 then
        multiplier = -multiplier
        sign = -sign
    end
    
    local result = 0
    local multiplier_copy = multiplier
    local shift = 0
    
    -- Booth's algorithm: process multiplier bit by bit
    while multiplier_copy > 0 do
        local current_bit = multiplier_copy % 2
        local next_bit = (multiplier_copy % 4) // 2
        
        -- Booth's recoding: look at current and next bit
        local recoded = 0
        if current_bit == 0 and next_bit == 1 then
            recoded = 1  -- Add multiplicand
        elseif current_bit == 1 and next_bit == 0 then
            recoded = -1  -- Subtract multiplicand
        end
        
        if recoded ~= 0 then
            if recoded == 1 then
                result = result + (multiplicand << shift)
            else
                result = result - (multiplicand << shift)
            end
        end
        
        multiplier_copy = math.floor(multiplier_copy / 2)
        shift = shift + 1
    end
    
    return result * sign
end

-- Simple Booth's algorithm for demonstration
function simple_booth_multiply(a, b)
    -- Simple version that demonstrates the concept
    print("Multiplying " .. a .. " and " .. b)
    
    local abs_a = math.abs(a)
    local abs_b = math.abs(b)
    local result = 0
    
    -- Convert b to binary and process each bit
    local temp_b = abs_b
    local position = 0
    
    while temp_b > 0 do
        if temp_b % 2 == 1 then
            print("Bit 1 at position " .. position .. ": Add " .. abs_a .. " shifted left " .. position .. " positions")
            result = result + (abs_a * (2 ^ position))
        else
            print("Bit 0 at position " .. position .. ": Skip")
        end
        temp_b = math.floor(temp_b / 2)
        position = position + 1
    end
    
    -- Handle sign
    if (a < 0) ~= (b < 0) then
        result = -result
        print("Result is negative")
    else
        print("Result is positive")
    end
    
    return result
end

-- Example usage and test cases
print("=== Booth's Multiplication Examples ===")

-- Test case 1: Positive numbers
print("\nExample 1: 5 × 3")
local result1 = simple_booth_multiply(5, 3)
print("Result: " .. result1)
print("Verification: " .. (5 * 3))

-- Test case 2: Negative and positive
print("\nExample 2: -4 × 6")
local result2 = simple_booth_multiply(-4, 6)
print("Result: " .. result2)
print("Verification: " .. (-4 * 6))

-- Test case 3: Both negative
print("\nExample 3: -3 × -7")
local result3 = simple_booth_multiply(-3, -7)
print("Result: " .. result3)
print("Verification: " .. (-3 * -7))

-- Test case 4: Large numbers
print("\nExample 4: 12 × 8")
local result4 = simple_booth_multiply(12, 8)
print("Result: " .. result4)
print("Verification: " .. (12 * 8))

-- Demonstrate bit operations
print("\n=== Bit Operations Demo ===")
print("Binary of 5: " .. string.format("%b", 5))
print("Binary of 3: " .. string.format("%b", 3))
print("5 << 1 = " .. (5 << 1))
print("5 << 2 = " .. (5 << 2))
```

## How Booth's Algorithm Works

Booth's multiplication algorithm is a method for multiplying binary numbers that reduces the number of operations needed. The key insight is that instead of just adding or subtracting the multiplicand based on each bit, we can look at pairs of bits to determine whether to add, subtract, or skip the multiplicand.

### Key Steps:
1. **Initialize**: Set up the multiplier and multiplicand
2. **Process bits**: Look at each bit and the next bit to determine operation
3. **Shift**: Shift the result appropriately
4. **Handle signs**: Apply correct sign to final result

### Time Complexity:
- **Time**: O(n) where n is the number of bits
- **Space**: O(1)

### Advantages:
- Reduces the number of partial products
- More efficient than traditional multiplication for large numbers
- Particularly useful in hardware implementations

This implementation demonstrates the basic principles of Booth's algorithm while showing how it can be applied to perform multiplication in Lua.

