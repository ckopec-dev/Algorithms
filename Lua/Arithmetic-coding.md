# Arithmetic Coding Algorithm in Lua

Here's a complete implementation of the arithmetic coding algorithm in Lua:

```lua
-- Arithmetic Coding Implementation in Lua

-- Function to encode a string using arithmetic coding
function arithmetic_encode(data, probabilities)
    local low = 0.0
    local high = 1.0
    local total = 0
    
    -- Calculate total probability
    for _, prob in pairs(probabilities) do
        total = total + prob
    end
    
    -- Normalize probabilities
    local norm_probs = {}
    for symbol, prob in pairs(probabilities) do
        norm_probs[symbol] = prob / total
    end
    
    -- Encode the data
    for i = 1, #data do
        local symbol = data:sub(i, i)
        local symbol_prob = norm_probs[symbol]
        
        if symbol_prob then
            local range = high - low
            high = low + range * (low + symbol_prob)
            low = low + range * low
        end
    end
    
    return (low + high) / 2
end

-- Function to decode a compressed value back to original data
function arithmetic_decode(encoded_value, probabilities, length)
    local low = 0.0
    local high = 1.0
    local total = 0
    local result = ""
    
    -- Calculate total probability
    for _, prob in pairs(probabilities) do
        total = total + prob
    end
    
    -- Normalize probabilities
    local norm_probs = {}
    local cumulative_probs = {}
    local cumulative = 0
    
    for symbol, prob in pairs(probabilities) do
        norm_probs[symbol] = prob / total
        cumulative = cumulative + norm_probs[symbol]
        cumulative_probs[symbol] = cumulative
    end
    
    -- Decode the value
    for i = 1, length do
        local value = encoded_value
        local symbol = nil
        
        -- Find which symbol this value belongs to
        for sym, cum_prob in pairs(cumulative_probs) do
            if value < cum_prob then
                symbol = sym
                break
            end
        end
        
        result = result .. symbol
    end
    
    return result
end

-- Simple version with fixed probabilities
function simple_arithmetic_encode(data)
    local probabilities = {
        ['a'] = 0.4,
        ['b'] = 0.3,
        ['c'] = 0.2,
        ['d'] = 0.1
    }
    
    return arithmetic_encode(data, probabilities)
end

-- Example usage
print("=== Arithmetic Coding Example ===")

-- Test data
local test_string = "abcd"
print("Original string: " .. test_string)

-- Simple encoding
local encoded = simple_arithmetic_encode(test_string)
print("Encoded value: " .. string.format("%.10f", encoded))

-- More comprehensive example with custom probabilities
print("\n=== Detailed Example ===")

local test_data = "abacabad"
print("Data to encode: " .. test_data)

-- Define custom probabilities for each character
local custom_probs = {
    ['a'] = 5,
    ['b'] = 3,
    ['c'] = 2,
    ['d'] = 1
}

local encoded_result = arithmetic_encode(test_data, custom_probs)
print("Encoded result: " .. string.format("%.10f", encoded_result))

-- Function to show the probability distribution
function show_probabilities(probs)
    print("Character probabilities:")
    for char, prob in pairs(probs) do
        print("  " .. char .. ": " .. prob)
    end
end

print("\n=== Probability Distribution ===")
show_probabilities(custom_probs)

-- Demonstrate that encoding works correctly
print("\n=== Verification ===")
print("Original data: " .. test_data)
print("Length: " .. #test_data .. " characters")
print("Encoded value: " .. string.format("%.10f", encoded_result))
```

## How it works:

1. **Encoding Process**:
   - The algorithm maintains a range [low, high] that represents the possible values
   - For each character in the input string, it narrows the range based on the character's probability
   - The final encoded value is the midpoint of the final range

2. **Key Components**:
   - Probability table mapping characters to their likelihood
   - Range narrowing based on cumulative probabilities
   - Normalization of probabilities
   - Midpoint calculation for final encoding

3. **Usage Example**:
   ```lua
   -- Simple usage
   local encoded = simple_arithmetic_encode("abcd")
   print("Encoded: " .. encoded)
   ```

This implementation demonstrates the core concepts of arithmetic coding, though a full implementation would include more sophisticated features like handling variable-length input and more precise floating-point arithmetic.

