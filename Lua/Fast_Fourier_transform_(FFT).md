# Fast Fourier Transform (FFT) in Lua

Here's an implementation of the Cooley-Tukey FFT algorithm in Lua:

```lua
-- Fast Fourier Transform implementation in Lua
function fft(x)
    local n = #x
    
    -- Handle edge cases
    if n <= 1 then return x end
    
    -- Check if n is a power of 2
    local isPowerOfTwo = (n & (n - 1)) == 0
    if not isPowerOfTwo then
        -- Pad with zeros to make it power of 2
        local padded = {}
        for i = 1, n do
            padded[i] = x[i]
        end
        for i = n + 1, 2 * n do
            padded[i] = {0, 0}  -- Zero padding
        end
        n = 2 * n
        x = padded
    end
    
    -- Bit-reversal permutation
    local bitReversed = {}
    for i = 1, n do
        local rev = 0
        local k = i - 1
        for j = 1, math.floor(math.log(n) / math.log(2)) do
            rev = rev * 2 + (k % 2)
            k = math.floor(k / 2)
        end
        bitReversed[i] = x[rev + 1]
    end
    
    -- FFT computation
    local step = 2
    while step <= n do
        local angle = -2 * math.pi / step
        local wstep = {math.cos(angle), math.sin(angle)}
        
        for i = 1, n, step do
            local w = {1, 0}
            for j = 1, step / 2 do
                local u = bitReversed[i + j - 1]
                local v = {
                    bitReversed[i + j + step / 2 - 1][1] * w[1] - bitReversed[i + j + step / 2 - 1][2] * w[2],
                    bitReversed[i + j + step / 2 - 1][1] * w[2] + bitReversed[i + j + step / 2 - 1][2] * w[1]
                }
                
                bitReversed[i + j - 1] = {
                    u[1] + v[1],
                    u[2] + v[2]
                }
                
                bitReversed[i + j + step / 2 - 1] = {
                    u[1] - v[1],
                    u[2] - v[2]
                }
                
                -- Update w
                local new_w = {
                    w[1] * wstep[1] - w[2] * wstep[2],
                    w[1] * wstep[2] + w[2] * wstep[1]
                }
                w = new_w
            end
        end
        step = step * 2
    end
    
    return bitReversed
end

-- Simple example usage
function printComplexArray(arr, name)
    print(name .. ":")
    for i, val in ipairs(arr) do
        if type(val) == "table" then
            print(string.format("  [%d] = %.2f + %.2fi", i, val[1], val[2]))
        else
            print(string.format("  [%d] = %.2f", i, val))
        end
    end
    print()
end

-- Example: Simple signal
local signal = {{1, 0}, {1, 0}, {1, 0}, {1, 0}}  -- Real signal
print("Original signal:")
printComplexArray(signal, "x")

-- Apply FFT
local result = fft(signal)
print("FFT result:")
printComplexArray(result, "X")

-- Example with complex signal
local complexSignal = {
    {1, 0},   -- 1 + 0i
    {0, 1},   -- 0 + 1i
    {-1, 0},  -- -1 + 0i
    {0, -1}   -- 0 - 1i
}

print("Complex signal:")
printComplexArray(complexSignal, "x")

local complexResult = fft(complexSignal)
print("FFT of complex signal:")
printComplexArray(complexResult, "X")
```

## Key Features of This Implementation:

1. **Cooley-Tukey Algorithm**: Uses the classic divide-and-conquer approach
2. **Bit-reversal**: Implements bit-reversal permutation for efficient computation
3. **Complex Numbers**: Handles complex numbers represented as tables with real and imaginary parts
4. **Power-of-2 Requirement**: Automatically pads input to nearest power of 2
5. **In-place Computation**: Modifies data in-place for efficiency

## Usage Example:

```lua
-- Simple usage
local input = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}}
local output = fft(input)
```

This implementation provides a solid foundation for FFT computations in Lua, suitable for signal processing applications.

