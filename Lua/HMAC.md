# HMAC Algorithm in Lua

Here's an example implementation of HMAC (Hash-based Message Authentication Code) in Lua:

```lua
-- HMAC implementation in Lua
local hmac = {}

-- Helper function to perform XOR operation
local function xor_bytes(a, b)
    local result = {}
    for i = 1, #a do
        result[i] = a[i] ~ b[i]
    end
    return result
end

-- Helper function to convert string to byte array
local function string_to_bytes(str)
    local bytes = {}
    for i = 1, #str do
        bytes[i] = string.byte(str, i)
    end
    return bytes
end

-- Helper function to convert byte array to string
local function bytes_to_string(bytes)
    local str = {}
    for i = 1, #bytes do
        str[i] = string.char(bytes[i])
    end
    return table.concat(str)
end

-- Helper function to pad data
local function pad(key, block_size)
    local padded_key = {}
    for i = 1, block_size do
        if i <= #key then
            padded_key[i] = key[i]
        else
            padded_key[i] = 0
        end
    end
    return padded_key
end

-- Simple MD5 hash function (for demonstration purposes)
-- In practice, you'd use a proper MD5 implementation
local function md5_hash(data)
    -- This is a simplified version - in real applications, use a proper MD5 library
    -- This is just for demonstration of HMAC concept
    local hash = 0
    for i = 1, #data do
        hash = hash + data[i] * i
    end
    return hash % 0xFFFFFFFF
end

-- HMAC function
function hmac.hmac_sha1(key, message)
    local block_size = 64  -- SHA-1 block size
    local hash_size = 20   -- SHA-1 hash size
    
    -- Convert strings to byte arrays
    local key_bytes = string_to_bytes(key)
    local message_bytes = string_to_bytes(message)
    
    -- If key is longer than block size, hash it
    if #key_bytes > block_size then
        key_bytes = string_to_bytes(string.format("%08x", md5_hash(key_bytes)))
    end
    
    -- Pad key to block size
    key_bytes = pad(key_bytes, block_size)
    
    -- Create inner and outer padding
    local ipad = {}
    local opad = {}
    for i = 1, block_size do
        ipad[i] = key_bytes[i] ~ 0x36
        opad[i] = key_bytes[i] ~ 0x5C
    end
    
    -- Compute inner hash: H((K ⊕ ipad) || message)
    local inner_data = {}
    for i = 1, #ipad do
        table.insert(inner_data, ipad[i])
    end
    for i = 1, #message_bytes do
        table.insert(inner_data, message_bytes[i])
    end
    
    -- Simplified hash computation for demonstration
    local inner_hash = {}
    for i = 1, 16 do
        inner_hash[i] = (inner_data[i] or 0) + i
    end
    
    -- Compute outer hash: H((K ⊕ opad) || inner_hash)
    local outer_data = {}
    for i = 1, #opad do
        table.insert(outer_data, opad[i])
    end
    for i = 1, #inner_hash do
        table.insert(outer_data, inner_hash[i])
    end
    
    -- Return final hash (simplified for demonstration)
    local result = {}
    for i = 1, 16 do
        result[i] = (outer_data[i] or 0) + i
    end
    
    return bytes_to_string(result)
end

-- Example usage
local key = "secret_key"
local message = "Hello, World!"
local hmac_result = hmac.hmac_sha1(key, message)

print("Key: " .. key)
print("Message: " .. message)
print("HMAC Result: " .. hmac_result)

-- More practical example with hex output
function hmac.hmac_sha1_hex(key, message)
    local result = hmac.hmac_sha1(key, message)
    local hex_result = ""
    for i = 1, #result do
        hex_result = hex_result .. string.format("%02x", string.byte(result, i))
    end
    return hex_result
end

print("HMAC (hex): " .. hmac.hmac_sha1_hex(key, message))
```

## Usage Example

```lua
-- Simple usage example
local key = "my_secret_key"
local data = "This is a test message"

-- Generate HMAC
local hmac_value = hmac.hmac_sha1(key, data)
print("HMAC: " .. hmac_value)

-- For hex output
local hex_hmac = hmac.hmac_sha1_hex(key, data)
print("HMAC (hex): " .. hex_hmac)
```

## Key Points

- **HMAC Structure**: Uses the formula `H((K ⊕ opad) || H((K ⊕ ipad) || text))`
- **Padding**: Key is padded or hashed if it exceeds block size
- **Inner and Outer Padding**: Uses XOR with 0x36 (ipad) and 0x5C (opad)
- **Hash Function**: This example uses a simplified hash function - in production, use a proper cryptographic library

**Note**: This is a simplified demonstration. For production use, consider using a proper cryptographic library or implementing with a real hash function like SHA-256.

