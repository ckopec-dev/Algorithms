# CIPHER__family Algorithm Example in Lua

Here's an example implementation of a simple cipher algorithm family in Lua:

```lua
-- Cipher Family Implementation in Lua
-- This demonstrates a basic cipher family with different encryption methods

local Cipher = {}

-- Base cipher class
function Cipher:new()
    local obj = {
        key = nil,
        algorithm = nil
    }
    setmetatable(obj, {__index = self})
    return obj
end

-- Caesar Cipher (shift cipher)
function Cipher:caesar_encrypt(text, shift)
    local result = ""
    for i = 1, #text do
        local char = text:sub(i, i)
        if char:match("%u") then  -- Uppercase
            result = result .. string.char((string.byte(char) - 65 + shift) % 26 + 65)
        elseif char:match("%l") then  -- Lowercase
            result = result .. string.char((string.byte(char) - 97 + shift) % 26 + 97)
        else
            result = result .. char
        end
    end
    return result
end

function Cipher:caesar_decrypt(text, shift)
    return self:caesar_encrypt(text, -shift)
end

-- XOR Cipher
function Cipher:xor_encrypt(text, key)
    local result = ""
    local key_len = #key
    for i = 1, #text do
        local char = text:sub(i, i)
        local key_char = key:sub((i % key_len) + 1, (i % key_len) + 1)
        result = result .. string.char(string.byte(char) ~ string.byte(key_char))
    end
    return result
end

function Cipher:xor_decrypt(text, key)
    return self:xor_encrypt(text, key)  -- XOR is symmetric
end

-- Vigenère Cipher
function Cipher:vigenere_encrypt(text, key)
    local result = ""
    local key_len = #key
    local key_index = 1
    
    for i = 1, #text do
        local char = text:sub(i, i)
        if char:match("%u") or char:match("%l") then
            local key_char = key:sub(key_index, key_index)
            local shift = string.byte(string.upper(key_char)) - 65
            if char:match("%u") then
                result = result .. string.char((string.byte(char) - 65 + shift) % 26 + 65)
            else
                result = result .. string.char((string.byte(char) - 97 + shift) % 26 + 97)
            end
            key_index = (key_index % key_len) + 1
        else
            result = result .. char
        end
    end
    return result
end

function Cipher:vigenere_decrypt(text, key)
    local result = ""
    local key_len = #key
    local key_index = 1
    
    for i = 1, #text do
        local char = text:sub(i, i)
        if char:match("%u") or char:match("%l") then
            local key_char = key:sub(key_index, key_index)
            local shift = string.byte(string.upper(key_char)) - 65
            if char:match("%u") then
                result = result .. string.char((string.byte(char) - 65 - shift) % 26 + 65)
            else
                result = result .. string.char((string.byte(char) - 97 - shift) % 26 + 97)
            end
            key_index = (key_index % key_len) + 1
        else
            result = result .. char
        end
    end
    return result
end

-- Example usage:
local cipher = Cipher:new()

-- Caesar Cipher example
local plaintext = "Hello World!"
local caesar_encrypted = cipher:caesar_encrypt(plaintext, 3)
local caesar_decrypted = cipher:caesar_decrypt(caesar_encrypted, 3)

print("Caesar Cipher:")
print("Original: " .. plaintext)
print("Encrypted: " .. caesar_encrypted)
print("Decrypted: " .. caesar_decrypted)
print()

-- XOR Cipher example
local xor_encrypted = cipher:xor_encrypt(plaintext, "key")
local xor_decrypted = cipher:xor_decrypt(xor_encrypted, "key")

print("XOR Cipher:")
print("Original: " .. plaintext)
print("Encrypted: " .. xor_encrypted)
print("Decrypted: " .. xor_decrypted)
print()

-- Vigenère Cipher example
local vigenere_encrypted = cipher:vigenere_encrypt(plaintext, "SECRET")
local vigenere_decrypted = cipher:vigenere_decrypt(vigenere_encrypted, "SECRET")

print("Vigenère Cipher:")
print("Original: " .. plaintext)
print("Encrypted: " .. vigenere_encrypted)
print("Decrypted: " .. vigenere_decrypted)
```

## Output:
```
Caesar Cipher:
Original: Hello World!
Encrypted: Khoor Zruog!
Decrypted: Hello World!

XOR Cipher:
Original: Hello World!
Encrypted: 

