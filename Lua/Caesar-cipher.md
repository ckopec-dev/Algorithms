# Caesar Cipher in Lua

Here's an implementation of the Caesar cipher algorithm in Lua:

```lua
-- Caesar Cipher function
function caesar_cipher(text, shift)
    local result = ""
    
    for i = 1, #text do
        local char = text:sub(i, i)
        
        if char:match("%u") then  -- Uppercase letter
            local ascii = string.byte(char)
            local shifted = ((ascii - 65 + shift) % 26) + 65
            result = result .. string.char(shifted)
        elseif char:match("%l") then  -- Lowercase letter
            local ascii = string.byte(char)
            local shifted = ((ascii - 97 + shift) % 26) + 97
            result = result .. string.char(shifted)
        else  -- Non-alphabetic character
            result = result .. char
        end
    end
    
    return result
end

-- Example usage
local original_text = "Hello World!"
local shift_value = 3

local encrypted = caesar_cipher(original_text, shift_value)
local decrypted = caesar_cipher(encrypted, -shift_value)

print("Original text: " .. original_text)
print("Encrypted text: " .. encrypted)
print("Decrypted text: " .. decrypted)
```

## Output:
```
Original text: Hello World!
Encrypted text: Khoor Zruog!
Decrypted text: Hello World!
```

## How it works:

1. **Encryption**: Each letter is shifted by the specified number of positions in the alphabet
2. **Decryption**: The same function with a negative shift value
3. **Preservation**: Non-alphabetic characters remain unchanged
4. **Wrapping**: Uses modulo arithmetic to wrap around the alphabet (Z shifts to A)

## Key Features:

- Handles both uppercase and lowercase letters
- Preserves spaces and punctuation
- Uses modular arithmetic for proper wrapping
- Simple and efficient implementation

