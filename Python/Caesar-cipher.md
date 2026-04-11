# Caesar Cipher Algorithm in Python

## Implementation

```python
def caesar_cipher(text, shift):
    """
    Encrypts or decrypts text using Caesar cipher algorithm
    
    Args:
        text (str): The text to encrypt/decrypt
        shift (int): The number of positions to shift
    
    Returns:
        str: The encrypted/decrypted text
    """
    result = ""
    
    for char in text:
        # Check if character is an uppercase letter
        if char.isupper():
            # Convert to number (A=0, B=1, ..., Z=25)
            char_index = ord(char) - ord('A')
            # Apply shift and wrap around using modulo
            shifted_index = (char_index + shift) % 26
            # Convert back to character
            shifted_char = chr(shifted_index + ord('A'))
            result += shifted_char
            
        # Check if character is a lowercase letter
        elif char.islower():
            # Convert to number (a=0, b=1, ..., z=25)
            char_index = ord(char) - ord('a')
            # Apply shift and wrap around using modulo
            shifted_index = (char_index + shift) % 26
            # Convert back to character
            shifted_char = chr(shifted_index + ord('a'))
            result += shifted_char
            
        else:
            # If not a letter, keep it unchanged
            result += char
    
    return result

# Example usage
if __name__ == "__main__":
    # Original message
    message = "Hello, World!"
    shift_value = 3
    
    # Encrypt the message
    encrypted = caesar_cipher(message, shift_value)
    print(f"Original: {message}")
    print(f"Encrypted: {encrypted}")
    
    # Decrypt the message (shift by negative value)
    decrypted = caesar_cipher(encrypted, -shift_value)
    print(f"Decrypted: {decrypted}")
```

## Output
```
Original: Hello, World!
Encrypted: Khoor, Zruog!
Decrypted: Hello, World!
```

## How it works:

1. **Encryption Process**: Each letter is shifted by the specified number of positions in the alphabet
2. **Wrapping**: When shifting goes past 'Z' or 'z', it wraps around to the beginning
3. **Preservation**: Non-alphabetic characters (spaces, punctuation) remain unchanged
4. **Decryption**: Same process but with negative shift value

## Key Features:
- Handles both uppercase and lowercase letters
- Preserves non-alphabetic characters
- Uses modulo arithmetic for wrapping
- Supports both encryption and decryption

