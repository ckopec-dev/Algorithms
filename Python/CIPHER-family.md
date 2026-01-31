# CIPHER__family Algorithm Example in Python

Here's an example implementation of a simple cipher algorithm using the Caesar cipher (part of the cipher family) in Python:

```python
def caesar_cipher_encrypt(text, shift):
    """
    Encrypts text using Caesar cipher algorithm
    """
    encrypted = ""
    
    for char in text:
        if char.isalpha():
            # Determine if uppercase or lowercase
            ascii_offset = ord('A') if char.isupper() else ord('a')
            # Shift character and wrap around
            shifted = (ord(char) - ascii_offset + shift) % 26
            encrypted += chr(shifted + ascii_offset)
        else:
            encrypted += char
    
    return encrypted

def caesar_cipher_decrypt(encrypted_text, shift):
    """
    Decrypts text using Caesar cipher algorithm
    """
    decrypted = ""
    
    for char in encrypted_text:
        if char.isalpha():
            # Determine if uppercase or lowercase
            ascii_offset = ord('A') if char.isupper() else ord('a')
            # Shift character back and wrap around
            shifted = (ord(char) - ascii_offset - shift) % 26
            decrypted += chr(shifted + ascii_offset)
        else:
            decrypted += char
    
    return decrypted

# Example usage
if __name__ == "__main__":
    # Original message
    message = "Hello World!"
    shift_value = 3
    
    print(f"Original message: {message}")
    print(f"Shift value: {shift_value}")
    
    # Encrypt
    encrypted = caesar_cipher_encrypt(message, shift_value)
    print(f"Encrypted: {encrypted}")
    
    # Decrypt
    decrypted = caesar_cipher_decrypt(encrypted, shift_value)
    print(f"Decrypted: {decrypted}")
```

**Output:**
```
Original message: Hello World!
Shift value: 3
Encrypted: Khoor Zruog!
Decrypted: Hello World!
```

## Key Features of This Cipher Family Algorithm:

- **Symmetric encryption**: Same key for encryption and decryption
- **Substitution cipher**: Each character is replaced by another character
- **Shift-based**: Characters shifted by a fixed number of positions
- **Modular arithmetic**: Uses modulo operation for wrapping around alphabet
- **Case preservation**: Maintains original case of letters
- **Non-alphabetic preservation**: Spaces and punctuation remain unchanged

This example demonstrates the fundamental principles of cipher algorithms, which form the basis for more complex encryption methods in the cipher family.

