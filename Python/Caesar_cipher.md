```python
def caesar_cipher(text, shift):
    result = ""
    
    for i in range(len(text)):
        char = text[i]
        
        # Encrypt uppercase characters
        if char.isupper():
            result += chr((ord(char) + shift - 65) % 26 + 65)
        
        # Encrypt lowercase characters
        elif char.islower():
            result += chr((ord(char) + shift - 97) % 26 + 97)
        
        # Leave non-alphabetic characters unchanged
        else:
            result += char
    
    return result

# Example usage
plaintext = "Hello World!"
shift_value = 3

encrypted = caesar_cipher(plaintext, shift_value)
print(f"Original text: {plaintext}")
print(f"Shift value: {shift_value}")
print(f"Encrypted text: {encrypted}")

# To decrypt, use negative shift
decrypted = caesar_cipher(encrypted, -shift_value)
print(f"Decrypted text: {decrypted}")
```

Output:
```
Original text: Hello World!
Shift value: 3
Encrypted text: Khoor Zruog!
Decrypted text: Hello World!
```

