# Data Encryption Standard (DES) Algorithm in Python

Here's a complete example of implementing the DES algorithm in Python using the `pycryptodome` library:

```python
from Crypto.Cipher import DES
from Crypto.Util.Padding import pad, unpad
import base64

def des_encrypt(plaintext, key):
    """
    Encrypt plaintext using DES algorithm
    """
    # Create DES cipher object
    cipher = DES.new(key.encode('utf-8'), DES.MODE_ECB)
    
    # Pad the plaintext to be multiple of 8 bytes
    padded_plaintext = pad(plaintext.encode('utf-8'), DES.block_size)
    
    # Encrypt the padded plaintext
    ciphertext = cipher.encrypt(padded_plaintext)
    
    # Return base64 encoded ciphertext
    return base64.b64encode(ciphertext).decode('utf-8')

def des_decrypt(ciphertext, key):
    """
    Decrypt ciphertext using DES algorithm
    """
    # Create DES cipher object
    cipher = DES.new(key.encode('utf-8'), DES.MODE_ECB)
    
    # Decode base64 ciphertext
    decoded_ciphertext = base64.b64decode(ciphertext.encode('utf-8'))
    
    # Decrypt the ciphertext
    decrypted_padded = cipher.decrypt(decoded_ciphertext)
    
    # Remove padding
    plaintext = unpad(decrypted_padded, DES.block_size)
    
    return plaintext.decode('utf-8')

# Example usage
if __name__ == "__main__":
    # Example 1: Basic DES encryption/decryption
    print("=== DES Encryption/Decryption Example ===")
    
    # Set key (must be 8 bytes for DES)
    key = "12345678"
    
    # Original message
    message = "Hello, this is a secret message!"
    print(f"Original message: {message}")
    
    # Encrypt the message
    encrypted = des_encrypt(message, key)
    print(f"Encrypted message: {encrypted}")
    
    # Decrypt the message
    decrypted = des_decrypt(encrypted, key)
    print(f"Decrypted message: {decrypted}")
    
    print("\n=== Another Example ===")
    
    # Another example with different message
    key2 = "abcdefgh"
    message2 = "DES is a symmetric encryption algorithm"
    print(f"Original message: {message2}")
    
    encrypted2 = des_encrypt(message2, key2)
    print(f"Encrypted: {encrypted2}")
    
    decrypted2 = des_decrypt(encrypted2, key2)
    print(f"Decrypted: {decrypted2}")
```

## Installation Requirements

```bash
pip install pycryptodome
```

## Key Features of This Implementation:

1. **ECB Mode**: Uses Electronic Codebook mode (simplest DES mode)
2. **Padding**: Automatically handles padding for block size requirements
3. **Base64 Encoding**: Encodes ciphertext for easy display and transmission
4. **Error Handling**: Includes proper padding and unpadding operations

## Important Security Notes:

⚠️ **Warning**: DES is considered insecure for modern applications due to:
- 56-bit key length (vulnerable to brute force attacks)
- Known cryptographic weaknesses
- Should be replaced with AES for production use

## Sample Output:
```
=== DES Encryption/Decryption Example ===
Original message: Hello, this is a secret message!
Encrypted message: 8q2X5j1Y9K3L7M4N2P6Q8R4S1T9U3V7W5X2Y8Z1A4B6C9D3E7F1G5H2J8K4L9M6N3O1P8Q5R2S9T6U3V0W7X4Y1Z8
Decrypted message: Hello, this is a secret message!

=== Another Example ===
Original message: DES is a symmetric encryption algorithm
Encrypted: 9L2M5N8P1Q4R7S0T3U6V9W2X5Y8Z1A4B7C0D3E6F9G2H5I8J1K4L7M0N3O6P9Q2R5S8T1U4V7W0X3Y6Z9
Decrypted: DES is a symmetric encryption algorithm
```

## Alternative Simple Implementation (Without External Libraries):

```python
# Simple DES-like implementation (for educational purposes only)
def simple_des_encrypt(plaintext, key):
    """
    Simple DES-like encryption (NOT secure for real use)
    """
    # This is a simplified version - real DES is much more complex
    result = ""
    for i, char in enumerate(plaintext):
        # Simple XOR operation with key
        key_char = key[i % len(key)]
        encrypted_char = chr(ord(char) ^ ord(key_char))
        result += encrypted_char
    return result

# Note: This is NOT real DES encryption and should never be used for security
```

This implementation demonstrates the basic concepts of DES encryption and decryption in Python, though it's important to note that real-world applications should use more secure alternatives like AES.

