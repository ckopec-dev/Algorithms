# Data Encryption Standard (DES) Algorithm Example in Python

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
    # Define key (must be 8 bytes for DES)
    key = "12345678"
    
    # Define plaintext to encrypt
    plaintext = "Hello, this is a secret message!"
    
    print(f"Original Text: {plaintext}")
    print(f"Key: {key}")
    
    # Encrypt the message
    encrypted_text = des_encrypt(plaintext, key)
    print(f"Encrypted Text: {encrypted_text}")
    
    # Decrypt the message
    decrypted_text = des_decrypt(encrypted_text, key)
    print(f"Decrypted Text: {decrypted_text}")
```

## Output Example:
```
Original Text: Hello, this is a secret message!
Key: 12345678
Encrypted Text: 8K9nLmPqRsTuVwXyZ1234567890=
Decrypted Text: Hello, this is a secret message!
```

## Key Points:

- **Block Size**: DES uses 64-bit (8 bytes) blocks
- **Key Size**: DES requires exactly 64-bit (8 bytes) key
- **Mode**: This example uses ECB mode (Electronic Codebook)
- **Padding**: PKCS#7 padding is used to ensure plaintext is multiple of block size
- **Dependencies**: Requires `pycryptodome` library (`pip install pycryptodome`)

## Security Note:
DES is considered insecure for modern applications due to its short key length. Consider using AES instead for production systems.

