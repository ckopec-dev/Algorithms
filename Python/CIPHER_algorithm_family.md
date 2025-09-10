# AES Cipher Algorithm Example in Python

```python
from cryptography.fernet import Fernet
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
import base64
import os

# Example 1: Using Fernet (Symmetric AES)
def fernet_example():
    # Generate a key
    key = Fernet.generate_key()
    cipher_suite = Fernet(key)
    
    # Encrypt data
    plaintext = b"Hello, World! This is a secret message."
    encrypted_text = cipher_suite.encrypt(plaintext)
    
    # Decrypt data
    decrypted_text = cipher_suite.decrypt(encrypted_text)
    
    print("Fernet Example:")
    print(f"Original: {plaintext}")
    print(f"Encrypted: {encrypted_text}")
    print(f"Decrypted: {decrypted_text}")
    print()

# Example 2: Using AES with PBKDF2 key derivation
def aes_pbkdf2_example():
    # Generate a salt
    salt = os.urandom(16)
    
    # Password-based key derivation
    password = b"my_secret_password"
    kdf = PBKDF2HMAC(
        algorithm=hashes.SHA256(),
        length=32,
        salt=salt,
        iterations=100000,
    )
    key = base64.urlsafe_b64encode(kdf.derive(password))
    
    # Encrypt using AES (using Fernet for simplicity)
    cipher_suite = Fernet(key)
    plaintext = b"Secret message with password protection"
    encrypted_text = cipher_suite.encrypt(plaintext)
    decrypted_text = cipher_suite.decrypt(encrypted_text)
    
    print("AES with PBKDF2 Example:")
    print(f"Original: {plaintext}")
    print(f"Encrypted: {encrypted_text}")
    print(f"Decrypted: {decrypted_text}")
    print()

# Example 3: Direct AES encryption using pycryptodome
try:
    from Crypto.Cipher import AES
    from Crypto.Random import get_random_bytes
    
    def aes_direct_example():
        # Generate a random key (32 bytes for AES-256)
        key = get_random_bytes(32)
        
        # Create cipher object
        cipher = AES.new(key, AES.MODE_EAX)
        
        # Encrypt data
        data = b"Direct AES encryption example"
        ciphertext, tag = cipher.encrypt_and_digest(data)
        
        # Decrypt data
        cipher_decrypt = AES.new(key, AES.MODE_EAX, nonce=cipher.nonce)
        plaintext = cipher_decrypt.decrypt_and_verify(ciphertext, tag)
        
        print("Direct AES Example:")
        print(f"Original: {data}")
        print(f"Encrypted: {ciphertext}")
        print(f"Decrypted: {plaintext}")
        print()
        
except ImportError:
    print("PyCryptodome not installed. Install with: pip install pycryptodome")

# Run examples
fernet_example()
aes_pbkdf2_example()

# Additional cipher algorithms available in Python:
"""
Other common cipher algorithms in Python include:

1. **RSA** - Asymmetric encryption (public/private key)
2. **ChaCha20** - Stream cipher
3. **Blowfish** - Block cipher
4. **DES/Triple DES** - Older symmetric ciphers
5. **SHA-256/SHA-3** - Hash functions (not encryption, but related)

The `cryptography` library provides high-level interfaces for most standard algorithms.
"""
```

## Installation Requirements

```bash
pip install cryptography pycryptodome
```

## Key Points About AES Cipher Family:

- **AES (Advanced Encryption Standard)** is the most widely used symmetric cipher
- **Key Sizes**: 128, 192, or 256 bits
- **Modes of Operation**: ECB, CBC, CFB, OFB, GCM
- **Fernet**: High-level symmetric encryption using AES-128 in CBC mode with HMAC
- **Security**: AES is considered secure when used properly

## Security Considerations:

- Always use random initialization vectors (IVs)
- Never reuse IVs with the same key
- Use authenticated encryption modes (GCM, EAX) when possible
- Implement proper key management practices

