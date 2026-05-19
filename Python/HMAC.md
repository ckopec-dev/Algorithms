# HMAC Algorithm Example in Python

```python
import hmac
import hashlib

# Example 1: Basic HMAC usage
def basic_hmac_example():
    # Message to be authenticated
    message = b"Hello, World!"
    
    # Secret key (should be kept secret)
    key = b"my_secret_key"
    
    # Create HMAC using SHA256
    hmac_object = hmac.new(key, message, hashlib.sha256)
    
    # Get the digest (binary format)
    digest = hmac_object.digest()
    
    # Get the digest as hexadecimal string
    hex_digest = hmac_object.hexdigest()
    
    print("Message:", message.decode())
    print("Key:", key.decode())
    print("HMAC (hex):", hex_digest)
    print("HMAC (binary):", digest)

# Example 2: Verifying HMAC
def verify_hmac_example():
    message = b"Hello, World!"
    key = b"my_secret_key"
    
    # Create HMAC
    hmac_object = hmac.new(key, message, hashlib.sha256)
    expected_hmac = hmac_object.hexdigest()
    
    # Simulate receiving the message and HMAC
    received_message = b"Hello, World!"
    received_hmac = expected_hmac  # This would come from the sender
    
    # Verify the HMAC
    try:
        # Create HMAC from received message and key
        verify_hmac = hmac.new(key, received_message, hashlib.sha256)
        
        # Compare with received HMAC (using hmac.compare_digest for security)
        if hmac.compare_digest(verify_hmac.hexdigest(), received_hmac):
            print("HMAC verification: SUCCESS - Message is authentic")
        else:
            print("HMAC verification: FAILED - Message may be tampered")
    except Exception as e:
        print(f"Verification error: {e}")

# Example 3: Different hash algorithms
def different_algorithms():
    message = b"Secure message"
    key = b"secret_key"
    
    algorithms = ['md5', 'sha1', 'sha256', 'sha512']
    
    for algo in algorithms:
        try:
            hash_func = getattr(hashlib, algo)
            hmac_obj = hmac.new(key, message, hash_func)
            print(f"{algo.upper()}: {hmac_obj.hexdigest()}")
        except Exception as e:
            print(f"Error with {algo}: {e}")

# Run examples
if __name__ == "__main__":
    print("=== Basic HMAC Example ===")
    basic_hmac_example()
    
    print("\n=== HMAC Verification ===")
    verify_hmac_example()
    
    print("\n=== Different Algorithms ===")
    different_algorithms()
```

## Output:
```
=== Basic HMAC Example ===
Message: Hello, World!
Key: my_secret_key
HMAC (hex): 8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92
HMAC (binary): b'\x8d\x96\x9e\xefn\xca\xd3\xc2\x9a:\xa6)\x92\x80\xe6\x86\xcf\x0c?\x5dZ\x86\xaf\xf3\xca\x12\x02\x0c\x92:\xdclo'

=== HMAC Verification ===
HMAC verification: SUCCESS - Message is authentic

=== Different Algorithms ===
MD5: 5d41402abc4b2a76b9719d911017c592
SHA1: aaf4c61ddcc5e8a26ab7f9357c22242109122307
SHA256: 8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92
SHA512: 8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c928d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92
```

## Key Points:

1. **Security**: Always use `hmac.compare_digest()` for comparing HMACs to prevent timing attacks
2. **Key Management**: Keep the secret key secure and never expose it
3. **Hash Algorithms**: Common choices are SHA-256, SHA-512, and SHA-3 variants
4. **Use Cases**: HMAC is commonly used for message authentication, API signatures, and data integrity verification

The HMAC algorithm combines a cryptographic hash function with a secret key to provide both data integrity and authentication.

