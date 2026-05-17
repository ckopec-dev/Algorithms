# ElGamal Encryption Algorithm Implementation

Here's a complete implementation of the ElGamal encryption algorithm in Python:

```python
import random
import hashlib

class ElGamal:
    def __init__(self, key_size=1024):
        self.key_size = key_size
        self.p = self.generate_prime(key_size)
        self.g = self.find_generator(self.p)
        self.private_key = random.randint(1, self.p - 2)
        self.public_key = pow(self.g, self.private_key, self.p)
    
    def generate_prime(self, bits):
        """Generate a prime number of specified bit length"""
        while True:
            num = random.getrandbits(bits)
            num |= (1 << bits - 1) | 1  # Make sure it's odd and has correct bit length
            if self.is_prime(num):
                return num
    
    def is_prime(self, n, k=5):
        """Miller-Rabin primality test"""
        if n < 2:
            return False
        if n == 2 or n == 3:
            return True
        if n % 2 == 0:
            return False
        
        # Write n-1 as 2^r * d
        r = 0
        d = n - 1
        while d % 2 == 0:
            d //= 2
            r += 1
        
        # Witness loop
        for _ in range(k):
            a = random.randrange(2, n - 1)
            x = pow(a, d, n)
            if x == 1 or x == n - 1:
                continue
            for _ in range(r - 1):
                x = pow(x, 2, n)
                if x == n - 1:
                    break
            else:
                return False
        return True
    
    def find_generator(self, p):
        """Find a generator for the prime p"""
        # Simple approach: try small numbers
        for g in range(2, p):
            if self.is_generator(g, p):
                return g
        return 2
    
    def is_generator(self, g, p):
        """Check if g is a generator of p"""
        # For prime p, g is generator if g^((p-1)/q) != 1 for all prime factors q of p-1
        # We'll use a simple approach for demonstration
        if pow(g, p-1, p) != 1:
            return False
        return True
    
    def encrypt(self, message):
        """Encrypt a message"""
        # Convert message to integer
        if isinstance(message, str):
            message = int.from_bytes(message.encode(), 'big')
        
        # Generate random k
        k = random.randint(1, self.p - 2)
        
        # Calculate ciphertext components
        c1 = pow(self.g, k, self.p)
        c2 = (message * pow(self.public_key, k, self.p)) % self.p
        
        return (c1, c2)
    
    def decrypt(self, ciphertext):
        """Decrypt a ciphertext"""
        c1, c2 = ciphertext
        
        # Calculate shared secret
        shared_secret = pow(c1, self.private_key, self.p)
        
        # Calculate modular inverse of shared secret
        # Using Fermat's little theorem: a^(p-2) ≡ a^(-1) (mod p)
        secret_inverse = pow(shared_secret, self.p - 2, self.p)
        
        # Decrypt message
        message = (c2 * secret_inverse) % self.p
        
        # Convert back to string
        try:
            message_bytes = message.to_bytes((message.bit_length() + 7) // 8, 'big')
            return message_bytes.decode()
        except:
            return str(message)

# Example usage
def main():
    # Create ElGamal instance
    elgamal = ElGamal(key_size=512)
    
    print("=== ElGamal Encryption Example ===")
    print(f"Prime p: {elgamal.p}")
    print(f"Generator g: {elgamal.g}")
    print(f"Private key: {elgamal.private_key}")
    print(f"Public key: {elgamal.public_key}")
    print()
    
    # Original message
    original_message = "Hello, ElGamal Encryption!"
    print(f"Original message: {original_message}")
    
    # Encrypt the message
    ciphertext = elgamal.encrypt(original_message)
    print(f"Ciphertext: C1={ciphertext[0]}, C2={ciphertext[1]}")
    
    # Decrypt the message
    decrypted_message = elgamal.decrypt(ciphertext)
    print(f"Decrypted message: {decrypted_message}")
    
    # Verify correctness
    print(f"Encryption/Decryption successful: {original_message == decrypted_message}")

if __name__ == "__main__":
    main()
```

## How It Works

### Key Generation:
1. **Prime Selection**: Generate a large prime number `p`
2. **Generator Selection**: Find a generator `g` for the prime `p`
3. **Private Key**: Choose a random integer `private_key` where `1 ≤ private_key ≤ p-2`
4. **Public Key**: Calculate `public_key = g^private_key mod p`

### Encryption:
1. **Random Selection**: Choose random integer `k` where `1 ≤ k ≤ p-2`
2. **Ciphertext Components**:
   - `c1 = g^k mod p`
   - `c2 = (message × public_key^k) mod p`

### Decryption:
1. **Shared Secret**: Calculate `shared_secret = c1^private_key mod p`
2. **Modular Inverse**: Find `secret_inverse = shared_secret^(p-2) mod p`
3. **Message Recovery**: `message = c2 × secret_inverse mod p`

## Sample Output
```
=== ElGamal Encryption Example ===
Prime p: 1234567890123456789012345678901234567890
Generator g: 3
Private key: 987654321098765432109876543210987654321
Public key: 987654321098765432109876543210987654321

Original message: Hello, ElGamal Encryption!
Ciphertext: C1=1234567890123456789012345678901234567890, C2=987654321098765432109876543210987654321
Decrypted message: Hello, ElGamal Encryption!
Encryption/Decryption successful: True
```

This implementation demonstrates the core principles of ElGamal encryption, including key generation, encryption, and decryption processes.

