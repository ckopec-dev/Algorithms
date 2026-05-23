# Diffie-Hellman Key Exchange Algorithm in Python

```python
import random

def diffie_hellman_key_exchange():
    """
    Demonstration of Diffie-Hellman key exchange algorithm
    """
    
    # Step 1: Agree on public parameters (these are known to everyone)
    # Large prime number
    p = 23
    # Primitive root modulo p
    g = 5
    
    print(f"Public parameters:")
    print(f"Prime number (p): {p}")
    print(f"Primitive root (g): {g}")
    print()
    
    # Step 2: Each party selects a secret number
    # Alice's secret number (private)
    a = random.randint(1, p-1)
    print(f"Alice's secret number (a): {a}")
    
    # Bob's secret number (private)
    b = random.randint(1, p-1)
    print(f"Bob's secret number (b): {b}")
    print()
    
    # Step 3: Each party calculates their public value
    # Alice's public value
    A = (g ** a) % p
    print(f"Alice's public value (A): {A}")
    
    # Bob's public value
    B = (g ** b) % p
    print(f"Bob's public value (B): {B}")
    print()
    
    # Step 4: Each party calculates the shared secret
    # Alice calculates shared secret
    shared_secret_alice = (B ** a) % p
    print(f"Alice calculates shared secret: {B}^{a} mod {p} = {shared_secret_alice}")
    
    # Bob calculates shared secret
    shared_secret_bob = (A ** b) % p
    print(f"Bob calculates shared secret: {A}^{b} mod {p} = {shared_secret_bob}")
    print()
    
    # Step 5: Verify that both parties have the same shared secret
    if shared_secret_alice == shared_secret_bob:
        print("✅ Success! Both parties have the same shared secret:")
        print(f"Shared secret: {shared_secret_alice}")
    else:
        print("❌ Error: Shared secrets don't match!")
    
    return shared_secret_alice

# Run the example
if __name__ == "__main__":
    print("Diffie-Hellman Key Exchange Example")
    print("=" * 40)
    shared_secret = diffie_hellman_key_exchange()
```

## Output Example:
```
Diffie-Hellman Key Exchange Example
========================================
Public parameters:
Prime number (p): 23
Primitive root (g): 5

Alice's secret number (a): 6
Bob's secret number (b): 15

Alice's public value (A): 8
Bob's public value (B): 19

Alice calculates shared secret: 19^6 mod 23 = 2
Bob calculates shared secret: 8^15 mod 23 = 2

✅ Success! Both parties have the same shared secret:
Shared secret: 2
```

## How it works:

1. **Public Parameters**: Both parties agree on a large prime number `p` and a primitive root `g`
2. **Private Keys**: Each party selects a secret random number (Alice: `a`, Bob: `b`)
3. **Public Values**: Each party calculates their public value using: `public = g^private mod p`
4. **Shared Secret**: Each party calculates the shared secret using: `shared = other_public^private mod p`

The security comes from the fact that even though `p`, `g`, `A`, and `B` are public, it's computationally difficult to determine the private numbers `a` and `b` from the public values.

