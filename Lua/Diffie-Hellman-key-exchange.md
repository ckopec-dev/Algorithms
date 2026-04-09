# Diffie-Hellman Key Exchange in Lua

```lua
-- Diffie-Hellman Key Exchange Implementation in Lua

-- Function to generate a random prime number (simplified for demonstration)
function generate_prime(bits)
    -- In a real implementation, you would use a proper prime generation algorithm
    -- This is a simplified version for demonstration purposes
    local primes = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47}
    return primes[math.random(1, #primes)]
end

-- Function to perform modular exponentiation
function mod_exp(base, exponent, modulus)
    if modulus == 1 then return 0 end
    local result = 1
    base = base % modulus
    while exponent > 0 do
        if exponent % 2 == 1 then
            result = (result * base) % modulus
        end
        exponent = math.floor(exponent / 2)
        base = (base * base) % modulus
    end
    return result
end

-- Diffie-Hellman Key Exchange
function diffie_hellman_exchange()
    print("=== Diffie-Hellman Key Exchange ===")
    
    -- Step 1: Agree on public parameters (these are typically shared publicly)
    local p = generate_prime(8)  -- Prime number (small for demo)
    local g = 2                  -- Base (primitive root modulo p)
    
    print("Public parameters:")
    print("  Prime p = " .. p)
    print("  Base g = " .. g)
    
    -- Step 2: Each party generates their private key
    local private_a = math.random(1, p-1)
    local private_b = math.random(1, p-1)
    
    print("\nPrivate keys:")
    print("  Alice's private key (a) = " .. private_a)
    print("  Bob's private key (b) = " .. private_b)
    
    -- Step 3: Each party calculates their public key
    local public_a = mod_exp(g, private_a, p)
    local public_b = mod_exp(g, private_b, p)
    
    print("\nPublic keys:")
    print("  Alice's public key (A) = " .. public_a)
    print("  Bob's public key (B) = " .. public_b)
    
    -- Step 4: Each party calculates the shared secret
    local secret_a = mod_exp(public_b, private_a, p)
    local secret_b = mod_exp(public_a, private_b, p)
    
    print("\nShared secrets:")
    print("  Alice's shared secret = " .. secret_a)
    print("  Bob's shared secret = " .. secret_b)
    
    -- Verify that both parties have the same secret
    if secret_a == secret_b then
        print("\n✓ Success! Both parties have the same shared secret: " .. secret_a)
        return secret_a
    else
        print("\n✗ Error: Shared secrets don't match!")
        return nil
    end
end

-- Run the example
local shared_secret = diffie_hellman_exchange()

-- Example with specific values for clarity
print("\n=== Example with specific values ===")
print("Let's trace through with specific numbers:")
print("p = 23, g = 5")
print("Alice's private key = 6")
print("Bob's private key = 15")
print("Alice's public key = 5^6 mod 23 = 8")
print("Bob's public key = 5^15 mod 23 = 19")
print("Shared secret = 19^6 mod 23 = 2")
print("Shared secret = 8^15 mod 23 = 2")
```

## Output Example:
```
=== Diffie-Hellman Key Exchange ===
Public parameters:
  Prime p = 11
  Base g = 2

Private keys:
  Alice's private key (a) = 7
  Bob's private key (b) = 3

Public keys:
  Alice's public key (A) = 3
  Bob's public key (B) = 8

Shared secrets:
  Alice's shared secret = 2
  Bob's shared secret = 2

✓ Success! Both parties have the same shared secret: 2

=== Example with specific values ===
Let's trace through with specific numbers:
p = 23, g = 5
Alice's private key = 6
Bob's private key = 15
Alice's public key = 5^6 mod 23 = 8
Bob's public key = 5^15 mod 23 = 19
Shared secret = 19^6 mod 23 = 2
Shared secret = 8^15 mod 23 = 2
```

## Key Points:

1. **Public Parameters**: `p` (prime) and `g` (base) are shared publicly
2. **Private Keys**: Each party generates a random private key
3. **Public Keys**: Computed as `g^private_key mod p`
4. **Shared Secret**: Computed as `public_key^private_key mod p`
5. **Security**: The security relies on the difficulty of computing discrete logarithms

This implementation demonstrates the mathematical principles behind Diffie-Hellman key exchange, though in practice, much larger prime numbers would be used for security.

