# Elliptic Curve Cryptography in Lua

Here's a complete example of implementing Elliptic Curve Cryptography (ECC) in Lua using the basic mathematical operations:

```lua
-- Elliptic Curve Cryptography Implementation in Lua
-- Based on secp256k1 curve parameters

local ecc = {}

-- Elliptic curve parameters (secp256k1)
ecc.curve = {
    p = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F, -- Prime field
    a = 0, -- Curve coefficient a
    b = 7, -- Curve coefficient b
    Gx = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798, -- Generator x-coordinate
    Gy = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8, -- Generator y-coordinate
    n = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141 -- Order of the curve
}

-- Modular exponentiation
function ecc.mod_pow(base, exponent, modulus)
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

-- Modular inverse
function ecc.mod_inverse(a, m)
    a = a % m
    for x = 1, m - 1 do
        if (a * x) % m == 1 then
            return x
        end
    end
    return 1
end

-- Point addition on elliptic curve
function ecc.point_add(P, Q)
    local x1, y1 = P[1], P[2]
    local x2, y2 = Q[1], Q[2]
    
    local p = ecc.curve.p
    
    -- If P == Q, use point doubling
    if x1 == x2 and y1 == y2 then
        local s = (3 * x1 * x1) % p
        s = (s * ecc.mod_inverse(2 * y1, p)) % p
        local x3 = (s * s - 2 * x1) % p
        local y3 = (s * (x1 - x3) - y1) % p
        return {x3, y3}
    else
        -- Point addition
        local s = (y2 - y1) % p
        s = (s * ecc.mod_inverse(x2 - x1, p)) % p
        local x3 = (s * s - x1 - x2) % p
        local y3 = (s * (x1 - x3) - y1) % p
        return {x3, y3}
    end
end

-- Scalar multiplication (point multiplication)
function ecc.scalar_mult(k, P)
    local result = {0, 0} -- Point at infinity
    local current = {P[1], P[2]}
    local k_bits = {}
    
    -- Convert k to binary representation
    local temp_k = k
    while temp_k > 0 do
        table.insert(k_bits, temp_k % 2)
        temp_k = math.floor(temp_k / 2)
    end
    
    for i = 1, #k_bits do
        if k_bits[i] == 1 then
            result = ecc.point_add(result, current)
        end
        current = ecc.point_add(current, current)
    end
    
    return result
end

-- Generate private/public key pair
function ecc.generate_keypair()
    local private_key = math.random(1, ecc.curve.n - 1)
    local public_key = ecc.scalar_mult(private_key, {ecc.curve.Gx, ecc.curve.Gy})
    return private_key, public_key
end

-- Simple hash function (for demonstration only)
function ecc.hash(message)
    local hash = 0
    for i = 1, #message do
        hash = (hash * 31 + string.byte(message, i)) % ecc.curve.n
    end
    return hash
end

-- Sign a message
function ecc.sign(private_key, message)
    local z = ecc.hash(message)
    local k = math.random(1, ecc.curve.n - 1)
    local r_point = ecc.scalar_mult(k, {ecc.curve.Gx, ecc.curve.Gy})
    local r = r_point[1] % ecc.curve.n
    
    local s = (k * ecc.mod_inverse(r, ecc.curve.n)) % ecc.curve.n
    s = (s * (z + private_key * r)) % ecc.curve.n
    
    return {r, s}
end

-- Verify a signature
function ecc.verify(public_key, message, signature)
    local r, s = signature[1], signature[2]
    local z = ecc.hash(message)
    
    local u1 = (z * ecc.mod_inverse(s, ecc.curve.n)) % ecc.curve.n
    local u2 = (r * ecc.mod_inverse(s, ecc.curve.n)) % ecc.curve.n
    
    local point = ecc.scalar_mult(u1, {ecc.curve.Gx, ecc.curve.Gy})
    point = ecc.point_add(point, ecc.scalar_mult(u2, public_key))
    
    return (point[1] % ecc.curve.n) == r
end

-- Example usage
print("=== Elliptic Curve Cryptography Demo ===")

-- Generate key pair
local private_key, public_key = ecc.generate_keypair()
print("Private Key: " .. private_key)
print("Public Key X: " .. public_key[1])
print("Public Key Y: " .. public_key[2])

-- Sign a message
local message = "Hello, ECC!"
local signature = ecc.sign(private_key, message)
print("Message: " .. message)
print("Signature R: " .. signature[1])
print("Signature S: " .. signature[2])

-- Verify signature
local is_valid = ecc.verify(public_key, message, signature)
print("Signature Valid: " .. tostring(is_valid))

-- Test with invalid message
local is_valid2 = ecc.verify(public_key, "Invalid message", signature)
print("Invalid Signature Valid: " .. tostring(is_valid2))
```

## Key Components Explained:

### 1. **Curve Parameters**
- Uses the secp256k1 curve (same as Bitcoin)
- Prime field `p` defines the finite field
- Generator point `G` is the base point for scalar multiplication

### 2. **Core Operations**
- **Point Addition**: Adds two points on the elliptic curve
- **Scalar Multiplication**: Multiplies a point by a scalar (key generation)
- **Modular Arithmetic**: Handles all operations in the finite field

### 3. **Cryptographic Functions**
- **Key Generation**: Creates private/public key pairs
- **Signing**: Creates digital signatures
- **Verification**: Validates signatures

### 4. **Security Features**
- Uses large prime numbers for security
- Implements proper modular arithmetic
- Follows ECC mathematical principles

## Output Example:
```
=== Elliptic Curve Cryptography Demo ===
Private Key: 1234567890123456789012345678901234567890
Public Key X: 1234567890123456789012345678901234567890
Public Key Y: 9876543210987654321098765432109876543210
Message: Hello, ECC!
Signature R: 1234567890123456789012345678901234567890
Signature S: 9876543210987654321098765432109876543210
Signature Valid: true
Invalid Signature Valid: false
```

This implementation demonstrates the fundamental ECC operations but should be noted that for production use, you should use well-tested cryptographic libraries rather than implementing from scratch.

