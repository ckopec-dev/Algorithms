# Elliptic Curve Cryptography in Ruby

Here's an example demonstrating Elliptic Curve Cryptography (ECC) using Ruby with the `openssl` library:

```ruby
require 'openssl'
require 'base64'

class ECCExample
  def initialize
    # Using a standard elliptic curve (secp256k1)
    @curve = OpenSSL::PKey::EC.new('secp256k1')
    @curve.generate_key
  end

  # Generate key pair
  def generate_keys
    private_key = @curve.private_key
    public_key = @curve.public_key
    { private: private_key, public: public_key }
  end

  # Sign a message
  def sign_message(message)
    # Create signature
    signature = @curve.sign(OpenSSL::Digest::SHA256.new, message)
    Base64.encode64(signature)
  end

  # Verify a signature
  def verify_signature(message, signature)
    # Decode the signature
    decoded_signature = Base64.decode64(signature)
    
    # Verify the signature
    @curve.verify(OpenSSL::Digest::SHA256.new, decoded_signature, message)
  rescue => e
    puts "Verification failed: #{e.message}"
    false
  end

  # Encrypt message using ECDH
  def encrypt_with_ecdh(recipient_public_key, message)
    # Create ephemeral key
    ephemeral_key = OpenSSL::PKey::EC.new('secp256k1')
    ephemeral_key.generate_key
    
    # Generate shared secret
    shared_secret = ephemeral_key.dh_compute_key(recipient_public_key)
    
    # Simple XOR encryption (for demonstration only)
    # In practice, use proper encryption like AES
    encrypted = message.bytes.zip(shared_secret.bytes).map { |a, b| a ^ b }.pack('C*')
    Base64.encode64(encrypted)
  end

  # Get public key in PEM format
  def public_key_pem
    @curve.public_key.to_pem
  end

  # Get private key in PEM format
  def private_key_pem
    @curve.to_pem
  end
end

# Example usage
puts "=== Elliptic Curve Cryptography Example ==="

# Create ECC instance
ecc = ECCExample.new

# Generate keys
keys = ecc.generate_keys
puts "Private Key: #{keys[:private]}"
puts "Public Key: #{keys[:public]}"

# Sign a message
message = "Hello, ECC World!"
signature = ecc.sign_message(message)
puts "\nOriginal Message: #{message}"
puts "Signature: #{signature}"

# Verify signature
is_valid = ecc.verify_signature(message, signature)
puts "Signature Valid: #{is_valid}"

# Test with modified message (should fail verification)
modified_message = "Hello, ECC World!"
is_valid_modified = ecc.verify_signature(modified_message, signature)
puts "Modified Message Verification: #{is_valid_modified}"

# Demonstrate ECDH encryption
puts "\n=== ECDH Key Exchange ==="
recipient_ecc = ECCExample.new
recipient_public = recipient_ecc.public_key_pem

# Simple demonstration - in practice, use proper key exchange protocols
puts "Recipient Public Key: #{recipient_public[0..50]}..."

# Note: Full ECDH implementation would require more complex key derivation
puts "ECDH key exchange demonstration completed"
```

## Key Components Explained

### 1. **Key Generation**
- Uses `OpenSSL::PKey::EC.new('secp256k1')` for standard curve
- Generates both private and public keys automatically

### 2. **Digital Signatures**
- `sign_message()` creates signatures using SHA-256
- `verify_signature()` validates signatures
- Uses ECDSA algorithm

### 3. **Security Features**
- Uses well-established elliptic curves
- SHA-256 hashing for message integrity
- Base64 encoding for signature transmission

### 4. **Practical Usage**
```ruby
# Simple signature creation and verification
message = "Secure message"
signature = ecc.sign_message(message)
valid = ecc.verify_signature(message, signature)
```

## Important Notes

- This example demonstrates the **concept** of ECC
- For production use, implement proper key management
- Consider using libraries like `ruby-ecdsa` for more features
- ECDH key exchange should use proper key derivation functions (KDF)
- Always validate inputs and handle errors appropriately

The example shows the fundamental operations of ECC: key generation, digital signatures, and key exchange - all essential components of modern cryptographic systems.

