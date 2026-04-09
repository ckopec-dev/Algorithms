# Elliptic Curve Cryptography in Rust

Here's a complete example of implementing Elliptic Curve Cryptography using the `k256` crate in Rust:

```rust
use k256::{
    ecdsa::{SigningKey, VerifyingKey, Signature, signature::Signer, signature::Verifier},
    EncodedPoint, SecretKey, PublicKey
};
use rand::rngs::OsRng;
use std::str::FromStr;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Generate a random EC private key
    let mut csprng = OsRng;
    let signing_key = SigningKey::random(&mut csprng);
    
    // 2. Get the corresponding public key
    let verifying_key = signing_key.verifying_key();
    
    // 3. Create a message to sign
    let message = b"Hello, Elliptic Curve Cryptography!";
    
    // 4. Sign the message
    let signature: Signature = signing_key.sign(message);
    
    // 5. Verify the signature
    assert!(verifying_key.verify(message, &signature).is_ok());
    
    println!("Signature verification successful!");
    println!("Message: {:?}", std::str::from_utf8(message)?);
    println!("Signature: {}", signature);
    
    // 6. Demonstrate key serialization
    let public_key_bytes = verifying_key.to_bytes();
    let public_key_hex = hex::encode(public_key_bytes);
    println!("Public key (hex): {}", public_key_hex);
    
    // 7. Create a new key pair from a secret scalar
    let secret_scalar = "1856303545574535783301100402111123530341122255533333333333333333";
    let secret_key = SecretKey::from_str(secret_scalar)?;
    let new_signing_key = SigningKey::from(secret_key);
    let new_verifying_key = new_signing_key.verifying_key();
    
    println!("New public key: {}", new_verifying_key.to_bytes());
    
    // 8. Demonstrate ECDH key exchange
    let other_signing_key = SigningKey::random(&mut csprng);
    let other_verifying_key = other_signing_key.verifying_key();
    
    // Both parties generate shared secret
    let shared_secret1 = signing_key.to_secret_scalar().multiply(other_verifying_key.to_encoded_point(false));
    let shared_secret2 = other_signing_key.to_secret_scalar().multiply(verifying_key.to_encoded_point(false));
    
    // The shared secrets should be equal
    assert_eq!(shared_secret1, shared_secret2);
    
    println!("ECDH key exchange successful!");
    
    Ok(())
}
```

## Cargo.toml dependencies

```toml
[dependencies]
k256 = "0.13"
rand = "0.8"
hex = "0.4"
```

## Key Features Demonstrated

1. **Key Generation**: Random private key generation using CSPRNG
2. **Signing**: ECDSA signature creation
3. **Verification**: Signature verification using public key
4. **Key Serialization**: Converting keys to/from bytes
5. **ECDH**: Elliptic Curve Diffie-Hellman key exchange
6. **Error Handling**: Proper error propagation with `Result` types

## Security Considerations

- Uses `OsRng` for cryptographically secure random number generation
- Implements NIST P-256 (secp256r1) curve
- Follows ECDSA standards for signature generation and verification
- Proper error handling for cryptographic operations

This example provides a solid foundation for implementing elliptic curve cryptography in Rust applications, suitable for secure communications, digital signatures, and key exchange protocols.

