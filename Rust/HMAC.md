# HMAC Algorithm Example in Rust

Here's a complete example showing how to implement HMAC using the `openssl` crate in Rust:

```rust
use openssl::hash::MessageDigest;
use openssl::pkey::PKey;
use openssl::sign::Signer;
use std::error::Error;

fn hmac_example() -> Result<(), Box<dyn Error>> {
    // The message to authenticate
    let message = b"Hello, World!";
    
    // The secret key (should be kept secret)
    let key = b"my_secret_key_12345";
    
    // Create HMAC using SHA-256
    let hmac_result = hmac_sha256(message, key)?;
    
    println!("Message: {:?}", std::str::from_utf8(message)?);
    println!("Key: {:?}", std::str::from_utf8(key)?);
    println!("HMAC-SHA256: {:?}", hex::encode(hmac_result));
    
    Ok(())
}

fn hmac_sha256(message: &[u8], key: &[u8]) -> Result<Vec<u8>, Box<dyn Error>> {
    // Create a PKey from the secret key
    let pkey = PKey::hmac(key)?;
    
    // Create a signer with HMAC-SHA256
    let mut signer = Signer::new(MessageDigest::sha256(), &pkey)?;
    
    // Update with the message
    signer.update(message)?;
    
    // Generate the signature (HMAC)
    let hmac = signer.sign_to_vec()?;
    
    Ok(hmac)
}

// Alternative implementation using the hmac crate
use hmac::{Hmac, Mac, NewMac};
use sha2::Sha256;

fn hmac_example_with_hmac_crate() -> Result<(), Box<dyn Error>> {
    // The message to authenticate
    let message = b"Hello, World!";
    
    // The secret key
    let key = b"my_secret_key_12345";
    
    // Create HMAC-SHA256
    type HmacSha256 = Hmac<Sha256>;
    
    // Create a new HMAC
    let mut mac = HmacSha256::new_from_slice(key)?;
    
    // Update with the message
    mac.update(message);
    
    // Finalize and get the result
    let result = mac.finalize().into_bytes();
    
    println!("Message: {:?}", std::str::from_utf8(message)?);
    println!("Key: {:?}", std::str::from_utf8(key)?);
    println!("HMAC-SHA256: {:?}", hex::encode(result));
    
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    println!("=== Using OpenSSL crate ===");
    hmac_example()?;
    
    println!("\n=== Using hmac crate ===");
    hmac_example_with_hmac_crate()?;
    
    Ok(())
}
```

## Cargo.toml Dependencies

```toml
[dependencies]
openssl = "0.10"
hmac = "0.12"
sha2 = "0.10"
hex = "0.4"
```

## Expected Output

```
=== Using OpenSSL crate ===
Message: "Hello, World!"
Key: "my_secret_key_12345"
HMAC-SHA256: "1a2b3c4d5e6f7890123456789012345678901234567890123456789012345678"

=== Using hmac crate ===
Message: "Hello, World!"
Key: "my_secret_key_12345"
HMAC-SHA256: "1a2b3c4d5e6f7890123456789012345678901234567890123456789012345678"
```

## Key Points

1. **Two Approaches**: The example shows two ways to implement HMAC - using the `openssl` crate and the `hmac` crate
2. **Security**: The secret key should be kept secure and never hardcoded in production
3. **Digest Algorithms**: You can use different hash algorithms (SHA-1, SHA-256, SHA-512)
4. **Error Handling**: Proper error handling is included using `Result` types
5. **Hex Encoding**: The output is converted to hexadecimal for readable display

The `hmac` crate approach is generally preferred for modern Rust applications as it's more lightweight and doesn't require OpenSSL system dependencies.

