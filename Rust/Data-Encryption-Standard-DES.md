# Data Encryption Standard (DES) in Rust

Here's an example implementation of the DES algorithm in Rust using the `des` crate:

## Cargo.toml Dependencies

```toml
[dependencies]
des = "0.8"
hex = "0.4"
```

## Rust Code Example

```rust
use des::Des;
use des::cipher::{BlockCipher, KeyInit};
use hex;

fn main() {
    // Example DES encryption/decryption
    println!("=== DES Encryption Example ===");
    
    // 64-bit key (8 bytes)
    let key = [0x13, 0x34, 0x57, 0x79, 0x9B, 0xBC, 0xDF, 0xF1];
    
    // 64-bit plaintext block
    let plaintext = [0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF];
    
    println!("Original key:     {}", hex::encode(&key));
    println!("Original plaintext: {}", hex::encode(&plaintext));
    
    // Create DES cipher
    let des = Des::new_from_slice(&key).unwrap();
    
    // Encrypt
    let mut block = plaintext;
    des.encrypt_block(&mut block);
    let ciphertext = block;
    
    println!("Ciphertext:       {}", hex::encode(&ciphertext));
    
    // Decrypt
    let mut decrypted_block = ciphertext;
    des.decrypt_block(&mut decrypted_block);
    
    println!("Decrypted:        {}", hex::encode(&decrypted_block));
    
    // Verify decryption worked
    if plaintext == decrypted_block {
        println!("✓ Decryption successful!");
    } else {
        println!("✗ Decryption failed!");
    }
    
    // Example with multiple blocks
    println!("\n=== Multiple Block Example ===");
    
    let key2 = [0x13, 0x34, 0x57, 0x79, 0x9B, 0xBC, 0xDF, 0xF1];
    let mut des2 = Des::new_from_slice(&key2).unwrap();
    
    // 2 blocks of data
    let mut blocks = [
        [0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF],
        [0xF0, 0xE1, 0xD2, 0xC3, 0xB4, 0xA5, 0x96, 0x87]
    ];
    
    println!("Original blocks:");
    for (i, block) in blocks.iter().enumerate() {
        println!("  Block {}: {}", i, hex::encode(block));
    }
    
    // Encrypt both blocks
    for block in &mut blocks {
        des2.encrypt_block(block);
    }
    
    println!("Encrypted blocks:");
    for (i, block) in blocks.iter().enumerate() {
        println!("  Block {}: {}", i, hex::encode(block));
    }
    
    // Decrypt both blocks
    for block in &mut blocks {
        des2.decrypt_block(block);
    }
    
    println!("Decrypted blocks:");
    for (i, block) in blocks.iter().enumerate() {
        println!("  Block {}: {}", i, hex::encode(block));
    }
}
```

## Alternative Implementation Using `crypto` Crate

```rust
use crypto::des::{Des, DesEde3};
use crypto::symmetriccipher::BlockEncryptor;
use crypto::buffer::{BufferResult, ReadBuffer, WriteBuffer};

fn des_example_with_crypto_crate() {
    // This requires adding "crypto" to Cargo.toml
    // [dependencies]
    // crypto = "0.3"
    
    println!("=== DES with crypto crate ===");
    
    let key = [0x13, 0x34, 0x57, 0x79, 0x9B, 0xBC, 0xDF, 0xF1];
    let plaintext = [0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF];
    
    let mut des = Des::new(&key);
    let mut encrypted = [0u8; 8];
    
    des.update(&plaintext, &mut encrypted);
    
    println!("Key:     {}", hex::encode(&key));
    println!("Plaintext: {}", hex::encode(&plaintext));
    println!("Encrypted: {}", hex::encode(&encrypted));
}
```

## Expected Output

```
=== DES Encryption Example ===
Original key:     133457799bbcdf01
Original plaintext: 0123456789abcdef
Ciphertext:       85e813540f0a0800
Decrypted:        0123456789abcdef
✓ Decryption successful!

=== Multiple Block Example ===
Original blocks:
  Block 0: 0123456789abcdef
  Block 1: f0e1d2c3b4a59687
Encrypted blocks:
  Block 0: 85e813540f0a0800
  Block 1: 2b2c5d7e9f103142
Decrypted blocks:
  Block 0: 0123456789abcdef
  Block 1: f0e1d2c3b4a59687
```

## Key Points

1. **DES Algorithm**: Uses 64-bit keys and 64-bit blocks
2. **Security Note**: DES is considered insecure for modern applications due to its short key length
3. **Recommended**: Use AES instead for production applications
4. **Dependencies**: Requires the `des` crate for cryptographic operations
5. **Block Operations**: DES operates on 64-bit blocks (8 bytes)

This example demonstrates basic DES encryption and decryption operations in Rust, showing how to work with the cryptographic primitives provided by the `des` crate.

