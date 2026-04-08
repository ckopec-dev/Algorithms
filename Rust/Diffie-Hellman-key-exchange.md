# Diffie-Hellman Key Exchange in Rust

This example demonstrates the Diffie-Hellman key exchange algorithm using Rust. We'll use the `num-bigint` and `num-traits` crates for handling large integers.

## Dependencies

Add these to your `Cargo.toml`:
```toml
[dependencies]
num-bigint = "0.4"
num-traits = "0.2"
```

## Implementation

```rust
use num_bigint::BigUint;
use num_traits::{One, Zero};
use rand::rngs::OsRng;
use rand::RngCore;

/// Performs modular exponentiation: base^exponent mod modulus
fn mod_exp(base: &BigUint, exponent: &BigUint, modulus: &BigUint) -> BigUint {
    base.modpow(exponent, modulus)
}

/// Generates a random private key in the range [2, p-2]
fn generate_private_key(prime: &BigUint) -> BigUint {
    let mut rng = OsRng;
    // Ensure the private key is at least 2 and less than p-1
    let mut private_key;
    loop {
        // Generate a random number in [0, prime-2]
        let random_bytes = (prime.bits() / 8 + 1) as usize;
        let mut buf = vec![0u8; random_bytes];
        rng.fill_bytes(&mut buf);
        private_key = BigUint::from_bytes_be(&buf) % (prime - 2u8) + 2u8;
        
        // Ensure it's in the valid range
        if private_key < prime - 1u8 {
            break;
        }
    }
    private_key
}

/// Computes the public key: g^private_key mod p
fn compute_public_key(private_key: &BigUint, generator: &BigUint, prime: &BigUint) -> BigUint {
    mod_exp(generator, private_key, prime)
}

/// Computes the shared secret: other_public_key^private_key mod p
fn compute_shared_secret(private_key: &BigUint, other_public_key: &BigUint, prime: &BigUint) -> BigUint {
    mod_exp(other_public_key, private_key, prime)
}

fn main() {
    // Public parameters (in practice, these should be well-known safe primes)
    // Using a small prime for demonstration - NOT SECURE FOR REAL USE
    let p = BigUint::from(23u32);  // Prime modulus
    let g = BigUint::from(5u32);   // Generator
    
    println!("Public parameters:");
    println!("Prime (p): {}", p);
    println!("Generator (g): {}", g);
    println!();

    // Alice's side
    println!("--- Alice ---");
    let alice_private = generate_private_key(&p);
    let alice_public = compute_public_key(&alice_private, &g, &p);
    println!("Private key: {}", alice_private);
    println!("Public key: {}", alice_public);
    println!();

    // Bob's side
    println!("--- Bob ---");
    let bob_private = generate_private_key(&p);
    let bob_public = compute_public_key(&bob_private, &g, &p);
    println!("Private key: {}", bob_private);
    println!("Public key: {}", bob_public);
    println!();

    // Shared secret computation
    println!("--- Shared Secret ---");
    let alice_secret = compute_shared_secret(&alice_private, &bob_public, &p);
    let bob_secret = compute_shared_secret(&bob_private, &alice_public, &p);
    
    println!("Alice's computed secret: {}", alice_secret);
    println!("Bob's computed secret: {}", bob_secret);
    
    if alice_secret == bob_secret {
        println!("SUCCESS: Shared secrets match!");
    } else {
        println!("ERROR: Shared secrets do not match!");
    }
}
```

## Important Notes for Production Use

1. **Security**: This example uses small numbers for clarity. In real applications:
   - Use large safe primes (at least 2048 bits)
   - Use cryptographically secure random number generators (we used `OsRng` which is good)
   - Consider using established libraries like `ring` or `openssl` instead of implementing from scratch

2. **Recommended Crates for Production**:
   - `ring`: Modern, secure cryptographic library
   - `openssl`: Bindings to OpenSSL
   - `crypto`: Another option (though less maintained than `ring`)

3. **Using `ring` for DH (simplified example)**:
   ```toml
   [dependencies]
   ring = "0.17"
   ```
   
   ```rust
   use ring::agreement;
   use rand::SystemRandom;
   
   fn main() {
       let rng = SystemRandom::new();
       
       // Generate Alice's key pair
       let alice_priv = agreement::EphemeralPrivateKey::generate(&agreement::X25519, &rng).unwrap();
       let alice_pub = agreement::PublicKey::from(&alice_priv);
       
       // Generate Bob's key pair
       let bob_priv = agreement::EphemeralPrivateKey::generate(&agreement::X25519, &rng).unwrap();
       let bob_pub = agreement::PublicKey::from(&bob_priv);
       
       // Compute shared secrets
       let alice_secret = agreement::agree_ephemeral(
           &alice_priv,
           &bob_pub,
           agreement::UnparsedPublicKey::new(&agreement::X25519, bob_pub.as_ref()),
           |key_material| Ok(key_material.to_vec())
       ).unwrap();
       
       let bob_secret = agreement::agree_ephemeral(
           &bob_priv,
           &alice_pub,
           agreement::UnparsedPublicKey::new(&agreement::X25519, alice_pub.as_ref()),
           |key_material| Ok(key_material.to_vec())
       ).unwrap();
       
       assert_eq!(alice_secret, bob_secret);
       println!("Shared secret established successfully!");
   }
   ```

## How It Works

1. Both parties agree on public parameters: a large prime `p` and a generator `g`
2. Each party generates a private key (random number)
3. Each party computes their public key: `g^private_key mod p`
4. Parties exchange public keys
5. Each party computes the shared secret: `other_party's_public_key^private_key mod p`
6. Due to modular arithmetic properties, both parties arrive at the same shared secret

The security relies on the difficulty of the discrete logarithm problem: given `g`, `p`, and `g^a mod p`, it's computationally infeasible to find `a`.