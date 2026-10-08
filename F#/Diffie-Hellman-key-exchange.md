# Diffie-Hellman Key Exchange in F#

Here's a complete implementation of the Diffie-Hellman key exchange algorithm in F#:

```fsharp
open System
open System.Numerics

module DiffieHellman =
    // Generate a random prime number (simplified for demonstration)
    let generatePrime (bits: int) : BigInteger =
        let mutable prime = BigInteger(0)
        let rng = Random()
        
        // Generate a random number with specified bits
        let bytes = Array.create (bits / 8 + 1) 0uy
        rng.NextBytes(bytes)
        bytes.[0] <- bytes.[0] ||| 0x80uy  // Ensure MSB is set
        bytes.[bytes.Length - 1] <- bytes.[bytes.Length - 1] ||| 0x01uy  // Ensure LSB is set
        
        prime <- BigInteger(bytes)
        
        // Find the next prime number (simplified approach)
        let rec findNextPrime n =
            if isPrime n then n
            else findNextPrime (n + 1I)
        
        findNextPrime prime
    
    // Simple primality test (for demonstration purposes)
    let isPrime (n: BigInteger) : bool =
        if n <= 1I then false
        elif n <= 3I then true
        elif n % 2I = 0I || n % 3I = 0I then false
        else
            let rec check i =
                if i * i > n then true
                elif n % i = 0I || n % (i + 2I) = 0I then false
                else check (i + 6I)
            check 5I
    
    // Generate a random private key
    let generatePrivateKey (maxValue: BigInteger) : BigInteger =
        let rng = Random()
        let bytes = Array.create 32 0uy
        rng.NextBytes(bytes)
        let privateKey = BigInteger(bytes) % maxValue
        if privateKey <= 0I then privateKey + 1I else privateKey
    
    // Perform Diffie-Hellman key exchange
    let diffieHellmanExchange (p: BigInteger) (g: BigInteger) (privateKey: BigInteger) : BigInteger =
        // Calculate public key: g^privateKey mod p
        BigInteger.ModPow(g, privateKey, p)
    
    // Calculate shared secret
    let calculateSharedSecret (publicKey: BigInteger) (privateKey: BigInteger) (p: BigInteger) : BigInteger =
        // Calculate shared secret: publicKey^privateKey mod p
        BigInteger.ModPow(publicKey, privateKey, p)

// Example usage
[<EntryPoint>]
let main argv =
    printfn "Diffie-Hellman Key Exchange Demo"
    printfn "==================================\n"
    
    // Step 1: Agree on public parameters (these are typically known to both parties)
    let p = BigInteger.Parse("12345678901234567890123456789012345678901234567890123456789012345678901234567890")
    let g = BigInteger(5)
    
    printfn "Public parameters:"
    printfn "Prime number p: %A" p
    printfn "Generator g: %A\n"
    
    // Step 2: Each party generates their private key
    let alicePrivateKey = generatePrivateKey (p - 1I)
    let bobPrivateKey = generatePrivateKey (p - 1I)
    
    printfn "Alice's private key: %A" alicePrivateKey
    printfn "Bob's private key: %A\n" bobPrivateKey
    
    // Step 3: Each party calculates their public key
    let alicePublicKey = diffieHellmanExchange p g alicePrivateKey
    let bobPublicKey = diffieHellmanExchange p g bobPrivateKey
    
    printfn "Alice's public key: %A" alicePublicKey
    printfn "Bob's public key: %A\n" bobPublicKey
    
    // Step 4: Each party calculates the shared secret
    let aliceSharedSecret = calculateSharedSecret bobPublicKey alicePrivateKey p
    let bobSharedSecret = calculateSharedSecret alicePublicKey bobPrivateKey p
    
    printfn "Alice's shared secret: %A" aliceSharedSecret
    printfn "Bob's shared secret: %A\n" bobSharedSecret
    
    // Verify they are the same
    if aliceSharedSecret = bobSharedSecret then
        printfn "✓ Key exchange successful! Both parties have the same shared secret."
    else
        printfn "✗ Key exchange failed! Shared secrets don't match."
    
    Console.ReadLine() |> ignore
    0
```

## How it works:

1. **Public Parameters**: Both parties agree on a large prime number `p` and a generator `g`
2. **Private Keys**: Each party generates their own private key (random number)
3. **Public Keys**: Each party calculates their public key using: `public_key = g^private_key mod p`
4. **Shared Secret**: Both parties calculate the shared secret using: `shared_secret = other_public_key^private_key mod p`

## Key Features:

- Uses `BigInteger` for handling large numbers
- Implements modular exponentiation using `BigInteger.ModPow`
- Includes basic primality testing
- Demonstrates the mathematical principles of Diffie-Hellman

## Note:

This is a simplified implementation for educational purposes. In production systems, you would use well-tested cryptographic libraries and properly sized parameters (typically 2048+ bits) for security.