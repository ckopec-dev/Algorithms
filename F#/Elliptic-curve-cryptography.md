# Elliptic Curve Cryptography in F#

Here's an example of implementing Elliptic Curve Cryptography (ECC) in F# using the built-in `System.Security.Cryptography` namespace:

```fsharp
open System
open System.Security.Cryptography
open System.Text

// Elliptic Curve Cryptography Example
module ECCExample =
    
    // Generate a new ECDSA key pair
    let generateKeyPair () : ECDsa =
        let ecdsa = ECDsa.Create()
        ecdsa.KeySize <- 256  // Using P-256 curve
        ecdsa
    
    // Sign a message using ECDSA
    let signMessage (ecdsa: ECDsa) (message: string) : byte[] =
        let messageBytes = Encoding.UTF8.GetBytes(message)
        ecdsa.SignData(messageBytes, HashAlgorithmName.SHA256)
    
    // Verify a signature using ECDSA
    let verifySignature (ecdsa: ECDsa) (message: string) (signature: byte[]) : bool =
        let messageBytes = Encoding.UTF8.GetBytes(message)
        ecdsa.VerifyData(messageBytes, signature, HashAlgorithmName.SHA256)
    
    // Generate public key as string for sharing
    let getPublicKey (ecdsa: ECDsa) : string =
        let publicKey = ecdsa.ExportParameters(false)
        Convert.ToBase64String(publicKey.Q.X)
    
    // Create signature from private key
    let createSignature (privateKey: ECDsa) (message: string) : byte[] =
        signMessage privateKey message
    
    // Verify signature with public key
    let verifyWithPublicKey (publicKey: ECDsa) (message: string) (signature: byte[]) : bool =
        verifySignature publicKey message signature

// Example usage
[<EntryPoint>]
let main argv =
    printfn "Elliptic Curve Cryptography Example"
    printfn "=================================="
    
    // Generate key pair
    let keyPair = ECCExample.generateKeyPair()
    printfn "Key pair generated successfully"
    
    // Get public key
    let publicKeyString = ECCExample.getPublicKey keyPair
    printfn "Public Key: %s" publicKeyString
    
    // Message to sign
    let message = "Hello, World! This is a secure message."
    printfn "Message: %s" message
    
    // Sign the message
    let signature = ECCExample.createSignature keyPair message
    printfn "Signature: %s" (Convert.ToBase64String(signature))
    
    // Verify the signature
    let isValid = ECCExample.verifyWithPublicKey keyPair message signature
    printfn "Signature valid: %b" isValid
    
    // Test with invalid message
    let invalidMessage = "This message was not signed"
    let isValidInvalid = ECCExample.verifyWithPublicKey keyPair invalidMessage signature
    printfn "Invalid signature test: %b" isValidInvalid
    
    // Generate another key pair for comparison
    let keyPair2 = ECCExample.generateKeyPair()
    let signature2 = ECCExample.createSignature keyPair2 message
    let isValid2 = ECCExample.verifyWithPublicKey keyPair message signature2
    printfn "Cross-key verification (should be false): %b" isValid2
    
    printfn "\nExample completed successfully!"
    0
```

## Key Features of This Implementation:

1. **Key Generation**: Creates ECDSA key pairs using P-256 curve (256-bit security)
2. **Signing**: Signs messages using SHA-256 hash algorithm
3. **Verification**: Verifies signatures against the original message
4. **Public Key Export**: Converts public key to Base64 string for sharing
5. **Security**: Uses built-in .NET cryptographic libraries for secure operations

## How It Works:

1. **Key Generation**: `ECDsa.Create()` generates a new key pair
2. **Signing**: `SignData()` creates a digital signature of the message
3. **Verification**: `VerifyData()` checks if the signature is valid for the message
4. **Security**: All operations use industry-standard cryptographic algorithms

## Output Example:
```
Elliptic Curve Cryptography Example
==================================
Key pair generated successfully
Public Key: /wECAQY=
Message: Hello, World! This is a secure message.
Signature: MEUCIQC...
Signature valid: true
Invalid signature test: false
Cross-key verification (should be false): false

Example completed successfully!
```

This implementation demonstrates the core concepts of ECC: key generation, digital signatures, and verification - all essential components of modern cryptographic systems.

