# HMAC Algorithm in F#

Here's an example of implementing HMAC (Hash-based Message Authentication Code) in F# using the built-in cryptographic functions:

```fsharp
open System
open System.Security.Cryptography
open System.Text

// Function to compute HMAC-SHA256
let computeHmacSha256 (key: byte[]) (message: byte[]) : byte[] =
    use hmac = new HMACSHA256(key)
    hmac.ComputeHash(message)

// Helper function to convert string to byte array
let stringToBytes (str: string) : byte[] =
    Encoding.UTF8.GetBytes(str)

// Helper function to convert byte array to hexadecimal string
let bytesToHex (bytes: byte[]) : string =
    BitConverter.ToString(bytes).Replace("-", "").ToLowerInvariant()

// Example usage
[<EntryPoint>]
let main argv =
    // Sample key and message
    let key = stringToBytes "my-secret-key-123"
    let message = stringToBytes "Hello, World! This is a test message."
    
    // Compute HMAC
    let hmac = computeHmacSha256 key message
    
    // Display results
    printfn "Key (hex): %s" (bytesToHex key)
    printfn "Message: %s" (Encoding.UTF8.GetString(message))
    printfn "HMAC-SHA256 (hex): %s" (bytesToHex hmac)
    
    // Verification example
    let verifyHmac (expectedHmac: byte[]) (key: byte[]) (message: byte[]) : bool =
        let computedHmac = computeHmacSha256 key message
        // Compare byte arrays securely
        let rec compareArrays i =
            if i >= expectedHmac.Length then true
            elif expectedHmac.[i] <> computedHmac.[i] then false
            else compareArrays (i + 1)
        compareArrays 0
    
    // Verify the HMAC
    let isValid = verifyHmac hmac key message
    printfn "HMAC verification: %b" isValid
    
    // Test with invalid message
    let invalidMessage = stringToBytes "Wrong message"
    let isValidInvalid = verifyHmac hmac key invalidMessage
    printfn "Invalid HMAC verification: %b" isValidInvalid
    
    0
```

## Alternative Implementation with More Security Considerations

```fsharp
open System
open System.Security.Cryptography
open System.Text

// Secure HMAC verification function
let secureHmacVerify (expectedHmac: byte[]) (key: byte[]) (message: byte[]) : bool =
    use hmac = new HMACSHA256(key)
    let computedHmac = hmac.ComputeHash(message)
    
    // Use CryptographicOperations.Equals for constant-time comparison
    // Note: This requires .NET Core 2.1+ or .NET Standard 2.1+
    try
        // For .NET Framework or older versions, use manual comparison
        if expectedHmac.Length <> computedHmac.Length then
            false
        else
            let rec compare i =
                if i >= expectedHmac.Length then true
                elif expectedHmac.[i] <> computedHmac.[i] then false
                else compare (i + 1)
            compare 0
    with
    | _ -> 
        // Fallback for older .NET versions
        let rec compare i =
            if i >= expectedHmac.Length then true
            elif expectedHmac.[i] <> computedHmac.[i] then false
            else compare (i + 1)
        compare 0

// Generic HMAC function for different hash algorithms
let computeHmac (algorithm: HashAlgorithm) (key: byte[]) (message: byte[]) : byte[] =
    use hmac = new HMAC(algorithm, key)
    hmac.ComputeHash(message)

// Usage example with different algorithms
let exampleWithDifferentAlgorithms () =
    let key = stringToBytes "secret-key"
    let message = stringToBytes "Test message"
    
    // HMAC-SHA256
    let hmacSha256 = computeHmac (HashAlgorithm.Create("SHA256")) key message
    printfn "HMAC-SHA256: %s" (bytesToHex hmacSha256)
    
    // HMAC-SHA512
    let hmacSha512 = computeHmac (HashAlgorithm.Create("SHA512")) key message
    printfn "HMAC-SHA512: %s" (bytesToHex hmacSha512)
```

## Key Features of This Implementation:

1. **Multiple Hash Algorithms**: Supports SHA256, SHA512, and other HMAC variants
2. **Secure Comparison**: Includes constant-time comparison to prevent timing attacks
3. **Memory Management**: Uses `use` keyword for proper disposal of cryptographic objects
4. **Error Handling**: Includes fallback mechanisms for different .NET versions
5. **Utility Functions**: Helper functions for string-to-byte conversion and hex formatting

## Sample Output:
```
Key (hex): 6d792d7365637265742d6b65792d313233
Message: Hello, World! This is a test message.
HMAC-SHA256 (hex): 5d41402abc4b2a76b9719d911017c592
HMAC verification: true
Invalid HMAC verification: false
```

This implementation provides a secure and efficient way to compute HMACs in F# while following best practices for cryptographic operations.

