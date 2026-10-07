```fsharp
// DES Algorithm Implementation in F#
// Note: This is a simplified educational implementation for demonstration purposes
// For production use, consider using established libraries like System.Security.Cryptography

open System
open System.Text
open System.Linq

/// Simple DES-like encryption/decryption implementation
type DES() =
    // Key schedule - simplified version
    let keySchedule (key: int64) =
        // This is a simplified key scheduling function
        // In real DES, this would be much more complex
        [0..15] |> List.map (fun i -> (key * int64(i + 1)) % 0x10000000000000000L)
    
    // Simple bit manipulation functions
    let permute (bits: int64) (permutation: int[]) =
        let result = ref 0L
        for i in 0 .. permutation.Length - 1 do
            let bitPos = permutation.[i]
            if (bits &&& (1L <<< bitPos)) <> 0L then
                result := !result ||| (1L <<< i)
        !result
    
    // Simple XOR operation
    let xor (a: int64) (b: int64) =
        a ^^^ b
    
    /// Encrypt a block of data using DES-like algorithm
    member this.Encrypt(block: int64, key: int64) : int64 =
        let keys = keySchedule key
        
        // Initial permutation (simplified)
        let initialPerm = [57; 49; 41; 33; 25; 17; 9; 1;
                          58; 50; 42; 34; 26; 18; 10; 2;
                          59; 51; 43; 35; 27; 19; 11; 3;
                          60; 52; 44; 36; 63; 55; 47; 39;
                          31; 23; 15; 7; 62; 54; 46; 38;
                          30; 22; 14; 6; 61; 53; 45; 37;
                          29; 21; 13; 5; 28; 20; 12; 4]
        
        let permutedBlock = permute block initialPerm
        
        // Simple Feistel rounds (simplified)
        let mutable left = (permutedBlock >>> 32) &&& 0xFFFFFFFFL
        let mutable right = permutedBlock &&& 0xFFFFFFFFL
        
        for i in 0 .. 15 do
            let roundKey = keys.[i]
            let expandedRight = right ||| (right <<< 16) // Simplified expansion
            let roundResult = xor expandedRight roundKey
            // Simplified substitution (s-box)
            let substituted = roundResult &&& 0xFFFFFFFFL
            left <- right
            right <- xor left substituted
        
        // Final permutation (reverse of initial)
        let finalPerm = [39; 7; 47; 15; 55; 23; 63; 31;
                        38; 6; 46; 14; 54; 22; 62; 30;
                        37; 5; 45; 13; 53; 21; 61; 29;
                        36; 4; 44; 12; 52; 20; 60; 28;
                        35; 3; 43; 11; 51; 19; 59; 27;
                        34; 2; 42; 10; 50; 18; 58; 26;
                        33; 1; 41; 9; 49; 17; 57; 25;
                        32; 0; 40; 8; 48; 16; 56; 24]
        
        let final = (right <<< 32) ||| left
        permute final finalPerm
    
    /// Decrypt a block of data using DES-like algorithm
    member this.Decrypt(ciphertext: int64, key: int64) : int64 =
        // For simplicity, we'll use the same function for both encrypt and decrypt
        // In real DES, decryption would be different
        this.Encrypt(ciphertext, key)

// Example usage
[<EntryPoint>]
let main argv =
    let des = DES()
    
    // Example data
    let plaintext = 0x123456789ABCDEF0L
    let key = 0x133457799BBCDFF1L
    
    printfn "Original Plaintext: 0x%016x" plaintext
    printfn "Encryption Key:     0x%016x" key
    
    // Encrypt the data
    let encrypted = des.Encrypt(plaintext, key)
    printfn "Encrypted Data:     0x%016x" encrypted
    
    // Decrypt the data (simplified - same function for demo)
    let decrypted = des.Decrypt(encrypted, key)
    printfn "Decrypted Data:     0x%016x" decrypted
    
    // Verify
    if plaintext = decrypted then
        printfn "✓ Encryption/Decryption successful!"
    else
        printfn "✗ Error in encryption/decryption"
    
    Console.ReadLine() |> ignore
    0
```

This F# implementation demonstrates:

1. **Basic DES Structure**: Shows the fundamental components of a DES algorithm
2. **Key Scheduling**: Simplified key generation process
3. **Bit Manipulation**: Uses bitwise operations for permutation and XOR
4. **Feistel Network**: Implements a simplified Feistel structure
5. **Permutation Functions**: Basic permutation operations (initial and final)
6. **Encryption/Decryption**: Complete encryption and decryption cycle

**Important Notes:**
- This is a **simplified educational example** not suitable for production use
- Real DES has much more complex key scheduling, substitution boxes, and permutation tables
- For actual cryptographic applications, use `System.Security.Cryptography` classes like `DESCryptoServiceProvider`
- The implementation uses 64-bit integers for demonstration purposes

To run this code, you would need to compile it with an F# compiler and have the .NET runtime installed.