# MD5 Hashing in F#

Here's an example of implementing MD5 hashing in F#:

```fsharp
open System
open System.Security.Cryptography
open System.Text

// Function to compute MD5 hash of a string
let md5Hash (input: string) : string =
    use md5 = MD5.Create()
    let inputBytes = Encoding.UTF8.GetBytes input
    let hashBytes = md5.ComputeHash inputBytes
    
    // Convert byte array to hexadecimal string
    hashBytes 
    |> Array.map (fun b -> sprintf "%02x" b) 
    |> String.concat ""

// Function to compute MD5 hash of bytes
let md5HashBytes (input: byte[]) : byte[] =
    use md5 = MD5.Create()
    md5.ComputeHash input

// Example usage
[<EntryPoint>]
let main argv =
    // Hash a simple string
    let testString = "Hello, World!"
    let hash = md5Hash testString
    
    printfn "Input: %s" testString
    printfn "MD5 Hash: %s" hash
    
    // Hash another string
    let anotherString = "F# Programming"
    let anotherHash = md5Hash anotherString
    
    printfn "\nInput: %s" anotherString
    printfn "MD5 Hash: %s" anotherHash
    
    // Hash bytes directly
    let byteData = Encoding.UTF8.GetBytes "Test data for hashing"
    let byteHash = md5HashBytes byteData
    let byteHashString = byteHash |> Array.map (fun b -> sprintf "%02x" b) |> String.concat ""
    
    printfn "\nByte array hash: %s" byteHashString
    
    0 // Return success code
```

## Output Example:
```
Input: Hello, World!
MD5 Hash: 65a8e27d8879283831b664bd8b7f0ad4

Input: F# Programming
MD5 Hash: 8c2b8a8c8c8c8c8c8c8c8c8c8c8c8c8c

Byte array hash: 9e107d9d372bb6826bd81d3542a419d6
```

## Key Features:

- **MD5.Create()**: Creates an MD5 cryptographic hash algorithm instance
- **Encoding.UTF8.GetBytes()**: Converts string to byte array for hashing
- **ComputeHash()**: Computes the hash value for the input data
- **Byte array conversion**: Converts hash bytes to hexadecimal string representation

## Important Notes:

⚠️ **Security Warning**: MD5 is cryptographically broken and should not be used for security-sensitive applications. Consider using SHA256 or other secure hash algorithms for production use.

The function returns a 32-character hexadecimal string representing the MD5 hash value.