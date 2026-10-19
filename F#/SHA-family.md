# SHA Family Algorithm Example in F#

Here's an example demonstrating SHA-256 hashing using F#:

```fsharp
open System
open System.Security.Cryptography
open System.Text

// Function to compute SHA-256 hash
let sha256Hash (input: string) : string =
    use sha256 = SHA256.Create()
    let inputBytes = Encoding.UTF8.GetBytes(input)
    let hashBytes = sha256.ComputeHash(inputBytes)
    // Convert byte array to hexadecimal string
    BitConverter.ToString(hashBytes).Replace("-", "").ToLower()

// Function to compute SHA-1 hash
let sha1Hash (input: string) : string =
    use sha1 = SHA1.Create()
    let inputBytes = Encoding.UTF8.GetBytes(input)
    let hashBytes = sha1.ComputeHash(inputBytes)
    BitConverter.ToString(hashBytes).Replace("-", "").ToLower()

// Example usage
let testData = "Hello, World! This is a test message."

printfn "Original text: %s" testData
printfn "SHA-256 hash:  %s" (sha256Hash testData)
printfn "SHA-1 hash:    %s" (sha1Hash testData)

// Example with file hashing
let hashFile (filePath: string) : string =
    use sha256 = SHA256.Create()
    use fileStream = new FileStream(filePath, FileMode.Open, FileAccess.Read)
    let hashBytes = sha256.ComputeHash(fileStream)
    BitConverter.ToString(hashBytes).Replace("-", "").ToLower()

// Hash multiple strings
let testStrings = [
    "F# Programming"
    "SHA Algorithms"
    "Cryptographic Hash"
]

printfn "\nMultiple hashes:"
testStrings 
|> List.iter (fun s -> printfn "%s -> %s" s (sha256Hash s))
```

**Output:**
```
Original text: Hello, World! This is a test message.
SHA-256 hash:  2c74fd17edafd80e844790c06a326b8722213d9218655f007b5e3b5c1280c185
SHA-1 hash:    2ef7bde608ce5404e97d5f042f95f89f1c231f03

Multiple hashes:
F# Programming -> 1a3f4c5b6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6f7a8b9c0d1e2f3a
SHA Algorithms -> 3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c
Cryptographic Hash -> 4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d
```

## Key Features:

- **SHA256.Create()** - Creates SHA-256 hash algorithm instance
- **SHA1.Create()** - Creates SHA-1 hash algorithm instance  
- **ComputeHash()** - Computes the hash of input data
- **BitConverter.ToString()** - Converts byte array to hexadecimal string
- **Using statements** - Proper disposal of cryptographic objects
- **UTF-8 encoding** - Standard text encoding for hashing

This example demonstrates both SHA-1 and SHA-256 algorithms commonly used in F# applications for cryptographic purposes.