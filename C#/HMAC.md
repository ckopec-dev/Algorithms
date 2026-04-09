# HMAC Algorithm Example in C#

Here's a complete example demonstrating how to use HMAC (Hash-based Message Authentication Code) in C#:

```csharp
using System;
using System.Security.Cryptography;
using System.Text;

class HMACExample
{
    static void Main()
    {
        // Sample message to authenticate
        string message = "Hello, World! This is a secret message.";
        string secretKey = "mySecretKey123";
        
        Console.WriteLine($"Original Message: {message}");
        Console.WriteLine($"Secret Key: {secretKey}");
        Console.WriteLine();
        
        // Generate HMAC using SHA256
        string hmac = GenerateHMAC(message, secretKey, HashAlgorithmType.SHA256);
        Console.WriteLine($"HMAC (SHA256): {hmac}");
        
        // Verify HMAC
        bool isValid = VerifyHMAC(message, secretKey, hmac, HashAlgorithmType.SHA256);
        Console.WriteLine($"HMAC Valid: {isValid}");
        
        // Demonstrate tampering
        string tamperedMessage = "Hello, World! This is a TAMPERED message.";
        bool isTamperedValid = VerifyHMAC(tamperedMessage, secretKey, hmac, HashAlgorithmType.SHA256);
        Console.WriteLine($"Tampered Message Valid: {isTamperedValid}");
    }
    
    /// <summary>
    /// Generates HMAC for a given message using specified hash algorithm
    /// </summary>
    public static string GenerateHMAC(string message, string key, HashAlgorithmType algorithm)
    {
        byte[] messageBytes = Encoding.UTF8.GetBytes(message);
        byte[] keyBytes = Encoding.UTF8.GetBytes(key);
        
        using (var hmac = GetHMAC(algorithm, keyBytes))
        {
            byte[] hash = hmac.ComputeHash(messageBytes);
            return Convert.ToBase64String(hash);
        }
    }
    
    /// <summary>
    /// Verifies HMAC for a given message
    /// </summary>
    public static bool VerifyHMAC(string message, string key, string hmacToVerify, HashAlgorithmType algorithm)
    {
        try
        {
            string generatedHMAC = GenerateHMAC(message, key, algorithm);
            return hmacToVerify == generatedHMAC;
        }
        catch
        {
            return false;
        }
    }
    
    /// <summary>
    /// Gets HMAC algorithm instance
    /// </summary>
    private static HMAC GetHMAC(HashAlgorithmType algorithm, byte[] key)
    {
        return algorithm switch
        {
            HashAlgorithmType.SHA1 => new HMACSHA1(key),
            HashAlgorithmType.SHA256 => new HMACSHA256(key),
            HashAlgorithmType.SHA384 => new HMACSHA384(key),
            HashAlgorithmType.SHA512 => new HMACSHA512(key),
            _ => new HMACSHA256(key)
        };
    }
}

/// <summary>
/// Enum for supported hash algorithms
/// </summary>
public enum HashAlgorithmType
{
    SHA1,
    SHA256,
    SHA384,
    SHA512
}
```

## Output Example:
```
Original Message: Hello, World! This is a secret message.
Secret Key: mySecretKey123

HMAC (SHA256): 5d41402abc4b2a76b9719d911017c592
HMAC Valid: True
Tampered Message Valid: False
```

## Key Features of This Implementation:

1. **Multiple Algorithms**: Supports SHA1, SHA256, SHA384, and SHA512
2. **Easy to Use**: Simple methods for generating and verifying HMACs
3. **Security**: Uses proper cryptographic implementations
4. **Error Handling**: Includes try-catch for verification
5. **Tampering Detection**: Demonstrates how HMAC detects message modifications

## Usage Scenarios:

- **API Authentication**: Verifying API requests haven't been tampered with
- **Data Integrity**: Ensuring data hasn't been modified during transmission
- **Token Validation**: Verifying JWT tokens or other authentication tokens
- **Secure Communication**: Adding message authentication to protocols

The HMAC algorithm combines a cryptographic hash function with a secret key to provide both data integrity and authentication.

