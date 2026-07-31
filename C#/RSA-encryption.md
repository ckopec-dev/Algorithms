# RSA Encryption Algorithm in C#

Here's a complete example of RSA encryption using C# with the `System.Security.Cryptography` namespace:

```csharp
using System;
using System.Security.Cryptography;
using System.Text;

public class RSAExample
{
    public static void Main()
    {
        // Generate RSA key pair
        using (RSA rsa = RSA.Create())
        {
            // Generate keys
            rsa.KeySize = 2048; // Use 2048-bit key for better security
            
            // Get public and private keys
            string publicKey = rsa.ToXmlString(false);  // Public key only
            string privateKey = rsa.ToXmlString(true);  // Private key including private parameters
            
            Console.WriteLine("Public Key:");
            Console.WriteLine(publicKey);
            Console.WriteLine("\nPrivate Key:");
            Console.WriteLine(privateKey);
            
            // Original message
            string originalMessage = "Hello, this is a secret message!";
            Console.WriteLine($"\nOriginal Message: {originalMessage}");
            
            // Encrypt the message
            byte[] encryptedData = EncryptString(originalMessage, publicKey);
            Console.WriteLine($"\nEncrypted Data (Base64): {Convert.ToBase64String(encryptedData)}");
            
            // Decrypt the message
            string decryptedMessage = DecryptString(encryptedData, privateKey);
            Console.WriteLine($"Decrypted Message: {decryptedMessage}");
        }
    }
    
    /// <summary>
    /// Encrypts a string using RSA public key
    /// </summary>
    public static byte[] EncryptString(string plainText, string publicKey)
    {
        using (RSA rsa = RSA.Create())
        {
            // Import the public key
            rsa.FromXmlString(publicKey);
            
            // Convert string to bytes
            byte[] plainBytes = Encoding.UTF8.GetBytes(plainText);
            
            // Encrypt the data
            byte[] encryptedBytes = rsa.Encrypt(plainBytes, RSAEncryptionPadding.OaepSHA256);
            
            return encryptedBytes;
        }
    }
    
    /// <summary>
    /// Decrypts a byte array using RSA private key
    /// </summary>
    public static string DecryptString(byte[] encryptedData, string privateKey)
    {
        using (RSA rsa = RSA.Create())
        {
            // Import the private key
            rsa.FromXmlString(privateKey);
            
            // Decrypt the data
            byte[] decryptedBytes = rsa.Decrypt(encryptedData, RSAEncryptionPadding.OaepSHA256);
            
            // Convert bytes back to string
            return Encoding.UTF8.GetString(decryptedBytes);
        }
    }
}
```

## Key Features of This Implementation:

1. **Key Generation**: Automatically generates 2048-bit RSA key pair
2. **Public/Private Key Separation**: Shows how to export keys separately
3. **OAEP Padding**: Uses SHA-256 padding for secure encryption
4. **UTF-8 Encoding**: Properly handles text encoding/decoding
5. **Resource Management**: Uses `using` statements for proper disposal

## Sample Output:
```
Public Key:
<RSAKeyValue>...</RSAKeyValue>

Private Key:
<RSAKeyValue>...</RSAKeyValue>

Original Message: Hello, this is a secret message!

Encrypted Data (Base64): 3gD... (long base64 string)

Decrypted Message: Hello, this is a secret message!
```

## Important Security Notes:

- **Key Size**: Uses 2048-bit keys for security (minimum recommended)
- **Padding**: OAEP padding with SHA-256 for better security
- **Private Key Storage**: Never store private keys in plain text
- **Key Management**: In production, use secure key management systems

This example demonstrates the basic RSA encryption/decryption process in C# using built-in .NET cryptographic classes.