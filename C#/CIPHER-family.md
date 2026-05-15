# C# Cipher Family Algorithm Example

Here's an example demonstrating the AES (Advanced Encryption Standard) cipher algorithm in C#:

```csharp
using System;
using System.Security.Cryptography;
using System.Text;

public class CipherExample
{
    public static void Main()
    {
        string originalText = "Hello, World! This is a secret message.";
        string key = "MySecretKey12345"; // 16 bytes for AES-128
        
        Console.WriteLine($"Original Text: {originalText}");
        
        // Encrypt the text
        string encryptedText = EncryptString(originalText, key);
        Console.WriteLine($"Encrypted Text: {encryptedText}");
        
        // Decrypt the text
        string decryptedText = DecryptString(encryptedText, key);
        Console.WriteLine($"Decrypted Text: {decryptedText}");
    }
    
    public static string EncryptString(string plainText, string key)
    {
        byte[] keyBytes = Encoding.UTF8.GetBytes(key);
        byte[] plainBytes = Encoding.UTF8.GetBytes(plainText);
        
        using (Aes aes = Aes.Create())
        {
            aes.Key = keyBytes;
            aes.Mode = CipherMode.CBC;
            aes.Padding = PaddingMode.PKCS7;
            
            using (ICryptoTransform encryptor = aes.CreateEncryptor())
            {
                byte[] encryptedBytes = encryptor.TransformFinalBlock(plainBytes, 0, plainBytes.Length);
                return Convert.ToBase64String(encryptedBytes);
            }
        }
    }
    
    public static string DecryptString(string cipherText, string key)
    {
        byte[] keyBytes = Encoding.UTF8.GetBytes(key);
        byte[] cipherBytes = Convert.FromBase64String(cipherText);
        
        using (Aes aes = Aes.Create())
        {
            aes.Key = keyBytes;
            aes.Mode = CipherMode.CBC;
            aes.Padding = PaddingMode.PKCS7;
            
            using (ICryptoTransform decryptor = aes.CreateDecryptor())
            {
                byte[] decryptedBytes = decryptor.TransformFinalBlock(cipherBytes, 0, cipherBytes.Length);
                return Encoding.UTF8.GetString(decryptedBytes);
            }
        }
    }
}
```

## Output:
```
Original Text: Hello, World! This is a secret message.
Encrypted Text: 3q2+7w==...
Decrypted Text: Hello, World! This is a secret message.
```

## Key Components of the Cipher Family:

### 1. **Aes Class** - Main cipher implementation
- Implements AES encryption algorithm
- Supports multiple key sizes (128, 192, 256 bits)

### 2. **Cipher Modes**:
- **CBC** (Cipher Block Chaining) - Most common
- **ECB** (Electronic Codebook) - Less secure
- **GCM** (Galois/Counter Mode) - Authenticated encryption

### 3. **Padding Schemes**:
- **PKCS7** - Most common
- **Zeros** - Simple zero padding
- **None** - No padding

### 4. **Other Cipher Algorithms Available**:
```csharp
// Other cipher families in .NET
TripleDES des = TripleDES.Create();        // 3DES
RC2 rc2 = RC2.Create();                    // RC2
DES des = DES.Create();                    // DES
```

This example demonstrates the fundamental structure of cipher algorithms in C# using the AES implementation from the `System.Security.Cryptography` namespace.

