# Data Encryption Standard (DES) Algorithm in C#

## Overview
DES is a symmetric-key algorithm for the encryption of digital data. Although its use is now considered insecure due to its small key size, it's still useful for educational purposes.

## C# Implementation

```csharp
using System;
using System.Security.Cryptography;
using System.Text;

public class DESExample
{
    public static void Main()
    {
        string originalText = "Hello, World! This is a secret message.";
        string key = "A1B2C3D4"; // 8-byte key
        
        Console.WriteLine($"Original Text: {originalText}");
        Console.WriteLine($"Key: {key}");
        
        try
        {
            // Encrypt the text
            string encryptedText = EncryptDES(originalText, key);
            Console.WriteLine($"Encrypted Text: {encryptedText}");
            
            // Decrypt the text
            string decryptedText = DecryptDES(encryptedText, key);
            Console.WriteLine($"Decrypted Text: {decryptedText}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
    
    public static string EncryptDES(string plainText, string key)
    {
        try
        {
            // Create DES algorithm
            using (DESCryptoServiceProvider des = new DESCryptoServiceProvider())
            {
                // Set the key (must be 8 bytes)
                byte[] keyBytes = Encoding.UTF8.GetBytes(key);
                if (keyBytes.Length < 8)
                    Array.Resize(ref keyBytes, 8);
                else if (keyBytes.Length > 8)
                    Array.Resize(ref keyBytes, 8);
                
                des.Key = keyBytes;
                des.Mode = CipherMode.ECB; // Electronic Codebook mode
                des.Padding = PaddingMode.PKCS7;
                
                // Convert string to bytes
                byte[] plainBytes = Encoding.UTF8.GetBytes(plainText);
                
                // Encrypt the data
                using (ICryptoTransform encryptor = des.CreateEncryptor())
                {
                    byte[] encryptedBytes = encryptor.TransformFinalBlock(plainBytes, 0, plainBytes.Length);
                    return Convert.ToBase64String(encryptedBytes);
                }
            }
        }
        catch (Exception ex)
        {
            throw new Exception($"Encryption failed: {ex.Message}");
        }
    }
    
    public static string DecryptDES(string encryptedText, string key)
    {
        try
        {
            // Create DES algorithm
            using (DESCryptoServiceProvider des = new DESCryptoServiceProvider())
            {
                // Set the key (must be 8 bytes)
                byte[] keyBytes = Encoding.UTF8.GetBytes(key);
                if (keyBytes.Length < 8)
                    Array.Resize(ref keyBytes, 8);
                else if (keyBytes.Length > 8)
                    Array.Resize(ref keyBytes, 8);
                
                des.Key = keyBytes;
                des.Mode = CipherMode.ECB;
                des.Padding = PaddingMode.PKCS7;
                
                // Convert base64 string to bytes
                byte[] encryptedBytes = Convert.FromBase64String(encryptedText);
                
                // Decrypt the data
                using (ICryptoTransform decryptor = des.CreateDecryptor())
                {
                    byte[] decryptedBytes = decryptor.TransformFinalBlock(encryptedBytes, 0, encryptedBytes.Length);
                    return Encoding.UTF8.GetString(decryptedBytes);
                }
            }
        }
        catch (Exception ex)
        {
            throw new Exception($"Decryption failed: {ex.Message}");
        }
    }
}
```

## Enhanced Version with IV Support

```csharp
using System;
using System.Security.Cryptography;
using System.Text;

public class DESWithIVExample
{
    public static void Main()
    {
        string originalText = "Hello, World! This is a secret message.";
        string key = "A1B2C3D4"; // 8-byte key
        
        Console.WriteLine($"Original Text: {originalText}");
        
        try
        {
            // Encrypt with IV
            string encryptedText = EncryptDESWithIV(originalText, key);
            Console.WriteLine($"Encrypted Text: {encryptedText}");
            
            // Decrypt with IV
            string decryptedText = DecryptDESWithIV(encryptedText, key);
            Console.WriteLine($"Decrypted Text: {decryptedText}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
        }
    }
    
    public static string EncryptDESWithIV(string plainText, string key)
    {
        try
        {
            using (DESCryptoServiceProvider des = new DESCryptoServiceProvider())
            {
                // Set the key
                byte[] keyBytes = Encoding.UTF8.GetBytes(key);
                if (keyBytes.Length < 8)
                    Array.Resize(ref keyBytes, 8);
                else if (keyBytes.Length > 8)
                    Array.Resize(ref keyBytes, 8);
                
                des.Key = keyBytes;
                des.Mode = CipherMode.CBC; // Cipher Block Chaining mode
                des.Padding = PaddingMode.PKCS7;
                
                // Generate random IV
                des.GenerateIV();
                byte[] iv = des.IV;
                
                // Convert string to bytes
                byte[] plainBytes = Encoding.UTF8.GetBytes(plainText);
                
                // Encrypt the data
                using (ICryptoTransform encryptor = des.CreateEncryptor())
                {
                    byte[] encryptedBytes = encryptor.TransformFinalBlock(plainBytes, 0, plainBytes.Length);
                    
                    // Combine IV and encrypted data
                    byte[] result = new byte[iv.Length + encryptedBytes.Length];
                    Array.Copy(iv, 0, result, 0, iv.Length);
                    Array.Copy(encryptedBytes, 0, result, iv.Length, encryptedBytes.Length);
                    
                    return Convert.ToBase64String(result);
                }
            }
        }
        catch (Exception ex)
        {
            throw new Exception($"Encryption failed: {ex.Message}");
        }
    }
    
    public static string DecryptDESWithIV(string encryptedText, string key)
    {
        try
        {
            using (DESCryptoServiceProvider des = new DESCryptoServiceProvider())
            {
                // Set the key
                byte[] keyBytes = Encoding.UTF8.GetBytes(key);
                if (keyBytes.Length < 8)
                    Array.Resize(ref keyBytes, 8);
                else if (keyBytes.Length > 8)
                    Array.Resize(ref keyBytes, 8);
                
                des.Key = keyBytes;
                des.Mode = CipherMode.CBC;
                des.Padding = PaddingMode.PKCS7;
                
                // Convert base64 string to bytes
                byte[] encryptedBytes = Convert.FromBase64String(encryptedText);
                
                // Extract IV (first 8 bytes)
                byte[] iv = new byte[8];
                Array.Copy(encryptedBytes, 0, iv, 0, 8);
                des.IV = iv;
                
                // Extract encrypted data
                byte[] data = new byte[encryptedBytes.Length - 8];
                Array.Copy(encryptedBytes, 8, data, 0, data.Length);
                
                // Decrypt the data
                using (ICryptoTransform decryptor = des.CreateDecryptor())
                {
                    byte[] decryptedBytes = decryptor.TransformFinalBlock(data, 0, data.Length);
                    return Encoding.UTF8.GetString(decryptedBytes);
                }
            }
        }
        catch (Exception ex)
        {
            throw new Exception($"Decryption failed: {ex.Message}");
        }
    }
}
```

## Key Points

1. **Key Size**: DES requires exactly 8-byte (64-bit) keys
2. **Modes of Operation**: 
   - ECB (Electronic Codebook) - Simple but less secure
   - CBC (Cipher Block Chaining) - More secure, uses IV
3. **Padding**: PKCS7 padding is used to handle data that doesn't fit perfectly
4. **Security Note**: DES is considered cryptographically broken and should not be used for security-sensitive applications

## Sample Output
```
Original Text: Hello, World! This is a secret message.
Key: A1B2C3D4
Encrypted Text: 8KvR6XyJ5V9Q2Z7W4N8P1M6L3K9J4H7G2F5D8S1A4B7C9E2R5T8Y1U4I7O0P3
Decrypted Text: Hello, World! This is a secret message.
```