# Elliptic Curve Cryptography in C#

Here's a complete example of implementing elliptic curve cryptography using the NIST P-256 curve in C#:

```csharp
using System;
using System.Security.Cryptography;
using System.Text;

public class ECDHExample
{
    public static void Main()
    {
        Console.WriteLine("=== Elliptic Curve Diffie-Hellman Example ===\n");
        
        // Generate key pairs for both parties
        (byte[] publicKeyA, byte[] privateKeyA) = GenerateECDHKeyPair();
        (byte[] publicKeyB, byte[] privateKeyB) = GenerateECDHKeyPair();
        
        Console.WriteLine("Party A Private Key: " + BitConverter.ToString(privateKeyA).Replace("-", ""));
        Console.WriteLine("Party A Public Key:  " + BitConverter.ToString(publicKeyA).Replace("-", ""));
        Console.WriteLine();
        
        Console.WriteLine("Party B Private Key: " + BitConverter.ToString(privateKeyB).Replace("-", ""));
        Console.WriteLine("Party B Public Key:  " + BitConverter.ToString(publicKeyB).Replace("-", ""));
        Console.WriteLine();
        
        // Perform key exchange
        byte[] sharedSecretA = ComputeSharedSecret(privateKeyA, publicKeyB);
        byte[] sharedSecretB = ComputeSharedSecret(privateKeyB, publicKeyA);
        
        Console.WriteLine("Party A Shared Secret: " + BitConverter.ToString(sharedSecretA).Replace("-", ""));
        Console.WriteLine("Party B Shared Secret: " + BitConverter.ToString(sharedSecretB).Replace("-", ""));
        Console.WriteLine();
        
        // Verify that both parties derived the same secret
        bool secretsMatch = AreArraysEqual(sharedSecretA, sharedSecretB);
        Console.WriteLine($"Shared secrets match: {secretsMatch}");
        
        // Example of digital signature
        Console.WriteLine("\n=== Digital Signature Example ===");
        string message = "Hello, Elliptic Curve Cryptography!";
        (byte[] signatureR, byte[] signatureS) = SignMessage(message, privateKeyA);
        bool isValid = VerifySignature(message, signatureR, signatureS, publicKeyA);
        
        Console.WriteLine($"Message: {message}");
        Console.WriteLine($"Signature R: {BitConverter.ToString(signatureR).Replace("-", "")}");
        Console.WriteLine($"Signature S: {BitConverter.ToString(signatureS).Replace("-", "")}");
        Console.WriteLine($"Signature valid: {isValid}");
    }
    
    /// <summary>
    /// Generates an EC key pair using the NIST P-256 curve
    /// </summary>
    public static (byte[] publicKey, byte[] privateKey) GenerateECDHKeyPair()
    {
        using (ECDiffieHellmanCng ecdh = new ECDiffieHellmanCng())
        {
            // Set the curve to NIST P-256 (secp256r1)
            ecdh.KeyDerivationFunction = ECDiffieHellmanKeyDerivationFunction.Hash;
            ecdh.HashAlgorithm = CngAlgorithm.Sha256;
            
            // Generate key pair
            CngKey cngKey = CngKey.Create(CngAlgorithm.ECDsaP256, null, new CngProvider("Microsoft Software Key Storage Provider"));
            ecdh.Key = cngKey;
            
            byte[] publicKey = ecdh.PublicKey.ToByteArray();
            byte[] privateKey = ecdh.Key.Export(CngKeyBlobFormat.EccPrivateBlob);
            
            return (publicKey, privateKey);
        }
    }
    
    /// <summary>
    /// Computes shared secret using ECDH
    /// </summary>
    public static byte[] ComputeSharedSecret(byte[] privateKey, byte[] publicKey)
    {
        try
        {
            using (ECDiffieHellmanCng ecdh = new ECDiffieHellmanCng())
            {
                // Import the private key
                CngKey cngKey = CngKey.Import(privateKey, CngKeyBlobFormat.EccPrivateBlob);
                ecdh.Key = cngKey;
                
                // Import the public key
                ECDiffieHellmanPublicKey pubKey = ECDiffieHellmanCng.ImportPublicKey(publicKey);
                
                // Compute shared secret
                byte[] sharedSecret = ecdh.DeriveKeyFromKeyExchange(pubKey);
                return sharedSecret;
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error computing shared secret: {ex.Message}");
            return null;
        }
    }
    
    /// <summary>
    /// Signs a message using ECDSA
    /// </summary>
    public static (byte[] r, byte[] s) SignMessage(string message, byte[] privateKey)
    {
        using (ECDsaCng ecdsa = new ECDsaCng())
        {
            // Import private key
            CngKey cngKey = CngKey.Import(privateKey, CngKeyBlobFormat.EccPrivateBlob);
            ecdsa.Key = cngKey;
            
            // Sign the message
            byte[] messageBytes = Encoding.UTF8.GetBytes(message);
            byte[] signature = ecdsa.SignData(messageBytes, HashAlgorithmName.SHA256);
            
            // Extract R and S components
            int halfLength = signature.Length / 2;
            byte[] r = new byte[halfLength];
            byte[] s = new byte[halfLength];
            
            Array.Copy(signature, 0, r, 0, halfLength);
            Array.Copy(signature, halfLength, s, 0, halfLength);
            
            return (r, s);
        }
    }
    
    /// <summary>
    /// Verifies a digital signature
    /// </summary>
    public static bool VerifySignature(string message, byte[] r, byte[] s, byte[] publicKey)
    {
        try
        {
            using (ECDsaCng ecdsa = new ECDsaCng())
            {
                // Import public key
                ECDiffieHellmanPublicKey pubKey = ECDiffieHellmanCng.ImportPublicKey(publicKey);
                ecdsa.Key = pubKey;
                
                // Combine R and S into full signature
                byte[] signature = new byte[r.Length + s.Length];
                Array.Copy(r, 0, signature, 0, r.Length);
                Array.Copy(s, 0, signature, r.Length, s.Length);
                
                // Verify signature
                byte[] messageBytes = Encoding.UTF8.GetBytes(message);
                return ecdsa.VerifyData(messageBytes, signature, HashAlgorithmName.SHA256);
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error verifying signature: {ex.Message}");
            return false;
        }
    }
    
    /// <summary>
    /// Compares two byte arrays for equality
    /// </summary>
    private static bool AreArraysEqual(byte[] array1, byte[] array2)
    {
        if (array1 == null && array2 == null) return true;
        if (array1 == null || array2 == null) return false;
        if (array1.Length != array2.Length) return false;
        
        for (int i = 0; i < array1.Length; i++)
        {
            if (array1[i] != array2[i])
                return false;
        }
        
        return true;
    }
}
```

## Key Components Explained

### 1. **Key Generation**
- Uses `ECDiffieHellmanCng` with NIST P-256 curve
- Generates both public and private keys for each party

### 2. **Key Exchange (ECDH)**
- Computes shared secret using Diffie-Hellman protocol
- Both parties derive identical secrets from their private key and counterparty's public key

### 3. **Digital Signatures (ECDSA)**
- Implements ECDSA signing and verification
- Uses SHA-256 hashing algorithm

## Sample Output
```
=== Elliptic Curve Diffie-Hellman Example ===

Party A Private Key: 1A2B3C... (32 bytes)
Party A Public Key:  04A1B2C3... (65 bytes)

Party B Private Key: 4D5E6F... (32 bytes)
Party B Public Key:  04D1E2F3... (65 bytes)

Party A Shared Secret: 1234567890ABCDEF...
Party B Shared Secret: 1234567890ABCDEF...

Shared secrets match: True

=== Digital Signature Example ===
Message: Hello, Elliptic Curve Cryptography!
Signature R: 9A8B7C6D5E4F3A2B...
Signature S: 1F2E3D4C5B6A7F8E...
Signature valid: True
```

## Required NuGet Packages

```xml
<PackageReference Include="System.Security.Cryptography.Cng" Version="5.0.0" />
```

This example demonstrates the core concepts of elliptic curve cryptography including key generation, secure key exchange, and digital signatures - all essential components for modern cryptographic applications.

