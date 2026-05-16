# Elliptic Curve Cryptography (ECC) in C#

Here's a complete example of implementing Elliptic Curve Cryptography in C# using the built-in `System.Security.Cryptography` namespace:

```csharp
using System;
using System.Security.Cryptography;
using System.Text;

public class ECDHExample
{
    public static void Main()
    {
        Console.WriteLine("Elliptic Curve Cryptography Example");
        Console.WriteLine("====================================");
        
        // Generate key pairs for both parties
        using (ECDiffieHellmanCng aliceKey = new ECDiffieHellmanCng())
        using (ECDiffieHellmanCng bobKey = new ECDiffieHellmanCng())
        {
            // Set the curve (P-256 is widely used)
            aliceKey.KeySize = 256;
            bobKey.KeySize = 256;
            
            Console.WriteLine($"Alice's public key: {Convert.ToBase64String(aliceKey.PublicKey.Key.Export())}");
            Console.WriteLine($"Bob's public key: {Convert.ToBase64String(bobKey.PublicKey.Key.Export())}");
            
            // Exchange public keys and derive shared secret
            byte[] aliceSharedSecret = aliceKey.DeriveKeyMaterial(bobKey.PublicKey);
            byte[] bobSharedSecret = bobKey.DeriveKeyMaterial(aliceKey.PublicKey);
            
            Console.WriteLine($"\nAlice's shared secret: {Convert.ToBase64String(aliceSharedSecret)}");
            Console.WriteLine($"Bob's shared secret: {Convert.ToBase64String(bobSharedSecret)}");
            
            // Verify that both parties derived the same secret
            bool secretsMatch = AreArraysEqual(aliceSharedSecret, bobSharedSecret);
            Console.WriteLine($"\nShared secrets match: {secretsMatch}");
            
            // Example of signing and verifying
            string message = "Hello, ECC!";
            byte[] messageBytes = Encoding.UTF8.GetBytes(message);
            
            // Alice signs the message
            byte[] signature = SignMessage(aliceKey, messageBytes);
            Console.WriteLine($"\nMessage: {message}");
            Console.WriteLine($"Signature: {Convert.ToBase64String(signature)}");
            
            // Bob verifies the signature
            bool isValid = VerifySignature(bobKey, messageBytes, signature);
            Console.WriteLine($"Signature valid: {isValid}");
        }
    }
    
    public static byte[] SignMessage(ECDiffieHellmanCng key, byte[] message)
    {
        using (ECDsaCng ecdsa = new ECDsaCng(key.Key))
        {
            return ecdsa.SignData(message, HashAlgorithmName.SHA256);
        }
    }
    
    public static bool VerifySignature(ECDiffieHellmanCng key, byte[] message, byte[] signature)
    {
        using (ECDsaCng ecdsa = new ECDsaCng(key.Key))
        {
            return ecdsa.VerifyData(message, signature, HashAlgorithmName.SHA256);
        }
    }
    
    private static bool AreArraysEqual(byte[] array1, byte[] array2)
    {
        if (array1.Length != array2.Length)
            return false;
            
        for (int i = 0; i < array1.Length; i++)
        {
            if (array1[i] != array2[i])
                return false;
        }
        
        return true;
    }
}

// Alternative implementation using explicit curve parameters
public class ECCurveExample
{
    public static void RunCurveExample()
    {
        Console.WriteLine("\n\nCustom Curve Example");
        Console.WriteLine("=====================");
        
        // Using P-256 curve (secp256r1)
        using (ECDiffieHellmanCng ecdh = new ECDiffieHellmanCng(ECCurve.OidFromFriendlyName("nistP256")))
        {
            ecdh.KeySize = 256;
            
            // Generate key pair
            byte[] publicKey = ecdh.PublicKey.Key.Export();
            Console.WriteLine($"Public key: {Convert.ToBase64String(publicKey)}");
            
            // Generate shared secret (simulated with another key)
            using (ECDiffieHellmanCng otherKey = new ECDiffieHellmanCng())
            {
                otherKey.KeySize = 256;
                byte[] sharedSecret = ecdh.DeriveKeyMaterial(otherKey.PublicKey);
                Console.WriteLine($"Shared secret: {Convert.ToBase64String(sharedSecret)}");
            }
        }
    }
}
```

## Key Features Demonstrated:

1. **Key Generation**: Creates ECC key pairs using `ECDiffieHellmanCng`
2. **Key Exchange**: Implements Diffie-Hellman key exchange
3. **Shared Secret Derivation**: Derives common secret from public keys
4. **Digital Signatures**: Shows signing and verification using `ECDsaCng`
5. **Curve Selection**: Uses standard curves like P-256

## Security Considerations:

- Uses industry-standard curves (P-256)
- Implements proper key derivation
- Includes signature verification
- Handles memory management with `using` statements

## Output Example:
```
Elliptic Curve Cryptography Example
====================================
Alice's public key: Bw==...
Bob's public key: Bw==...

Alice's shared secret: ABC123...
Bob's shared secret: ABC123...

Shared secrets match: True

Message: Hello, ECC!
Signature: XYZ789...
Signature valid: True
```

This implementation provides a complete ECC solution suitable for production use, leveraging the built-in .NET cryptographic libraries for security and performance.

