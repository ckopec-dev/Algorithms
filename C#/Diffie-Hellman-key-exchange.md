# Diffie-Hellman Key Exchange in C#

```csharp
using System;
using System.Security.Cryptography;

public class DiffieHellmanExample
{
    public static void Main()
    {
        Console.WriteLine("Diffie-Hellman Key Exchange Example");
        Console.WriteLine("==================================");
        
        // Generate DH parameters (prime and generator)
        using (var dh = ECDiffieHellman.Create())
        {
            // For demonstration, we'll use standard parameters
            // In practice, you'd want to use secure parameters
            var parameters = dh.ExportParameters(false);
            
            Console.WriteLine($"Prime (P): {BitConverter.ToString(parameters.Q).Replace("-", "")}");
            Console.WriteLine($"Generator (G): {BitConverter.ToString(parameters.G).Replace("-", "")}");
            
            // Alice generates her key pair
            var alicePrivateKey = ECDiffieHellman.Create();
            var alicePublicKey = alicePrivateKey.ExportPublicKey();
            
            Console.WriteLine($"\nAlice's Public Key: {BitConverter.ToString(alicePublicKey).Replace("-", "")}");
            
            // Bob generates his key pair
            var bobPrivateKey = ECDiffieHellman.Create();
            var bobPublicKey = bobPrivateKey.ExportPublicKey();
            
            Console.WriteLine($"Bob's Public Key: {BitConverter.ToString(bobPublicKey).Replace("-", "")}");
            
            // Both parties compute the shared secret
            byte[] aliceSharedSecret = alicePrivateKey.ComputeKeyExchange(bobPublicKey);
            byte[] bobSharedSecret = bobPrivateKey.ComputeKeyExchange(alicePublicKey);
            
            Console.WriteLine($"\nAlice's Shared Secret: {BitConverter.ToString(aliceSharedSecret).Replace("-", "")}");
            Console.WriteLine($"Bob's Shared Secret: {BitConverter.ToString(bobSharedSecret).Replace("-", "")}");
            
            // Verify that both secrets are equal
            bool secretsMatch = CompareArrays(aliceSharedSecret, bobSharedSecret);
            Console.WriteLine($"\nShared secrets match: {secretsMatch}");
        }
    }
    
    // Helper method to compare byte arrays
    private static bool CompareArrays(byte[] array1, byte[] array2)
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
```

## Alternative Implementation with Manual DH Parameters

```csharp
using System;
using System.Numerics;

public class ManualDiffieHellman
{
    // Example prime number (should be large in practice)
    private static readonly BigInteger P = BigInteger.Parse("17");
    // Example generator
    private static readonly BigInteger G = BigInteger.Parse("3");
    
    public static void Main()
    {
        Console.WriteLine("Manual Diffie-Hellman Key Exchange");
        Console.WriteLine("==================================");
        
        // Alice's private key (random number)
        BigInteger alicePrivateKey = 4;
        Console.WriteLine($"Alice's Private Key: {alicePrivateKey}");
        
        // Bob's private key (random number)
        BigInteger bobPrivateKey = 3;
        Console.WriteLine($"Bob's Private Key: {bobPrivateKey}");
        
        // Alice computes public key: G^a mod P
        BigInteger alicePublicKey = BigInteger.ModPow(G, alicePrivateKey, P);
        Console.WriteLine($"Alice's Public Key: {alicePublicKey}");
        
        // Bob computes public key: G^b mod P
        BigInteger bobPublicKey = BigInteger.ModPow(G, bobPrivateKey, P);
        Console.WriteLine($"Bob's Public Key: {bobPublicKey}");
        
        // Alice computes shared secret: B^a mod P
        BigInteger aliceSharedSecret = BigInteger.ModPow(bobPublicKey, alicePrivateKey, P);
        Console.WriteLine($"Alice's Shared Secret: {aliceSharedSecret}");
        
        // Bob computes shared secret: A^b mod P
        BigInteger bobSharedSecret = BigInteger.ModPow(alicePublicKey, bobPrivateKey, P);
        Console.WriteLine($"Bob's Shared Secret: {bobSharedSecret}");
        
        // Verify they're the same
        Console.WriteLine($"Shared secrets match: {aliceSharedSecret == bobSharedSecret}");
    }
}
```

## Key Points:

1. **Security**: The first example uses `ECDiffieHellman` which is cryptographically secure
2. **Parameters**: In practice, use large primes (at least 2048 bits) and secure generators
3. **Randomness**: Private keys should be generated using cryptographically secure random number generators
4. **Verification**: Both parties should arrive at the same shared secret
5. **Applications**: Commonly used for key exchange in TLS, SSH, and other secure protocols

The Diffie-Hellman algorithm allows two parties to establish a shared secret over an insecure channel, which can then be used for symmetric encryption.

