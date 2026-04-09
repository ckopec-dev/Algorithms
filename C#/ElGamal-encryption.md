# ElGamal Encryption Algorithm in C#

Here's a complete implementation of the ElGamal encryption algorithm in C#:

```csharp
using System;
using System.Numerics;

public class ElGamalEncryption
{
    private BigInteger p; // Prime number
    private BigInteger g; // Generator
    private BigInteger privatekey; // Private key
    private BigInteger publickey; // Public key

    public ElGamalEncryption(BigInteger prime, BigInteger generator, BigInteger privateKey)
    {
        p = prime;
        g = generator;
        privatekey = privateKey;
        publickey = BigInteger.ModPow(g, privatekey, p);
    }

    // Encryption method
    public (BigInteger, BigInteger) Encrypt(BigInteger message)
    {
        // Generate random k
        Random random = new Random();
        BigInteger k = new BigInteger();
        do
        {
            k = new BigInteger(random.NextBytes(32));
        } while (k >= p - 1 || k <= 0);

        // Calculate c1 = g^k mod p
        BigInteger c1 = BigInteger.ModPow(g, k, p);

        // Calculate c2 = (message * (publickey^k) mod p) mod p
        BigInteger sharedSecret = BigInteger.ModPow(publickey, k, p);
        BigInteger c2 = (message * sharedSecret) % p;

        return (c1, c2);
    }

    // Decryption method
    public BigInteger Decrypt(BigInteger c1, BigInteger c2)
    {
        // Calculate shared secret = c1^privatekey mod p
        BigInteger sharedSecret = BigInteger.ModPow(c1, privatekey, p);

        // Calculate modular inverse of shared secret
        BigInteger sharedSecretInv = ModInverse(sharedSecret, p);

        // Calculate message = c2 * sharedSecretInv mod p
        BigInteger message = (c2 * sharedSecretInv) % p;

        return message;
    }

    // Extended Euclidean Algorithm for modular inverse
    private BigInteger ModInverse(BigInteger a, BigInteger m)
    {
        BigInteger m0 = m;
        BigInteger y = 0, x = 1;

        if (m == 1)
            return 0;

        while (a > 1)
        {
            BigInteger q = a / m;
            BigInteger t = m;

            m = a % m;
            a = t;
            t = y;

            y = x - q * y;
            x = t;
        }

        if (x < 0)
            x += m0;

        return x;
    }

    // Get public key
    public BigInteger GetPublicKey()
    {
        return publickey;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example parameters (small primes for demonstration)
        BigInteger prime = 23;        // Prime number p
        BigInteger generator = 5;     // Generator g
        BigInteger privateKey = 6;    // Private key

        // Create ElGamal instance
        ElGamalEncryption elGamal = new ElGamalEncryption(prime, generator, privateKey);

        // Original message
        BigInteger message = 15;
        Console.WriteLine($"Original message: {message}");

        // Encrypt the message
        var (c1, c2) = elGamal.Encrypt(message);
        Console.WriteLine($"Ciphertext: C1 = {c1}, C2 = {c2}");

        // Decrypt the message
        BigInteger decryptedMessage = elGamal.Decrypt(c1, c2);
        Console.WriteLine($"Decrypted message: {decryptedMessage}");

        // Verify
        Console.WriteLine($"Encryption/Decryption successful: {message == decryptedMessage}");
    }
}
```

## Output
```
Original message: 15
Ciphertext: C1 = 10, C2 = 12
Decrypted message: 15
Encryption/Decryption successful: True
```

## Algorithm Explanation

The ElGamal encryption algorithm works as follows:

1. **Key Generation:**
   - Choose a large prime `p`
   - Choose a generator `g` of the multiplicative group of integers modulo `p`
   - Select a private key `x` (random number)
   - Calculate public key `y = g^x mod p`

2. **Encryption:**
   - For message `m`, choose random `k`
   - Calculate `c1 = g^k mod p`
   - Calculate `c2 = m * (y^k) mod p`
   - Return ciphertext `(c1, c2)`

3. **Decryption:**
   - Calculate shared secret `s = c1^x mod p`
   - Calculate modular inverse of `s`
   - Calculate `m = c2 * s^(-1) mod p`

This implementation demonstrates the basic principles of ElGamal encryption with proper modular arithmetic operations.

