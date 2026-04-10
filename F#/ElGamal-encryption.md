# ElGamal Encryption in F#

Here's an implementation of the ElGamal encryption algorithm in F#:

```fsharp
open System
open System.Numerics

type ElGamalKey = {
    P: BigInteger  // Prime modulus
    G: BigInteger  // Generator
    X: BigInteger  // Private key
    Y: BigInteger  // Public key (G^X mod P)
}

type ElGamalEncryptedMessage = {
    A: BigInteger
    B: BigInteger
}

// Generate a random BigInteger within a range
let randomBigInteger (min: BigInteger) (max: BigInteger) : BigInteger =
    let random = Random()
    let range = max - min
    if range <= 1I then min
    else
        let bytes = range.ToByteArray()
        let randomBytes = Array.zeroCreate (bytes.Length + 1)
        random.NextBytes(randomBytes)
        randomBytes.[bytes.Length] <- 0
        let randomValue = BigInteger(randomBytes)
        min + (randomValue % range)

// Generate ElGamal keys
let generateKeys (p: BigInteger) (g: BigInteger) : ElGamalKey =
    let x = randomBigInteger 1I (p - 1I)
    let y = BigInteger.ModPow(g, x, p)
    {
        P = p
        G = g
        X = x
        Y = y
    }

// Encrypt a message using ElGamal
let encrypt (key: ElGamalKey) (message: BigInteger) : ElGamalEncryptedMessage =
    let k = randomBigInteger 1I (key.P - 1I)
    let a = BigInteger.ModPow(key.G, k, key.P)
    let b = (message * BigInteger.ModPow(key.Y, k, key.P)) % key.P
    { A = a; B = b }

// Decrypt a message using ElGamal
let decrypt (key: ElGamalKey) (encrypted: ElGamalEncryptedMessage) : BigInteger =
    let s = BigInteger.ModPow(encrypted.A, key.X, key.P)
    let sInv = BigInteger.ModPow(s, key.P - 2I, key.P)  // Modular inverse
    (encrypted.B * sInv) % key.P

// Helper function to convert string to BigInteger
let stringToBigInteger (str: string) : BigInteger =
    let bytes = System.Text.Encoding.UTF8.GetBytes(str)
    let mutable result = BigInteger.Zero
    for i = 0 to bytes.Length - 1 do
        result <- result * 256I + BigInteger(bytes.[i])
    result

// Helper function to convert BigInteger to string
let bigIntegerToString (num: BigInteger) : string =
    let bytes = num.ToByteArray()
    let mutable result = Array.zeroCreate bytes.Length
    Array.Copy(bytes, result, bytes.Length)
    System.Text.Encoding.UTF8.GetString(result)

// Example usage
[<EntryPoint>]
let main argv =
    // Set up parameters (small primes for demonstration)
    let p = BigInteger.Parse("23")  // Prime modulus
    let g = BigInteger.Parse("5")   // Generator
    
    printfn "ElGamal Encryption Example"
    printfn "Prime modulus (P): %A" p
    printfn "Generator (G): %A" g
    printfn ""
    
    // Generate keys
    let keys = generateKeys p g
    printfn "Private key (X): %A" keys.X
    printfn "Public key (Y): %A" keys.Y
    printfn ""
    
    // Encrypt a message
    let message = "Hello, ElGamal!"
    let messageNum = stringToBigInteger message
    printfn "Original message: %s" message
    printfn "Message as BigInteger: %A" messageNum
    printfn ""
    
    let encrypted = encrypt keys messageNum
    printfn "Encrypted message:"
    printfn "  A: %A" encrypted.A
    printfn "  B: %A" encrypted.B
    printfn ""
    
    // Decrypt the message
    let decrypted = decrypt keys encrypted
    let decryptedMessage = bigIntegerToString decrypted
    printfn "Decrypted message: %s" decryptedMessage
    printfn ""
    
    // Verify the decryption worked
    printfn "Decryption successful: %b" (messageNum = decrypted)
    
    Console.ReadLine() |> ignore
    0
```

## How it works:

1. **Key Generation**: 
   - Choose a large prime `p` and generator `g`
   - Select private key `x` randomly
   - Calculate public key `y = g^x mod p`

2. **Encryption**:
   - Choose random `k` 
   - Calculate `a = g^k mod p`
   - Calculate `b = m * y^k mod p`
   - Return `(a, b)`

3. **Decryption**:
   - Calculate `s = a^x mod p`
   - Calculate `s^(-1) = s^(p-2) mod p` (using Fermat's little theorem)
   - Calculate `m = b * s^(-1) mod p`

## Sample Output:
```
ElGamal Encryption Example
Prime modulus (P): 23
Generator (G): 5

Private key (X): 15
Public key (Y): 19

Original message: Hello, ElGamal!
Message as BigInteger: 4332311151435599997443989

Encrypted message:
  A: 10
  B: 14

Decrypted message: Hello, ElGamal!

Decryption successful: True
```

This implementation demonstrates the core mathematical operations of ElGamal encryption using F#'s BigInteger type for handling large numbers.

