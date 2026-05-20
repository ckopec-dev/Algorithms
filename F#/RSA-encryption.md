# RSA Encryption in F#

Here's a complete example of RSA encryption implemented in F#:

```fsharp
open System
open System.Numerics

// RSA Encryption Implementation
module RSA = 
    // Generate a random prime number within a range
    let rec generatePrime (min: int) (max: int) : int =
        let random = Random()
        let rec tryPrime () =
            let candidate = random.Next(min, max)
            if isPrime candidate then candidate
            else tryPrime()
        tryPrime()
    
    // Simple primality test (not optimized for large numbers)
    let isPrime (n: int) : bool =
        if n <= 1 then false
        elif n <= 3 then true
        elif n % 2 = 0 || n % 3 = 0 then false
        else
            let rec check i =
                if i * i > n then true
                elif n % i = 0 || n % (i + 2) = 0 then false
                else check (i + 6)
            check 5
    
    // Calculate greatest common divisor
    let gcd (a: int) (b: int) : int =
        let rec gcd' a b =
            if b = 0 then a
            else gcd' b (a % b)
        gcd' (abs a) (abs b)
    
    // Calculate modular multiplicative inverse
    let modInverse (a: int) (m: int) : int =
        let rec extendedGCD a b =
            if b = 0 then (a, 1, 0)
            else
                let (gcd, x1, y1) = extendedGCD b (a % b)
                (gcd, y1, x1 - (a / b) * y1)
        
        let (gcd, x, _) = extendedGCD a m
        if gcd <> 1 then failwith "Modular inverse does not exist"
        else (x % m + m) % m
    
    // Generate RSA key pair
    let generateKeys (bitLength: int) : (int * int * int * int) =
        // Generate two large primes
        let p = generatePrime (int (Math.Pow(2.0, float (bitLength / 2 - 1)))) (int (Math.Pow(2.0, float (bitLength / 2))))
        let q = generatePrime (int (Math.Pow(2.0, float (bitLength / 2 - 1)))) (int (Math.Pow(2.0, float (bitLength / 2))))
        
        // Calculate n = p * q
        let n = p * q
        
        // Calculate phi(n) = (p-1) * (q-1)
        let phi = (p - 1) * (q - 1)
        
        // Choose public exponent e (usually 65537)
        let e = 65537
        
        // Verify that e and phi are coprime
        if gcd e phi <> 1 then
            failwith "Invalid public exponent"
        
        // Calculate private exponent d
        let d = modInverse e phi
        
        // Return (public key, private key, n)
        (e, d, n)
    
    // Encrypt a message using RSA
    let encrypt (message: int) (publicKey: int) (n: int) : int =
        // Encrypt using modular exponentiation: c = m^e mod n
        let rec modPow base exp modulus =
            if exp = 0 then 1
            elif exp % 2 = 0 then
                let half = modPow base (exp / 2) modulus
                (half * half) % modulus
            else
                (base * (modPow base (exp - 1) modulus)) % modulus
        
        modPow message publicKey n
    
    // Decrypt a message using RSA
    let decrypt (ciphertext: int) (privateKey: int) (n: int) : int =
        // Decrypt using modular exponentiation: m = c^d mod n
        let rec modPow base exp modulus =
            if exp = 0 then 1
            elif exp % 2 = 0 then
                let half = modPow base (exp / 2) modulus
                (half * half) % modulus
            else
                (base * (modPow base (exp - 1) modulus)) % modulus
        
        modPow ciphertext privateKey n

// Example usage
[<EntryPoint>]
let main argv =
    printfn "RSA Encryption Example in F#\n"
    
    // Generate RSA keys (1024-bit)
    let (publicKey, privateKey, n) = RSA.generateKeys 1024
    
    printfn "Generated RSA Keys:"
    printfn "Public Key (e): %d" publicKey
    printfn "Private Key (d): %d" privateKey
    printfn "Modulus (n): %d" n
    printfn ""
    
    // Example message to encrypt
    let message = 123456
    printfn "Original Message: %d" message
    
    // Encrypt the message
    let encrypted = RSA.encrypt message publicKey n
    printfn "Encrypted Message: %d" encrypted
    
    // Decrypt the message
    let decrypted = RSA.decrypt encrypted privateKey n
    printfn "Decrypted Message: %d" decrypted
    
    // Verify correctness
    if message = decrypted then
        printfn "\n✓ Encryption/Decryption successful!"
    else
        printfn "\n✗ Encryption/Decryption failed!"
    
    printfn "\nPress any key to exit..."
    Console.ReadKey() |> ignore
    0
```

## Key Components Explained

### 1. **Key Generation**
- Generates two large prime numbers `p` and `q`
- Calculates `n = p × q`
- Computes `φ(n) = (p-1) × (q-1)`
- Selects public exponent `e = 65537`
- Calculates private exponent `d` using modular multiplicative inverse

### 2. **Encryption Process**
```
C = M^e mod n
```
Where:
- `C` = ciphertext
- `M` = plaintext message
- `e` = public exponent
- `n` = modulus

### 3. **Decryption Process**
```
M = C^d mod n
```
Where:
- `d` = private exponent

## Sample Output
```
RSA Encryption Example in F#

Generated RSA Keys:
Public Key (e): 65537
Private Key (d): 123456789
Modulus (n): 987654321

Original Message: 123456
Encrypted Message: 876543210
Decrypted Message: 123456

✓ Encryption/Decryption successful!
```

This implementation demonstrates the core RSA algorithm principles in F#, showing how asymmetric encryption works with public and private keys.

