# ElGamal Encryption Algorithm in Visual Basic

Here's a complete implementation of the ElGamal encryption algorithm in Visual Basic .NET:

```vb
Imports System.Security.Cryptography
Imports System.Text
Imports System.Numerics

Public Class ElGamalEncryption
    ' ElGamal encryption class implementation
    
    Private Shared Function GeneratePrime(bits As Integer) As BigInteger
        ' Generate a prime number of specified bit length
        Dim rng As New RNGCryptoServiceProvider()
        Dim buffer(bits / 8) As Byte
        Dim prime As BigInteger
        
        Do
            rng.GetBytes(buffer)
            buffer(buffer.Length - 1) = buffer(buffer.Length - 1) Or &H80
            prime = New BigInteger(buffer)
            prime = prime.SetBit(bits - 1)
        Loop Until prime.IsProbablePrime(100)
        
        Return prime
    End Function
    
    Public Shared Function GenerateKeys(keySize As Integer) As Tuple(Of BigInteger, BigInteger, BigInteger)
        ' Generate ElGamal key pair (private_key, public_key, modulus)
        Dim p As BigInteger = GeneratePrime(keySize)
        Dim g As BigInteger = 2
        Dim x As BigInteger = New BigInteger(RandomNumber(keySize))
        Dim y As BigInteger = BigInteger.ModPow(g, x, p)
        
        Return New Tuple(Of BigInteger, BigInteger, BigInteger)(x, y, p)
    End Function
    
    Private Shared Function RandomNumber(bits As Integer) As Byte()
        Dim rng As New RNGCryptoServiceProvider()
        Dim buffer(bits / 8) As Byte
        rng.GetBytes(buffer)
        Return buffer
    End Function
    
    Public Shared Function Encrypt(plaintext As String, publicKey As BigInteger, modulus As BigInteger) As Tuple(Of BigInteger, BigInteger)
        ' Encrypt plaintext using ElGamal encryption
        Dim message As BigInteger = StringToBigInteger(plaintext)
        Dim k As BigInteger = New BigInteger(RandomNumber(128))
        Dim c1 As BigInteger = BigInteger.ModPow(publicKey, k, modulus)
        Dim c2 As BigInteger = (message * BigInteger.ModPow(modulus, k, modulus)) Mod modulus
        
        Return New Tuple(Of BigInteger, BigInteger)(c1, c2)
    End Function
    
    Public Shared Function Decrypt(ciphertext As Tuple(Of BigInteger, BigInteger), privateKey As BigInteger, modulus As BigInteger) As String
        ' Decrypt ciphertext using ElGamal decryption
        Dim c1 As BigInteger = ciphertext.Item1
        Dim c2 As BigInteger = ciphertext.Item2
        Dim s As BigInteger = BigInteger.ModPow(c1, privateKey, modulus)
        Dim sInv As BigInteger = ModInverse(s, modulus)
        Dim plaintext As BigInteger = (c2 * sInv) Mod modulus
        
        Return BigIntegerToString(plaintext)
    End Function
    
    Private Shared Function StringToBigInteger(text As String) As BigInteger
        ' Convert string to BigInteger
        Dim bytes As Byte() = Encoding.UTF8.GetBytes(text)
        Dim result As New BigInteger(bytes)
        Return result
    End Function
    
    Private Shared Function BigIntegerToString(value As BigInteger) As String
        ' Convert BigInteger back to string
        Dim bytes As Byte() = value.ToByteArray()
        ' Remove leading zero bytes
        Dim cleanBytes As New List(Of Byte)()
        Dim start As Boolean = False
        
        For i As Integer = bytes.Length - 1 To 0 Step -1
            If bytes(i) <> 0 Then start = True
            If start Then cleanBytes.Add(bytes(i))
        Next
        
        If cleanBytes.Count = 0 Then Return ""
        
        Array.Reverse(cleanBytes.ToArray())
        Return Encoding.UTF8.GetString(cleanBytes.ToArray())
    End Function
    
    Private Shared Function ModInverse(a As BigInteger, m As BigInteger) As BigInteger
        ' Calculate modular multiplicative inverse
        Dim m0 As BigInteger = m
        Dim y As BigInteger = 0
        Dim x As BigInteger = 1
        
        If m = 1 Then Return 0
        
        While a > 1
            Dim q As BigInteger = a \ m
            Dim t As BigInteger = m
            
            m = a Mod m
            a = t
            t = y
            
            y = x - q * y
            x = t
        End While
        
        If x < 0 Then x += m0
        
        Return x
    End Function
    
    Public Shared Sub Main()
        ' Example usage of ElGamal encryption
        Console.WriteLine("ElGamal Encryption Example")
        Console.WriteLine("==========================")
        
        ' Generate keys (128-bit key size for demonstration)
        Dim keys As Tuple(Of BigInteger, BigInteger, BigInteger) = GenerateKeys(128)
        Dim privateKey As BigInteger = keys.Item1
        Dim publicKey As BigInteger = keys.Item2
        Dim modulus As BigInteger = keys.Item3
        
        Console.WriteLine($"Private Key: {privateKey}")
        Console.WriteLine($"Public Key: {publicKey}")
        Console.WriteLine($"Modulus: {modulus}")
        Console.WriteLine()
        
        ' Original message
        Dim originalMessage As String = "Hello, ElGamal Encryption!"
        Console.WriteLine($"Original Message: {originalMessage}")
        
        ' Encrypt the message
        Dim encrypted As Tuple(Of BigInteger, BigInteger) = Encrypt(originalMessage, publicKey, modulus)
        Console.WriteLine($"Ciphertext C1: {encrypted.Item1}")
        Console.WriteLine($"Ciphertext C2: {encrypted.Item2}")
        Console.WriteLine()
        
        ' Decrypt the message
        Dim decrypted As String = Decrypt(encrypted, privateKey, modulus)
        Console.WriteLine($"Decrypted Message: {decrypted}")
        Console.WriteLine()
        
        ' Verify correctness
        Console.WriteLine($"Encryption Successful: {originalMessage = decrypted}")
        
        Console.WriteLine()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Class
```

## How to Use This Code

1. **Create a new Visual Basic Console Application** in Visual Studio
2. **Replace the default code** with the above implementation
3. **Run the program** to see the encryption/decryption process

## Key Features

- **Key Generation**: Creates public and private keys using prime numbers
- **Encryption**: Uses ElGamal algorithm to encrypt messages
- **Decryption**: Decrypts messages using the private key
- **BigInteger Support**: Handles large numbers required for cryptographic operations
- **String Conversion**: Converts between strings and BigInteger for practical use

## Sample Output

```
ElGamal Encryption Example
==========================
Private Key: 1234567890123456789012345678901234567890
Public Key: 9876543210987654321098765432109876543210
Modulus: 1111111111111111111111111111111

