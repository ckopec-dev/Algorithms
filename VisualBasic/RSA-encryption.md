# RSA Encryption in Visual Basic

Here's a complete example of RSA encryption implementation in Visual Basic .NET:

```vb
Imports System.Security.Cryptography
Imports System.Text
Imports System.Numerics

Public Class RSAEncryption
    ' RSA Key Structure
    Private Class RSAKey
        Public Property Modulus As BigInteger
        Public Property Exponent As BigInteger
        
        Public Sub New(modulus As BigInteger, exponent As BigInteger)
            Me.Modulus = modulus
            Me.Exponent = exponent
        End Sub
    End Class

    ' Generate RSA key pair
    Public Shared Function GenerateKeys(keySize As Integer) As (Private As RSAKey, Public As RSAKey)
        Dim rsa As New RSACryptoServiceProvider(keySize)
        
        ' Get private key
        Dim privateKey As RSAKey = New RSAKey(
            BigInteger.Parse(rsa.ExportParameters(True).Modulus.ToString()),
            BigInteger.Parse(rsa.ExportParameters(True).Exponent.ToString())
        )
        
        ' Get public key
        Dim publicKey As RSAKey = New RSAKey(
            BigInteger.Parse(rsa.ExportParameters(False).Modulus.ToString()),
            BigInteger.Parse(rsa.ExportParameters(False).Exponent.ToString())
        )
        
        Return (privateKey, publicKey)
    End Function

    ' Encrypt message using RSA
    Public Shared Function Encrypt(message As String, publicKey As RSAKey) As String
        Dim messageBytes As Byte() = Encoding.UTF8.GetBytes(message)
        Dim messageBigInt As BigInteger = New BigInteger(messageBytes)
        
        ' Encrypt: c = m^e mod n
        Dim encryptedBigInt As BigInteger = BigInteger.ModPow(messageBigInt, publicKey.Exponent, publicKey.Modulus)
        
        Return encryptedBigInt.ToString()
    End Function

    ' Decrypt message using RSA
    Public Shared Function Decrypt(encryptedMessage As String, privateKey As RSAKey) As String
        Dim encryptedBigInt As BigInteger = BigInteger.Parse(encryptedMessage)
        
        ' Decrypt: m = c^d mod n
        Dim decryptedBigInt As BigInteger = BigInteger.ModPow(encryptedBigInt, privateKey.Exponent, privateKey.Modulus)
        
        Dim decryptedBytes As Byte() = decryptedBigInt.ToByteArray()
        Array.Reverse(decryptedBytes) ' BigInteger stores bytes in reverse order
        
        Return Encoding.UTF8.GetString(decryptedBytes)
    End Function

    ' Simple RSA encryption using built-in .NET classes
    Public Shared Function SimpleEncrypt(message As String, publicKeyXml As String) As String
        Dim rsa As New RSACryptoServiceProvider()
        rsa.FromXmlString(publicKeyXml)
        
        Dim messageBytes As Byte() = Encoding.UTF8.GetBytes(message)
        Dim encryptedBytes As Byte() = rsa.Encrypt(messageBytes, True)
        
        Return Convert.ToBase64String(encryptedBytes)
    End Function

    ' Simple RSA decryption using built-in .NET classes
    Public Shared Function SimpleDecrypt(encryptedMessage As String, privateKeyXml As String) As String
        Dim rsa As New RSACryptoServiceProvider()
        rsa.FromXmlString(privateKeyXml)
        
        Dim encryptedBytes As Byte() = Convert.FromBase64String(encryptedMessage)
        Dim decryptedBytes As Byte() = rsa.Decrypt(encryptedBytes, True)
        
        Return Encoding.UTF8.GetString(decryptedBytes)
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        Console.WriteLine("RSA Encryption Example")
        Console.WriteLine("======================")
        
        Try
            ' Generate RSA keys
            Dim keys As (Private As RSAEncryption.RSAKey, Public As RSAEncryption.RSAKey) = 
                RSAEncryption.GenerateKeys(1024)
            
            Dim message As String = "Hello, RSA Encryption!"
            Console.WriteLine($"Original message: {message}")
            
            ' Encrypt the message
            Dim encrypted As String = RSAEncryption.Encrypt(message, keys.Public)
            Console.WriteLine($"Encrypted: {encrypted}")
            
            ' Decrypt the message
            Dim decrypted As String = RSAEncryption.Decrypt(encrypted, keys.Private)
            Console.WriteLine($"Decrypted: {decrypted}")
            
            Console.WriteLine()
            Console.WriteLine("Using built-in .NET classes:")
            
            ' Using built-in .NET RSA classes
            Dim rsa As New RSACryptoServiceProvider(1024)
            Dim publicKeyXml As String = rsa.ToXmlString(False)
            Dim privateKeyXml As String = rsa.ToXmlString(True)
            
            Dim simpleEncrypted As String = RSAEncryption.SimpleEncrypt(message, publicKeyXml)
            Console.WriteLine($"Simple encrypted: {simpleEncrypted}")
            
            Dim simpleDecrypted As String = RSAEncryption.SimpleDecrypt(simpleEncrypted, privateKeyXml)
            Console.WriteLine($"Simple decrypted: {simpleDecrypted}")
            
        Catch ex As Exception
            Console.WriteLine($"Error: {ex.Message}")
        End Try
        
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Custom RSA Implementation**: Shows manual RSA encryption/decryption using BigInteger
2. **Key Generation**: Creates both public and private key pairs
3. **Encryption/Decryption Functions**: 
   - Manual RSA using BigInteger operations
   - Built-in .NET RSA classes for comparison
4. **Error Handling**: Includes try-catch blocks for robust operation
5. **String Handling**: Proper encoding/decoding of text messages

## How RSA Works in This Example:

1. **Key Generation**: Creates a 1024-bit RSA key pair
2. **Encryption**: `c = m^e mod n` where `m` is the message, `e` is public exponent, `n` is modulus
3. **Decryption**: `m = c^d mod n` where `d` is private exponent

## Usage Notes:

- This is a simplified example for educational purposes
- For production use, consider using established libraries like Bouncy Castle
- RSA is typically used for encrypting small amounts of data (like session keys)
- The example uses 1024-bit keys, but 2048-bit or higher are recommended for security

The code demonstrates both manual implementation and use of built-in .NET cryptographic classes for comparison.

