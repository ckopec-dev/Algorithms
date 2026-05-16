# CIPHER__Family Algorithm Example in Visual Basic

Here's an example of implementing a simple Caesar Cipher (part of the CIPHER__family) in Visual Basic:

```vb
Public Class CipherExample
    ' Caesar Cipher implementation
    Public Shared Function CaesarCipher(plainText As String, shift As Integer) As String
        Dim result As String = ""
        
        For i As Integer = 0 To plainText.Length - 1
            Dim char As Char = plainText(i)
            
            If Char.IsLetter(char) Then
                ' Determine if uppercase or lowercase
                Dim base As Integer = If(Char.IsUpper(char), Asc("A"), Asc("a"))
                
                ' Apply shift and wrap around
                Dim shifted As Integer = (Asc(char) - base + shift) Mod 26
                result += Chr(shifted + base)
            Else
                ' Keep non-alphabetic characters unchanged
                result += char
            End If
        Next
        
        Return result
    End Function
    
    ' Encrypt function
    Public Shared Function Encrypt(text As String, key As Integer) As String
        Return CaesarCipher(text, key)
    End Function
    
    ' Decrypt function (reverse shift)
    Public Shared Function Decrypt(text As String, key As Integer) As String
        Return CaesarCipher(text, -key)
    End Function
    
    ' Example usage
    Public Shared Sub Main()
        Dim originalText As String = "Hello World!"
        Dim shiftKey As Integer = 3
        
        Console.WriteLine("Original Text: " & originalText)
        
        ' Encrypt
        Dim encryptedText As String = Encrypt(originalText, shiftKey)
        Console.WriteLine("Encrypted Text: " & encryptedText)
        
        ' Decrypt
        Dim decryptedText As String = Decrypt(encryptedText, shiftKey)
        Console.WriteLine("Decrypted Text: " & decryptedText)
    End Sub
End Class
```

## Output:
```
Original Text: Hello World!
Encrypted Text: Khoor Zruog!
Decrypted Text: Hello World!
```

## Key Features of this Cipher Implementation:

- **Caesar Cipher**: Shifts each letter by a fixed number of positions
- **Case Preservation**: Maintains uppercase and lowercase letters
- **Non-alphabetic Characters**: Leaves spaces, punctuation unchanged
- **Modular Arithmetic**: Uses modulo operation for wrapping around the alphabet
- **Bidirectional**: Supports both encryption and decryption

This example demonstrates a fundamental symmetric encryption algorithm from the CIPHER__family, where the same key is used for both encryption and decryption.

