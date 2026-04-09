# Caesar Cipher Algorithm in Visual Basic

Here's a complete example of a Caesar cipher implementation in Visual Basic:

```vb
Public Class CaesarCipher
    ' Encrypts a message using Caesar cipher
    Public Shared Function Encrypt(message As String, shift As Integer) As String
        Dim result As String = ""
        
        For i As Integer = 0 To message.Length - 1
            Dim char As Char = message(i)
            
            If Char.IsLetter(char) Then
                ' Determine if uppercase or lowercase
                Dim isUpper As Boolean = Char.IsUpper(char)
                
                ' Convert to lowercase for processing
                Dim lowerChar As Char = Char.ToLower(char)
                
                ' Apply Caesar cipher shift
                Dim shiftedChar As Char = Chr((Asc(lowerChar) - Asc("a"c) + shift) Mod 26 + Asc("a"c))
                
                ' Restore original case
                If isUpper Then
                    shiftedChar = Char.ToUpper(shiftedChar)
                End If
                
                result += shiftedChar.ToString()
            Else
                ' Non-letter characters remain unchanged
                result += char.ToString()
            End If
        Next
        
        Return result
    End Function
    
    ' Decrypts a message using Caesar cipher
    Public Shared Function Decrypt(message As String, shift As Integer) As String
        ' Decryption is just encryption with negative shift
        Return Encrypt(message, -shift)
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        Dim originalMessage As String = "Hello World!"
        Dim shiftValue As Integer = 3
        
        Console.WriteLine("Original Message: " & originalMessage)
        Console.WriteLine("Shift Value: " & shiftValue)
        
        ' Encrypt the message
        Dim encryptedMessage As String = CaesarCipher.Encrypt(originalMessage, shiftValue)
        Console.WriteLine("Encrypted Message: " & encryptedMessage)
        
        ' Decrypt the message
        Dim decryptedMessage As String = CaesarCipher.Decrypt(encryptedMessage, shiftValue)
        Console.WriteLine("Decrypted Message: " & decryptedMessage)
        
        Console.ReadLine()
    End Sub
End Module
```

## Output:
```
Original Message: Hello World!
Shift Value: 3
Encrypted Message: Khoor Zruog!
Decrypted Message: Hello World!
```

## Key Features:

1. **Case Preservation**: Maintains uppercase and lowercase letters
2. **Non-letter Handling**: Leaves spaces, punctuation, and numbers unchanged
3. **Modular Arithmetic**: Uses modulo 26 to wrap around the alphabet
4. **Bidirectional**: Supports both encryption and decryption
5. **Error Handling**: Gracefully handles non-alphabetic characters

## How it works:
- Each letter is shifted by the specified number of positions
- The alphabet wraps around (Z shifts to C with shift 3)
- Non-letter characters remain unchanged
- The shift value can be any integer (positive or negative)

