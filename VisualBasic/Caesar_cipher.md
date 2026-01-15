# Caesar Cipher in Visual Basic

Here's a complete example of a Caesar cipher algorithm implemented in Visual Basic:

```vb
Module CaesarCipher
    Sub Main()
        ' Example usage
        Dim plaintext As String = "Hello World!"
        Dim shift As Integer = 3
        
        Console.WriteLine("Caesar Cipher Example")
        Console.WriteLine("====================")
        Console.WriteLine($"Original text: {plaintext}")
        Console.WriteLine($"Shift value: {shift}")
        
        ' Encrypt the text
        Dim encrypted As String = EncryptCaesar(plaintext, shift)
        Console.WriteLine($"Encrypted text: {encrypted}")
        
        ' Decrypt the text
        Dim decrypted As String = DecryptCaesar(encrypted, shift)
        Console.WriteLine($"Decrypted text: {decrypted}")
        
        Console.ReadLine()
    End Sub
    
    ' Encrypt function using Caesar cipher
    Function EncryptCaesar(text As String, shift As Integer) As String
        Dim result As String = ""
        
        For i As Integer = 0 To text.Length - 1
            Dim char As Char = text(i)
            
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
                ' Non-alphabetic characters remain unchanged
                result += char.ToString()
            End If
        Next
        
        Return result
    End Function
    
    ' Decrypt function using Caesar cipher
    Function DecryptCaesar(text As String, shift As Integer) As String
        Dim result As String = ""
        
        For i As Integer = 0 To text.Length - 1
            Dim char As Char = text(i)
            
            If Char.IsLetter(char) Then
                ' Determine if uppercase or lowercase
                Dim isUpper As Boolean = Char.IsUpper(char)
                
                ' Convert to lowercase for processing
                Dim lowerChar As Char = Char.ToLower(char)
                
                ' Apply reverse Caesar cipher shift
                Dim shiftedChar As Char = Chr((Asc(lowerChar) - Asc("a"c) - shift + 26) Mod 26 + Asc("a"c))
                
                ' Restore original case
                If isUpper Then
                    shiftedChar = Char.ToUpper(shiftedChar)
                End If
                
                result += shiftedChar.ToString()
            Else
                ' Non-alphabetic characters remain unchanged
                result += char.ToString()
            End If
        Next
        
        Return result
    End Function
End Module
```

## Output Example:
```
Caesar Cipher Example
====================
Original text: Hello World!
Shift value: 3
Encrypted text: Khoor Zruog!
Decrypted text: Hello World!
```

## How it works:

1. **Encryption**: Each letter is shifted forward by the specified number of positions in the alphabet
2. **Decryption**: Each letter is shifted backward by the specified number of positions
3. **Case preservation**: Uppercase and lowercase letters maintain their case
4. **Non-alphabetic characters**: Spaces, punctuation, and numbers remain unchanged
5. **Wrapping**: When shifting goes beyond 'z', it wraps around to 'a'

## Key Features:

- Handles both uppercase and lowercase letters
- Preserves case sensitivity
- Maintains non-alphabetic characters
- Uses modular arithmetic for wrapping
- Includes both encryption and decryption functions

The algorithm uses the formula: `(character_position + shift) Mod 26` for encryption and `(character_position - shift + 26) Mod 26` for decryption.

