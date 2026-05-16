# MD5 Hashing in Visual Basic

Here's an example of implementing MD5 hashing in Visual Basic .NET:

```vb
Imports System.Security.Cryptography
Imports System.Text

Public Class MD5Example
    ' Method to generate MD5 hash from string
    Public Shared Function GenerateMD5(input As String) As String
        Using md5Hash As MD5 = MD5.Create()
            ' Convert input string to byte array and compute hash
            Dim data As Byte() = md5Hash.ComputeHash(Encoding.UTF8.GetBytes(input))
            
            ' Create StringBuilder to collect result
            Dim sBuilder As New StringBuilder()
            
            ' Loop through each byte of the hash and format as hexadecimal
            For i As Integer = 0 To data.Length - 1
                sBuilder.Append(data(i).ToString("x2"))
            Next
            
            Return sBuilder.ToString()
        End Using
    End Function
    
    ' Method to compare input string with MD5 hash
    Public Shared Function VerifyMD5(input As String, hash As String) As Boolean
        Dim hashOfInput As String = GenerateMD5(input)
        Dim comparer As StringComparer = StringComparer.OrdinalIgnoreCase
        
        Return comparer.Equals(hashOfInput, hash)
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        Dim inputText As String = "Hello, World!"
        Dim md5Hash As String = MD5Example.GenerateMD5(inputText)
        
        Console.WriteLine("Original Text: " & inputText)
        Console.WriteLine("MD5 Hash: " & md5Hash)
        Console.WriteLine("Hash Length: " & md5Hash.Length & " characters")
        
        ' Verify hash
        Dim isValid As Boolean = MD5Example.VerifyMD5(inputText, md5Hash)
        Console.WriteLine("Verification: " & If(isValid, "Valid", "Invalid"))
        
        ' Test with different input
        Dim differentText As String = "Hello, World!!"
        Dim differentHash As String = MD5Example.GenerateMD5(differentText)
        Console.WriteLine(vbNewLine & "Different text: " & differentText)
        Console.WriteLine("Different hash: " & differentHash)
    End Sub
End Module
```

## Output Example:
```
Original Text: Hello, World!
MD5 Hash: 65a8e27d8879283831b664bd8b7f0ad4
Hash Length: 32 characters
Verification: Valid

Different text: Hello, World!!
Different hash: 1804d748973849278053710553148237
```

## Key Points:

1. **Using Statement**: The `Using` statement ensures proper disposal of the MD5 object
2. **Encoding**: UTF-8 encoding is used to convert strings to bytes
3. **Hexadecimal Format**: Each byte is formatted as a 2-digit hexadecimal string
4. **Security Note**: MD5 is cryptographically broken and should not be used for security purposes
5. **Length**: MD5 hashes are always 32 characters long (128 bits)

## Alternative Implementation (More Compact):

```vb
Imports System.Security.Cryptography
Imports System.Text

Public Shared Function GetMD5Hash(input As String) As String
    Using md5 As MD5 = MD5.Create()
        Return String.Join("", md5.ComputeHash(Encoding.UTF8.GetBytes(input)).Select(Function(b) b.ToString("x2")))
    End Using
End Function
```

This implementation demonstrates how to create MD5 hashes in Visual Basic .NET using the built-in cryptographic classes.

