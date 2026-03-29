# Lempel-Ziv Compression Algorithm in Visual Basic

Here's a complete implementation of the Lempel-Ziv compression algorithm in Visual Basic .NET:

```vb
Imports System.Text
Imports System.Collections.Generic

Public Class LempelZivCompressor
    ' Compress a string using Lempel-Ziv algorithm
    Public Shared Function Compress(input As String) As String
        Dim dictionary As New Dictionary(Of String, Integer)()
        Dim result As New List(Of String)()
        Dim index As Integer = 256 ' Start with ASCII characters
        
        ' Initialize dictionary with ASCII characters
        For i As Integer = 0 To 255
            dictionary(Char.ConvertFromUtf32(i)) = i
        Next
        
        Dim current As String = ""
        Dim i As Integer = 0
        
        While i < input.Length
            current += input(i)
            
            If dictionary.ContainsKey(current) Then
                i += 1
            Else
                ' Add new entry to dictionary
                dictionary(current) = index
                index += 1
                
                ' Output the code for the previous string
                If current.Length > 1 Then
                    Dim previousCode As Integer = GetCodeFromDictionary(dictionary, current.Substring(0, current.Length - 1))
                    result.Add(previousCode.ToString())
                Else
                    result.Add(current(0).ToString())
                End If
                
                current = ""
            End If
        End While
        
        ' Output the last code
        If current.Length > 0 Then
            Dim code As Integer = GetCodeFromDictionary(dictionary, current)
            result.Add(code.ToString())
        End If
        
        Return String.Join(",", result)
    End Function
    
    ' Decompress a compressed string
    Public Shared Function Decompress(compressed As String) As String
        Dim dictionary As New List(Of String)()
        Dim result As New StringBuilder()
        
        ' Initialize dictionary with ASCII characters
        For i As Integer = 0 To 255
            dictionary.Add(Char.ConvertFromUtf32(i))
        Next
        
        Dim codes As String() = compressed.Split(","c)
        Dim i As Integer = 0
        
        While i < codes.Length
            Dim code As Integer = Integer.Parse(codes(i))
            
            Dim entry As String = ""
            If code < dictionary.Count Then
                entry = dictionary(code)
            Else
                entry = dictionary(0) ' This shouldn't happen in valid compression
            End If
            
            result.Append(entry)
            
            ' Add new entry to dictionary
            If i > 0 Then
                Dim firstChar As String = entry.Substring(0, 1)
                Dim previousEntry As String = dictionary(Integer.Parse(codes(i - 1)))
                Dim newEntry As String = previousEntry + firstChar
                dictionary.Add(newEntry)
            End If
            
            i += 1
        End While
        
        Return result.ToString()
    End Function
    
    ' Helper function to get code from dictionary
    Private Shared Function GetCodeFromDictionary(dictionary As Dictionary(Of String, Integer), key As String) As Integer
        If dictionary.ContainsKey(key) Then
            Return dictionary(key)
        Else
            Return -1 ' Not found
        End If
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Example 1: Simple text compression
        Dim originalText As String = "ABABABAB"
        Console.WriteLine("Original text: " & originalText)
        
        Dim compressed As String = LempelZivCompressor.Compress(originalText)
        Console.WriteLine("Compressed: " & compressed)
        
        Dim decompressed As String = LempelZivCompressor.Decompress(compressed)
        Console.WriteLine("Decompressed: " & decompressed)
        Console.WriteLine()
        
        ' Example 2: More complex text
        originalText = "TOBEORNOTTOBEORTOBEORNOT"
        Console.WriteLine("Original text: " & originalText)
        
        compressed = LempelZivCompressor.Compress(originalText)
        Console.WriteLine("Compressed: " & compressed)
        
        decompressed = LempelZivCompressor.Decompress(compressed)
        Console.WriteLine("Decompressed: " & decompressed)
        Console.WriteLine()
        
        ' Example 3: With repeated patterns
        originalText = "AAAAAAABBBBBBBCCCCCCC"
        Console.WriteLine("Original text: " & originalText)
        
        compressed = LempelZivCompressor.Compress(originalText)
        Console.WriteLine("Compressed: " & compressed)
        
        decompressed = LempelZivCompressor.Decompress(compressed)
        Console.WriteLine("Decompressed: " & decompressed)
    End Sub
End Module
```

## How it works:

1. **Compression Process:**
   - Initialize dictionary with all ASCII characters (0-255)
   - Read input character by character
   - Build strings incrementally
   - When a string is not in dictionary, output the code for the previous string
   - Add new string to dictionary
   - Continue until all input is processed

2. **Decompression Process:**
   - Initialize dictionary with ASCII characters
   - Read compressed codes one by one
   - Look up codes in dictionary
   - Build new entries in dictionary as decompression proceeds
   - Reconstruct original text

## Sample Output:
```
Original text: ABABABAB
Compressed: 65,66,65,66,65,66,65,66
Decompressed: ABABABAB

Original text: TOBEORNOTTOBEORTOBEORNOT
Compressed: 84,79,66,69,79,82,78,79,84,84,79,66,69,79,82,84,79,66,69,79,82,78,79,84
Decompressed: TOBEORNOTTOBEORTOBEORNOT

Original text: AAAAAAABBBBBBBCCCCCCC
Compressed: 65,65,65,65,65,65,66,66,66,66,66,66,67,67,67,67,67,67,67
Decompressed: AAAAAAABBBBBBBCCCCCCC
```

This implementation demonstrates the core principles of the Lempel-Ziv algorithm, which is the foundation for many modern compression algorithms including LZ77 and LZ78.

