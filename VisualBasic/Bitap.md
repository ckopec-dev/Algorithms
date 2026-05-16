# Bitap Algorithm Implementation in Visual Basic

The Bitap algorithm is a string searching algorithm that uses bitwise operations to find occurrences of a pattern in a text. Here's a complete implementation in Visual Basic:

```vb
Public Class BitapAlgorithm
    ' Bitap algorithm implementation for pattern searching
    Public Shared Function BitapSearch(text As String, pattern As String) As Integer
        If String.IsNullOrEmpty(pattern) Then Return -1
        If String.IsNullOrEmpty(text) Then Return -1
        
        Dim patternLength As Integer = pattern.Length
        Dim textLength As Integer = text.Length
        
        ' Create the mask array
        Dim mask(255) As Integer
        Dim i As Integer
        
        ' Initialize mask array with 0 for all characters
        For i = 0 To 255
            mask(i) = 0
        Next
        
        ' Set bits in mask for each character in pattern
        For i = 0 To patternLength - 1
            Dim charCode As Integer = Asc(pattern(i))
            mask(charCode) = mask(charCode) Or (1 << i)
        Next
        
        ' Initialize the bit array
        Dim bitArray As Integer = (1 << patternLength) - 1
        
        ' Search through the text
        For i = 0 To textLength - 1
            ' Shift the bit array to the left and set the new bit
            bitArray = (bitArray <> 1) Or mask(Asc(text(i)))
            
            ' Check if we found a match
            If (bitArray And (1 << (patternLength - 1))) = 0 Then
                Return i - patternLength + 1
            End If
        Next
        
        Return -1 ' Not found
    End Function
    
    ' Enhanced version that returns all matches
    Public Shared Function BitapSearchAll(text As String, pattern As String) As List(Of Integer)
        Dim matches As New List(Of Integer)
        Dim position As Integer = 0
        
        Do While position <= text.Length - pattern.Length
            Dim foundPosition As Integer = BitapSearch(text.Substring(position), pattern)
            If foundPosition = -1 Then Exit Do
            
            ' Adjust position to absolute position in original text
            position += foundPosition
            matches.Add(position)
            
            ' Move one position forward to find next match
            position += 1
        Loop
        
        Return matches
    End Function
    
    ' Simple version that returns boolean result
    Public Shared Function ContainsPattern(text As String, pattern As String) As Boolean
        Return BitapSearch(text, pattern) <> -1
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        Dim text As String = "This is a sample text for testing the Bitap algorithm"
        Dim pattern As String = "sample"
        
        ' Test single search
        Dim result As Integer = BitapAlgorithm.BitapSearch(text, pattern)
        If result <> -1 Then
            Console.WriteLine($"Pattern '{pattern}' found at position {result}")
        Else
            Console.WriteLine($"Pattern '{pattern}' not found")
        End If
        
        ' Test all matches
        Dim allMatches As List(Of Integer) = BitapAlgorithm.BitapSearchAll(text, "t")
        Console.WriteLine($"All occurrences of 't': {String.Join(", ", allMatches)}")
        
        ' Test boolean check
        Dim contains As Boolean = BitapAlgorithm.ContainsPattern(text, "algorithm")
        Console.WriteLine($"Text contains 'algorithm': {contains}")
        
        Console.ReadLine()
    End Sub
End Module
```

## How it works:

1. **Mask Creation**: For each character in the pattern, we create a bitmask that represents its positions
2. **Bit Array**: We maintain a bit array that represents the current state of our pattern matching
3. **Bitwise Operations**: We use bitwise OR and right shift operations to efficiently update our matching state
4. **Match Detection**: When the most significant bit becomes 0, we have found a match

## Key Features:

- **Efficient**: Uses bitwise operations for fast pattern matching
- **Multiple Results**: Can find all occurrences of a pattern
- **Boolean Check**: Simple method to check if pattern exists
- **Error Handling**: Handles edge cases like empty strings

## Time Complexity:
- Best case: O(n) where n is the text length
- Worst case: O(n × m) where m is the pattern length

This implementation demonstrates the power of bitwise operations in string searching algorithms, making it particularly efficient for scenarios where you need to search for multiple patterns or when performance is critical.

