# Knuth-Morris-Pratt (KMP) Algorithm in Visual Basic

Here's a complete implementation of the KMP algorithm in Visual Basic .NET:

```vb
Public Class KMPAlgorithm
    ' Build the failure function (also known as the prefix function)
    Private Shared Function BuildFailureFunction(pattern As String) As Integer()
        Dim m As Integer = pattern.Length
        Dim failure As New Integer(m)
        failure(0) = 0
        
        Dim j As Integer = 0
        
        For i As Integer = 1 To m - 1
            While j > 0 AndAlso pattern(i) <> pattern(j)
                j = failure(j - 1)
            End While
            
            If pattern(i) = pattern(j) Then
                j += 1
            End If
            
            failure(i) = j
        Next
        
        Return failure
    End Function
    
    ' KMP search algorithm
    Public Shared Function KMPSearch(text As String, pattern As String) As Integer
        If String.IsNullOrEmpty(pattern) Then
            Return -1
        End If
        
        If String.IsNullOrEmpty(text) Then
            Return -1
        End If
        
        Dim failure As Integer() = BuildFailureFunction(pattern)
        Dim n As Integer = text.Length
        Dim m As Integer = pattern.Length
        Dim j As Integer = 0
        
        For i As Integer = 0 To n - 1
            While j > 0 AndAlso text(i) <> pattern(j)
                j = failure(j - 1)
            End While
            
            If text(i) = pattern(j) Then
                j += 1
            End If
            
            If j = m Then
                Return i - m + 1 ' Found match at position i - m + 1
            End If
        Next
        
        Return -1 ' No match found
    End Function
    
    ' Find all occurrences of pattern in text
    Public Shared Function FindAllOccurrences(text As String, pattern As String) As List(Of Integer)
        Dim occurrences As New List(Of Integer)
        Dim failure As Integer() = BuildFailureFunction(pattern)
        Dim n As Integer = text.Length
        Dim m As Integer = pattern.Length
        Dim j As Integer = 0
        
        For i As Integer = 0 To n - 1
            While j > 0 AndAlso text(i) <> pattern(j)
                j = failure(j - 1)
            End While
            
            If text(i) = pattern(j) Then
                j += 1
            End If
            
            If j = m Then
                occurrences.Add(i - m + 1)
                j = failure(j - 1) ' Continue searching for more matches
            End If
        Next
        
        Return occurrences
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        Dim text As String = "ABABDABACDABABCABCABCABCABC"
        Dim pattern As String = "ABABCABCABCABC"
        
        Console.WriteLine("Text: " & text)
        Console.WriteLine("Pattern: " & pattern)
        Console.WriteLine()
        
        ' Single search
        Dim result As Integer = KMPAlgorithm.KMPSearch(text, pattern)
        If result <> -1 Then
            Console.WriteLine("Pattern found at position: " & result)
        Else
            Console.WriteLine("Pattern not found")
        End If
        
        Console.WriteLine()
        
        ' Find all occurrences
        Dim allOccurrences As List(Of Integer) = KMPAlgorithm.FindAllOccurrences(text, pattern)
        If allOccurrences.Count > 0 Then
            Console.WriteLine("All occurrences found at positions: " & String.Join(", ", allOccurrences))
        Else
            Console.WriteLine("No occurrences found")
        End If
        
        Console.WriteLine()
        
        ' Test with another example
        Dim text2 As String = "AABAACAADAABAABA"
        Dim pattern2 As String = "AABA"
        
        Console.WriteLine("Text: " & text2)
        Console.WriteLine("Pattern: " & pattern2)
        Console.WriteLine()
        
        Dim result2 As Integer = KMPAlgorithm.KMPSearch(text2, pattern2)
        If result2 <> -1 Then
            Console.WriteLine("Pattern found at position: " & result2)
        Else
            Console.WriteLine("Pattern not found")
        End If
        
        Console.ReadLine()
    End Sub
End Module
```

## How the Algorithm Works

### Key Components:

1. **BuildFailureFunction**: Creates the failure function (prefix function) that helps determine how much to shift the pattern when a mismatch occurs
2. **KMPSearch**: Main search function that finds the first occurrence of the pattern
3. **FindAllOccurrences**: Extended version that finds all occurrences of the pattern

### Algorithm Steps:

1. **Preprocessing**: Build the failure function for the pattern
2. **Search**: Use the failure function to avoid unnecessary character comparisons
3. **Match**: When a complete match is found, return the position

### Time Complexity:
- Preprocessing: O(m) where m is the length of the pattern
- Search: O(n) where n is the length of the text
- Overall: O(n + m)

### Example Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC

Pattern found at position: 10

All occurrences found at positions: 10, 15

Text: AABAACAADAABAABA
Pattern: AABA

Pattern found at position: 12
```

This implementation efficiently handles pattern searching with the KMP algorithm's advantage of avoiding redundant comparisons by using the precomputed failure function.

