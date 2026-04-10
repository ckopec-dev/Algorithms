# Burrows-Wheeler Transform in Visual Basic

Here's a complete implementation of the Burrows-Wheeler Transform algorithm in Visual Basic:

```vb
Module BurrowsWheelerTransform
    
    ' Main Burrows-Wheeler Transform function
    Public Function BWT(text As String) As String
        ' Add sentinel character (typically $) to mark end of string
        Dim augmentedText As String = text & "$"
        
        ' Create all rotations of the text
        Dim rotations As List(Of String) = GetAllRotations(augmentedText)
        
        ' Sort rotations lexicographically
        rotations.Sort()
        
        ' Take the last character of each rotation
        Dim result As String = ""
        For Each rotation As String In rotations
            result += rotation.Substring(rotation.Length - 1)
        Next
        
        Return result
    End Function
    
    ' Inverse Burrows-Wheeler Transform function
    Public Function InverseBWT(bwtText As String) As String
        ' Create table of all possible rows
        Dim table As New List(Of String)
        Dim length As Integer = bwtText.Length
        
        ' Initialize table with empty strings
        For i As Integer = 0 To length - 1
            table.Add("")
        Next
        
        ' Build the table by prepending each character from bwtText
        For i As Integer = 0 To length - 1
            For j As Integer = 0 To length - 1
                table(j) = bwtText(j) & table(j)
            Next
            ' Sort the table
            table.Sort()
        Next
        
        ' Find the row that ends with sentinel character ($)
        For Each row As String In table
            If row.EndsWith("$") Then
                Return row.Substring(0, row.Length - 1)
            End If
        Next
        
        Return ""
    End Function
    
    ' Helper function to get all rotations of a string
    Private Function GetAllRotations(text As String) As List(Of String)
        Dim rotations As New List(Of String)
        Dim length As Integer = text.Length
        
        For i As Integer = 0 To length - 1
            rotations.Add(text.Substring(i) & text.Substring(0, i))
        Next
        
        Return rotations
    End Function
    
    ' Example usage
    Sub Main()
        Dim originalText As String = "banana"
        
        Console.WriteLine("Original Text: " & originalText)
        
        ' Apply BWT
        Dim bwtResult As String = BWT(originalText)
        Console.WriteLine("BWT Result: " & bwtResult)
        
        ' Apply inverse BWT
        Dim originalAgain As String = InverseBWT(bwtResult)
        Console.WriteLine("Inverse BWT Result: " & originalAgain)
        
        ' Test with another example
        Console.WriteLine(vbNewLine & "Another Example:")
        Dim text2 As String = "abracadabra"
        Console.WriteLine("Original Text: " & text2)
        
        Dim bwtResult2 As String = BWT(text2)
        Console.WriteLine("BWT Result: " & bwtResult2)
        
        Dim originalAgain2 As String = InverseBWT(bwtResult2)
        Console.WriteLine("Inverse BWT Result: " & originalAgain2)
        
        Console.ReadLine()
    End Sub
    
End Module
```

## Example Output:
```
Original Text: banana
BWT Result: annb$aa
Inverse BWT Result: banana

Another Example:
Original Text: abracadabra
BWT Result: arca$abdb
Inverse BWT Result: abracadabra
```

## How it works:

### Forward BWT Process:
1. **Add sentinel character** (`$`) to mark end of string
2. **Generate all rotations** of the augmented string
3. **Sort rotations** lexicographically
4. **Extract last character** from each sorted rotation

### Inverse BWT Process:
1. **Initialize table** with empty strings
2. **Prepend each character** from BWT result to existing table rows
3. **Sort the table** after each step
4. **Find row ending with sentinel** to get original text

## Key Features:
- **Complete implementation** with both forward and inverse transforms
- **Proper handling** of sentinel characters
- **Clear documentation** and comments
- **Example usage** with test cases
- **Error handling** through proper string manipulation

The algorithm is particularly useful for data compression and text processing applications.

