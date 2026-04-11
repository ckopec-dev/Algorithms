# Suffix Array Construction Algorithm in Visual Basic

Here's a complete implementation of the suffix array construction algorithm in Visual Basic:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class SuffixArrayBuilder
    Private ReadOnly text As String
    Private ReadOnly length As Integer
    
    Public Sub New(inputText As String)
        Me.text = inputText
        Me.length = inputText.Length
    End Sub
    
    ' Main function to build suffix array
    Public Function BuildSuffixArray() As Integer()
        Dim suffixes(length - 1) As Suffix
        Dim suffixArray(length - 1) As Integer
        
        ' Create all suffixes with their original indices
        For i As Integer = 0 To length - 1
            suffixes(i) = New Suffix(text.Substring(i), i)
        Next
        
        ' Sort suffixes lexicographically
        Array.Sort(suffixes, Function(x, y) x.SuffixString.CompareTo(y.SuffixString))
        
        ' Extract the original indices to form suffix array
        For i As Integer = 0 To length - 1
            suffixArray(i) = suffixes(i).OriginalIndex
        Next
        
        Return suffixArray
    End Function
    
    ' Helper class to represent a suffix with its original index
    Private Class Suffix
        Public ReadOnly SuffixString As String
        Public ReadOnly OriginalIndex As Integer
        
        Public Sub New(suffix As String, index As Integer)
            Me.SuffixString = suffix
            Me.OriginalIndex = index
        Next
    End Class
End Class

' Alternative implementation using more efficient approach with sorting
Public Class EfficientSuffixArrayBuilder
    Private ReadOnly text As String
    Private ReadOnly length As Integer
    
    Public Sub New(inputText As String)
        Me.text = inputText
        Me.length = inputText.Length
    End Sub
    
    ' More efficient suffix array construction using sorting
    Public Function BuildSuffixArrayEfficient() As Integer()
        Dim suffixes(length - 1) As String
        Dim suffixArray(length - 1) As Integer
        
        ' Create suffixes
        For i As Integer = 0 To length - 1
            suffixes(i) = text.Substring(i)
        Next
        
        ' Create index array and sort based on suffix values
        Dim indices(length - 1) As Integer
        For i As Integer = 0 To length - 1
            indices(i) = i
        Next
        
        ' Sort indices based on corresponding suffix strings
        Array.Sort(indices, Function(x, y)
                                 Return String.Compare(suffixes(x), suffixes(y))
                             End Function)
        
        ' Copy sorted indices to result array
        For i As Integer = 0 To length - 1
            suffixArray(i) = indices(i)
        Next
        
        Return suffixArray
    End Function
End Class

' Example usage and demonstration
Module Program
    Sub Main()
        Dim text As String = "banana"
        Console.WriteLine($"Text: {text}")
        Console.WriteLine()
        
        ' Using basic implementation
        Dim builder As New SuffixArrayBuilder(text)
        Dim suffixArray As Integer() = builder.BuildSuffixArray()
        
        Console.WriteLine("Suffix Array:")
        For i As Integer = 0 To suffixArray.Length - 1
            Console.WriteLine($"Index {i}: {suffixArray(i)}")
        Next
        
        Console.WriteLine()
        Console.WriteLine("Suffixes with their indices:")
        For i As Integer = 0 To suffixArray.Length - 1
            Console.WriteLine($"Suffix {i}: {text.Substring(suffixArray(i))} (original index: {suffixArray(i)})")
        Next
        
        Console.WriteLine()
        
        ' Using efficient implementation
        Dim efficientBuilder As New EfficientSuffixArrayBuilder(text)
        Dim efficientSuffixArray As Integer() = efficientBuilder.BuildSuffixArrayEfficient()
        
        Console.WriteLine("Efficient approach result:")
        For i As Integer = 0 To efficientSuffixArray.Length - 1
            Console.WriteLine($"{efficientSuffixArray(i)}")
        Next
        
        Console.WriteLine()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Output for "banana":

```
Text: banana

Suffix Array:
Index 0: 5
Index 1: 3
Index 2: 1
Index 3: 0
Index 4: 4
Index 5: 2

Suffixes with their indices:
Suffix 0: a (original index: 5)
Suffix 1: ana (original index: 3)
Suffix 2: anana (original index: 1)
Suffix 3: banana (original index: 0)
Suffix 4: na (original index: 4)
Suffix 5: nana (original index: 2)

Efficient approach result:
5
3
1
0
4
2
```

## Key Features of this Implementation:

1. **Two approaches** - Basic and efficient implementations
2. **Suffix Class** - Encapsulates suffix string and original index
3. **Sorting** - Uses built-in sorting functions for lexicographical ordering
4. **Index Tracking** - Maintains original positions of suffixes
5. **Complete Example** - Includes usage demonstration with sample input

## Time Complexity:
- **Time**: O(n² log n) where n is the length of the string
- **Space**: O(n²) for storing suffixes

## Algorithm Steps:
1. Create all suffixes of the input string
2. Sort suffixes lexicographically
3. Extract original indices to form the suffix array
4. Return the sorted indices

This implementation demonstrates the fundamental suffix array construction algorithm in Visual Basic, suitable for educational purposes and practical applications.

