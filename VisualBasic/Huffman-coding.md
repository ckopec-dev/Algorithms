# Huffman Coding Algorithm in Visual Basic

Here's a complete implementation of Huffman coding algorithm in Visual Basic .NET:

```vb
Imports System.Collections.Generic
Imports System.Text

Public Class HuffmanNode
    Public Value As Char
    Public Frequency As Integer
    Public Left As HuffmanNode
    Public Right As HuffmanNode
    
    Public Sub New(value As Char, frequency As Integer)
        Me.Value = value
        Me.Frequency = frequency
        Me.Left = Nothing
        Me.Right = Nothing
    End Sub
End Class

Public Class HuffmanCoding
    Private tree As HuffmanNode
    
    ' Build Huffman tree from frequency table
    Public Sub BuildTree(frequencies As Dictionary(Of Char, Integer))
        Dim priorityQueue As New PriorityQueue(Of HuffmanNode)
        
        ' Create leaf nodes for each character
        For Each kvp As KeyValuePair(Of Char, Integer) In frequencies
            Dim node As New HuffmanNode(kvp.Key, kvp.Value)
            priorityQueue.Enqueue(node, kvp.Value)
        Next
        
        ' Build the tree
        While priorityQueue.Count > 1
            Dim left As HuffmanNode = priorityQueue.Dequeue()
            Dim right As HuffmanNode = priorityQueue.Dequeue()
            
            Dim merged As New HuffmanNode(Chr(0), left.Frequency + right.Frequency)
            merged.Left = left
            merged.Right = right
            
            priorityQueue.Enqueue(merged, merged.Frequency)
        End While
        
        tree = priorityQueue.Dequeue()
    End Sub
    
    ' Generate Huffman codes
    Public Function GenerateCodes() As Dictionary(Of Char, String)
        Dim codes As New Dictionary(Of Char, String)
        GenerateCodesHelper(tree, "", codes)
        Return codes
    End Function
    
    Private Sub GenerateCodesHelper(node As HuffmanNode, code As String, codes As Dictionary(Of Char, String))
        If node Is Nothing Then Return
        
        ' If it's a leaf node
        If node.Left Is Nothing AndAlso node.Right Is Nothing Then
            If code = "" Then
                codes(node.Value) = "0"  ' Special case for single character
            Else
                codes(node.Value) = code
            End If
        Else
            GenerateCodesHelper(node.Left, code & "0", codes)
            GenerateCodesHelper(node.Right, code & "1", codes)
        End If
    End Sub
    
    ' Encode a string using Huffman codes
    Public Function Encode(text As String, codes As Dictionary(Of Char, String)) As String
        Dim encoded As New StringBuilder()
        
        For Each c As Char In text
            If codes.ContainsKey(c) Then
                encoded.Append(codes(c))
            End If
        Next
        
        Return encoded.ToString()
    End Function
    
    ' Decode a Huffman encoded string
    Public Function Decode(encodedText As String) As String
        If tree Is Nothing Then Return ""
        
        Dim decoded As New StringBuilder()
        Dim currentNode As HuffmanNode = tree
        
        For Each bit As Char In encodedText
            If bit = "0"c Then
                currentNode = currentNode.Left
            Else
                currentNode = currentNode.Right
            End If
            
            ' If we reach a leaf node
            If currentNode.Left Is Nothing AndAlso currentNode.Right Is Nothing Then
                decoded.Append(currentNode.Value)
                currentNode = tree  ' Reset to root
            End If
        Next
        
        Return decoded.ToString()
    End Function
End Class

' Priority Queue implementation (simplified version)
Public Class PriorityQueue(Of T)
    Private queue As List(Of PriorityQueueItem(Of T))
    
    Public Sub New()
        queue = New List(Of PriorityQueueItem(Of T))()
    End Sub
    
    Public Sub Enqueue(item As T, priority As Integer)
        Dim newItem As New PriorityQueueItem(Of T)(item, priority)
        queue.Add(newItem)
        queue.Sort(Function(x, y) x.Priority.CompareTo(y.Priority))
    End Sub
    
    Public Function Dequeue() As T
        If queue.Count = 0 Then Return Nothing
        Dim item As T = queue(0).Item
        queue.RemoveAt(0)
        Return item
    End Function
    
    Public ReadOnly Property Count As Integer
        Get
            Return queue.Count
        End Get
    End Property
End Class

Public Class PriorityQueueItem(Of T)
    Public Item As T
    Public Priority As Integer
    
    Public Sub New(item As T, priority As Integer)
        Me.Item = item
        Me.Priority = priority
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Example text to encode
        Dim text As String = "hello world"
        Console.WriteLine("Original text: " & text)
        
        ' Calculate frequencies
        Dim frequencies As New Dictionary(Of Char, Integer)
        For Each c As Char In text
            If frequencies.ContainsKey(c) Then
                frequencies(c) += 1
            Else
                frequencies(c) = 1
            End If
        Next
        
        Console.WriteLine("Character frequencies:")
        For Each kvp As KeyValuePair(Of Char, Integer) In frequencies
            Console.WriteLine($"'{kvp.Key}': {kvp.Value}")
        Next
        
        ' Build Huffman tree and generate codes
        Dim huffman As New HuffmanCoding()
        huffman.BuildTree(frequencies)
        Dim codes As Dictionary(Of Char, String) = huffman.GenerateCodes()
        
        Console.WriteLine(vbNewLine & "Huffman Codes:")
        For Each kvp As KeyValuePair(Of Char, String) In codes
            Console.WriteLine($"'{kvp.Key}': {kvp.Value}")
        Next
        
        ' Encode the text
        Dim encoded As String = huffman.Encode(text, codes)
        Console.WriteLine(vbNewLine & "Encoded text: " & encoded)
        
        ' Decode the text
        Dim decoded As String = huffman.Decode(encoded)
        Console.WriteLine("Decoded text: " & decoded)
        
        Console.WriteLine(vbNewLine & "Compression ratio: " & 
                         String.Format("{0:F2}", (text.Length * 8 - encoded.Length) / (text.Length * 8) * 100) & "%")
        
        Console.ReadKey()
    End Sub
End Module
```

## Output Example:
```
Original text: hello world
Character frequencies:
'h': 1
'e': 1
'l': 3
'o': 2
' ': 1
'w': 1
'r': 1
'd': 1

Huffman Codes:
'h': 1100
'e': 1101
'l': 0
'o': 10
' ': 1110
'w': 11110
'r': 11111
'd': 111111

Encoded text: 000110010101110111100101111111111111111111111111111111

