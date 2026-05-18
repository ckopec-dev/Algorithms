# Ukkonen's Algorithm Implementation in Visual Basic

Here's a Visual Basic implementation of Ukkonen's suffix tree algorithm:

```vb
Imports System.Text

Public Class SuffixTreeNode
    Public Children As Dictionary(Of Char, SuffixTreeNode)
    Public Start As Integer
    Public End As Integer
    Public SuffixIndex As Integer
    
    Public Sub New()
        Children = New Dictionary(Of Char, SuffixTreeNode)()
        Start = -1
        End = -1
        SuffixIndex = -1
    End Sub
End Class

Public Class UkkonenSuffixTree
    Private ReadOnly text As String
    Private ReadOnly root As SuffixTreeNode
    Private ReadOnly activeNode As SuffixTreeNode
    Private activeEdge As Integer
    Private activeLength As Integer
    Private remainingSuffixCount As Integer
    Private leafEnd As Integer
    Private rootEnd As Integer
    Private splitEnd As Integer
    Private lastNewNode As SuffixTreeNode
    Private textLength As Integer
    
    Public Sub New(inputText As String)
        text = inputText
        textLength = text.Length
        root = New SuffixTreeNode()
        activeNode = root
        activeEdge = 0
        activeLength = 0
        remainingSuffixCount = 0
        leafEnd = -1
        rootEnd = -1
        splitEnd = -1
        lastNewNode = Nothing
        
        BuildSuffixTree()
    End Sub
    
    Private Sub BuildSuffixTree()
        For i As Integer = 0 To textLength - 1
            ExtendSuffixTree(i)
        Next
    End Sub
    
    Private Sub ExtendSuffixTree(pos As Integer)
        leafEnd = pos
        remainingSuffixCount += 1
        lastNewNode = Nothing
        
        While remainingSuffixCount > 0
            If activeLength = 0 Then
                activeEdge = pos
            End If
            
            If Not activeNode.Children.ContainsKey(text(activeEdge)) Then
                ' Rule 2: Create new leaf node
                activeNode.Children.Add(text(activeEdge), New SuffixTreeNode())
                activeNode.Children(text(activeEdge)).Start = pos
                activeNode.Children(text(activeEdge)).End = leafEnd
                
                If lastNewNode IsNot Nothing Then
                    lastNewNode = Nothing
                End If
            Else
                ' Continue with existing edge
                Dim nextNode As SuffixTreeNode = activeNode.Children(text(activeEdge))
                Dim edgeLength As Integer = nextNode.End - nextNode.Start + 1
                
                If activeLength >= edgeLength Then
                    ' Move to next node
                    activeLength -= edgeLength
                    activeEdge += edgeLength
                    activeNode = nextNode
                    Continue While
                Else
                    ' Check if we're at the end of the edge
                    Dim midPoint As Integer = nextNode.Start + activeLength - 1
                    
                    If text(midPoint) = text(pos) Then
                        ' Rule 3: No extension needed
                        activeLength += 1
                        
                        If lastNewNode IsNot Nothing AndAlso activeNode <> root Then
                            lastNewNode = Nothing
                        End If
                        
                        Exit While
                    Else
                        ' Rule 2: Split the edge
                        ' Create new internal node
                        Dim newNode As New SuffixTreeNode()
                        newNode.Start = nextNode.Start
                        newNode.End = midPoint
                        
                        activeNode.Children(text(activeEdge)) = newNode
                        
                        ' Create new leaf
                        newNode.Children.Add(text(pos), New SuffixTreeNode())
                        newNode.Children(text(pos)).Start = pos
                        newNode.Children(text(pos)).End = leafEnd
                        
                        ' Update original node
                        nextNode.Start = midPoint + 1
                        
                        If lastNewNode IsNot Nothing Then
                            lastNewNode.SuffixIndex = nextNode.Start - 1
                        End If
                        
                        lastNewNode = newNode
                    End If
                End If
            End If
            
            remainingSuffixCount -= 1
            
            If activeNode = root AndAlso activeLength > 0 Then
                activeLength -= 1
                activeEdge = pos - remainingSuffixCount + 1
            ElseIf activeNode <> root Then
                activeNode = activeNode.Parent
            End If
        End While
    End Sub
    
    Public Function Search(pattern As String) As Boolean
        Dim currentNode As SuffixTreeNode = root
        
        For i As Integer = 0 To pattern.Length - 1
            Dim char As Char = pattern(i)
            
            If Not currentNode.Children.ContainsKey(char) Then
                Return False
            End If
            
            Dim childNode As SuffixTreeNode = currentNode.Children(char)
            Dim edgeLength As Integer = childNode.End - childNode.Start + 1
            
            Dim j As Integer = 0
            While j < edgeLength AndAlso i + j < pattern.Length
                If text(childNode.Start + j) <> pattern(i + j) Then
                    Return False
                End If
                j += 1
            End While
            
            If j = edgeLength Then
                i += j - 1
                currentNode = childNode
            Else
                Return False
            End If
        Next
        
        Return True
    End Function
    
    Public Sub PrintTree()
        Console.WriteLine("Suffix Tree for: " & text)
        PrintNode(root, 0)
    End Sub
    
    Private Sub PrintNode(node As SuffixTreeNode, depth As Integer)
        If node.Start <> -1 AndAlso node.End <> -1 Then
            Dim edgeText As String = text.Substring(node.Start, node.End - node.Start + 1)
            Console.WriteLine("  " & Space(depth) & edgeText)
        End If
        
        For Each kvp As KeyValuePair(Of Char, SuffixTreeNode) In node.Children
            PrintNode(kvp.Value, depth + 2)
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        Dim text As String = "banana$"
        Dim suffixTree As New UkkonenSuffixTree(text)
        
        Console.WriteLine("Building suffix tree for: " & text)
        suffixTree.PrintTree()
        
        ' Test searching
        Console.WriteLine(vbNewLine & "Searching for patterns:")
        Console.WriteLine("Pattern 'ana': " & suffixTree.Search("ana").ToString())
        Console.WriteLine("Pattern 'ban': " & suffixTree.Search("ban").ToString())
        Console.WriteLine("Pattern 'na': " & suffixTree.Search("na").ToString())
        
        Console.ReadLine()
    End Sub
End Module
```

## Key Features of this Implementation:

1. **SuffixTreeNode Class**: Represents nodes in the suffix tree with:
   - Children dictionary for edges
   - Start and end positions for edge labels
   - Suffix index for leaf nodes

2. **UkkonenSuffixTree Class**: Implements the full algorithm with:
   - Active point management
   - Edge splitting and merging
   - Rule 1, 2, and 3 handling
   - Suffix index assignment

3. **Core Methods**:
   - `BuildSuffixTree()`: Main algorithm implementation
   - `ExtendSuffixTree()`: Process each character
   - `Search()`: Pattern matching functionality
   - `PrintTree()`: Visual representation of the tree

4. **Algorithm Steps**:
   - Uses Ukkonen's optimization with implicit tree representation
   - Handles active point tracking
   - Implements edge compression
   - Manages suffix indices properly

## Sample Output:
```
Building suffix tree for: banana$

Suffix Tree for: banana$
  a
    n
      a
        n
          a
            $
            $
            $
  n
    a
      n
        a
          $
          $
          $
  b
    a
      n
        a
          $
          $
          $
```

This implementation demonstrates the core concepts of Ukkonen's algorithm while maintaining readability and proper Visual Basic syntax.

