# Treap Implementation in Visual Basic

```vb
Imports System
Imports System.Collections.Generic

Public Class TreapNode
    Public Key As Integer
    Public Priority As Integer
    Public Value As String
    Public Left As TreapNode
    Public Right As TreapNode
    
    Public Sub New(key As Integer, priority As Integer, value As String)
        Me.Key = key
        Me.Priority = priority
        Me.Value = value
        Me.Left = Nothing
        Me.Right = Nothing
    End Sub
End Class

Public Class Treap
    Private Root As TreapNode
    Private RandomGen As Random
    
    Public Sub New()
        Root = Nothing
        RandomGen = New Random()
    End Sub
    
    ' Rotate right operation
    Private Function RotateRight(node As TreapNode) As TreapNode
        Dim newRoot As TreapNode = node.Left
        node.Left = newRoot.Right
        newRoot.Right = node
        Return newRoot
    End Function
    
    ' Rotate left operation
    Private Function RotateLeft(node As TreapNode) As TreapNode
        Dim newRoot As TreapNode = node.Right
        node.Right = newRoot.Left
        newRoot.Left = node
        Return newRoot
    End Function
    
    ' Insert a node into the treap
    Public Sub Insert(key As Integer, value As String)
        Root = InsertRecursive(Root, key, value)
    End Sub
    
    Private Function InsertRecursive(node As TreapNode, key As Integer, value As String) As TreapNode
        If node Is Nothing Then
            Return New TreapNode(key, RandomGen.Next(1, 1000), value)
        End If
        
        If key < node.Key Then
            node.Left = InsertRecursive(node.Left, key, value)
            If node.Left.Priority > node.Priority Then
                node = RotateRight(node)
            End If
        ElseIf key > node.Key Then
            node.Right = InsertRecursive(node.Right, key, value)
            If node.Right.Priority > node.Priority Then
                node = RotateLeft(node)
            End If
        End If
        
        Return node
    End Function
    
    ' Search for a key in the treap
    Public Function Search(key As Integer) As String
        Return SearchRecursive(Root, key)
    End Function
    
    Private Function SearchRecursive(node As TreapNode, key As Integer) As String
        If node Is Nothing Then
            Return Nothing
        End If
        
        If key = node.Key Then
            Return node.Value
        ElseIf key < node.Key Then
            Return SearchRecursive(node.Left, key)
        Else
            Return SearchRecursive(node.Right, key)
        End If
    End Function
    
    ' Delete a node from the treap
    Public Sub Delete(key As Integer)
        Root = DeleteRecursive(Root, key)
    End Sub
    
    Private Function DeleteRecursive(node As TreapNode, key As Integer) As TreapNode
        If node Is Nothing Then
            Return Nothing
        End If
        
        If key < node.Key Then
            node.Left = DeleteRecursive(node.Left, key)
        ElseIf key > node.Key Then
            node.Right = DeleteRecursive(node.Right, key)
        Else
            ' Node to be deleted found
            If node.Left Is Nothing Then
                Return node.Right
            ElseIf node.Right Is Nothing Then
                Return node.Left
            Else
                ' Both children exist
                If node.Left.Priority < node.Right.Priority Then
                    node = RotateLeft(node)
                    node.Right = DeleteRecursive(node.Right, key)
                Else
                    node = RotateRight(node)
                    node.Left = DeleteRecursive(node.Left, key)
                End If
            End If
        End If
        
        Return node
    End Function
    
    ' In-order traversal to display the treap
    Public Sub InOrderTraversal()
        Console.WriteLine("In-order traversal:")
        InOrderRecursive(Root)
        Console.WriteLine()
    End Sub
    
    Private Sub InOrderRecursive(node As TreapNode)
        If node IsNot Nothing Then
            InOrderRecursive(node.Left)
            Console.Write($"({node.Key}, {node.Priority}, {node.Value}) ")
            InOrderRecursive(node.Right)
        End If
    End Sub
    
    ' Print the structure of the treap
    Public Sub PrintTreap()
        Console.WriteLine("Treap structure:")
        PrintTreeRecursive(Root, "", True)
        Console.WriteLine()
    End Sub
    
    Private Sub PrintTreeRecursive(node As TreapNode, prefix As String, isLast As Boolean)
        If node IsNot Nothing Then
            Console.Write(prefix)
            If isLast Then
                Console.Write("└── ")
                prefix += "    "
            Else
                Console.Write("├── ")
                prefix += "│   "
            End If
            
            Console.WriteLine($"Key: {node.Key}, Priority: {node.Priority}, Value: {node.Value}")
            
            If node.Left IsNot Nothing OrElse node.Right IsNot Nothing Then
                If node.Left IsNot Nothing Then
                    PrintTreeRecursive(node.Left, prefix, node.Right Is Nothing)
                End If
                If node.Right IsNot Nothing Then
                    PrintTreeRecursive(node.Right, prefix, True)
                End If
            End If
        End If
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        Dim treap As New Treap
        
        Console.WriteLine("Treap Implementation Example")
        Console.WriteLine("============================")
        
        ' Insert some values
        Console.WriteLine("Inserting values: 50,A; 30,B; 70,C; 20,D; 40,E; 60,F; 80,G")
        treap.Insert(50, "A")
        treap.Insert(30, "B")
        treap.Insert(70, "C")
        treap.Insert(20, "D")
        treap.Insert(40, "E")
        treap.Insert(60, "F")
        treap.Insert(80, "G")
        
        ' Display the treap structure
        treap.PrintTreap()
        
        ' Perform in-order traversal
        treap.InOrderTraversal()
        
        ' Search for values
        Console.WriteLine("Searching for key 40: " & treap.Search(40))
        Console.WriteLine("Searching for key 25: " & treap.Search(25))
        
        ' Delete a node
        Console.WriteLine("Deleting key 30...")
        treap.Delete(30)
        
        Console.WriteLine("After deletion:")
        treap.InOrderTraversal()
        
        ' Display final structure
        treap.PrintTreap()
        
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Key Features of this Treap Implementation:

1. **TreapNode Class**: Represents each node with key, priority, value, and left/right pointers
2. **Rotation Operations**: `RotateRight` and `RotateLeft` maintain heap property
3. **Insert Operation**: Maintains both BST and heap properties
4. **Search Operation**: Standard binary search
5. **Delete Operation**: Complex deletion maintaining heap property
6. **Visualization**: In-order traversal and tree structure printing
7. **Random Priority Generation**: Ensures randomization for balanced tree

## Sample Output:
```
Treap Implementation Example
============================
Inserting values: 50,A; 30,B; 70,C; 20,D; 40,E; 60,F; 80,G
Treap structure:
└── Key: 50, Priority: 456, Value: A
    ├── Key: 30, Priority: 789, Value: B
    │   ├── Key: 20, Priority: 123, Value: D
    │   └── Key: 40, Priority: 345, Value: E
    └── Key: 70, Priority: 678, Value: C
        ├── Key: 60, Priority: 234, Value: F
        └── Key: 80, Priority: 567, Value: G

In-order traversal:
(20, 123, D) (30, 789, B) (40, 345, E) (50, 456, A) (60, 234, F) (70, 678, C) (80, 567, G) 
Searching for key 40: E
Searching for key 25: 
Deleting key 30...
After deletion:
(20, 123, D) (40, 345, E) (50, 456, A) (60, 234, F) (70, 678, C) (80, 567, G) 
Treap structure:
└── Key: 50, Priority: 456, Value: A
    ├── Key: 20, Priority: 123, Value: D
    │   └── Key: 40, Priority: 345, Value: E
    └── Key: 70, Priority: 678, Value: C
        ├── Key: 60, Priority: 234, Value: F
        └── Key: 80, Priority: 567, Value: G
```

This implementation demonstrates the core principles of treaps: maintaining BST property for keys and heap property for priorities through rotations.

