# Binary Search Tree in Visual Basic

Here's a complete implementation of a Binary Search Tree algorithm in Visual Basic:

```vb
' Binary Search Tree Implementation in Visual Basic

Public Class TreeNode
    Public Value As Integer
    Public Left As TreeNode
    Public Right As TreeNode
    
    Public Sub New(value As Integer)
        Me.Value = value
        Me.Left = Nothing
        Me.Right = Nothing
    End Sub
End Class

Public Class BinarySearchTree
    Private Root As TreeNode
    
    Public Sub New()
        Root = Nothing
    End Sub
    
    ' Insert a value into the BST
    Public Sub Insert(value As Integer)
        Root = InsertRecursive(Root, value)
    End Sub
    
    Private Function InsertRecursive(current As TreeNode, value As Integer) As TreeNode
        If current Is Nothing Then
            Return New TreeNode(value)
        End If
        
        If value < current.Value Then
            current.Left = InsertRecursive(current.Left, value)
        ElseIf value > current.Value Then
            current.Right = InsertRecursive(current.Right, value)
        End If
        
        Return current
    End Function
    
    ' Search for a value in the BST
    Public Function Search(value As Integer) As Boolean
        Return SearchRecursive(Root, value)
    End Function
    
    Private Function SearchRecursive(current As TreeNode, value As Integer) As Boolean
        If current Is Nothing Then
            Return False
        End If
        
        If value = current.Value Then
            Return True
        ElseIf value < current.Value Then
            Return SearchRecursive(current.Left, value)
        Else
            Return SearchRecursive(current.Right, value)
        End If
    End Function
    
    ' In-order traversal (prints values in sorted order)
    Public Sub InOrderTraversal()
        Console.Write("In-order: ")
        InOrderRecursive(Root)
        Console.WriteLine()
    End Sub
    
    Private Sub InOrderRecursive(current As TreeNode)
        If current IsNot Nothing Then
            InOrderRecursive(current.Left)
            Console.Write(current.Value & " ")
            InOrderRecursive(current.Right)
        End If
    End Sub
    
    ' Pre-order traversal
    Public Sub PreOrderTraversal()
        Console.Write("Pre-order: ")
        PreOrderRecursive(Root)
        Console.WriteLine()
    End Sub
    
    Private Sub PreOrderRecursive(current As TreeNode)
        If current IsNot Nothing Then
            Console.Write(current.Value & " ")
            PreOrderRecursive(current.Left)
            PreOrderRecursive(current.Right)
        End If
    End Sub
    
    ' Post-order traversal
    Public Sub PostOrderTraversal()
        Console.Write("Post-order: ")
        PostOrderRecursive(Root)
        Console.WriteLine()
    End Sub
    
    Private Sub PostOrderRecursive(current As TreeNode)
        If current IsNot Nothing Then
            PostOrderRecursive(current.Left)
            PostOrderRecursive(current.Right)
            Console.Write(current.Value & " ")
        End If
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        Dim bst As New BinarySearchTree()
        
        ' Insert values
        Console.WriteLine("Inserting values: 50, 30, 70, 20, 40, 60, 80")
        bst.Insert(50)
        bst.Insert(30)
        bst.Insert(70)
        bst.Insert(20)
        bst.Insert(40)
        bst.Insert(60)
        bst.Insert(80)
        
        ' Display traversals
        bst.InOrderTraversal()      ' Should print sorted order
        bst.PreOrderTraversal()     ' Should print root first
        bst.PostOrderTraversal()    ' Should print leaves first
        
        ' Search for values
        Console.WriteLine()
        Console.WriteLine("Searching for 40: " & bst.Search(40).ToString())
        Console.WriteLine("Searching for 25: " & bst.Search(25).ToString())
        Console.WriteLine("Searching for 80: " & bst.Search(80).ToString())
        
        Console.ReadLine()
    End Sub
End Module
```

## Output Example:
```
Inserting values: 50, 30, 70, 20, 40, 60, 80
In-order: 20 30 40 50 60 70 80 
Pre-order: 50 30 20 40 70 60 80 
Post-order: 20 40 30 60 80 70 50 

Searching for 40: True
Searching for 25: False
Searching for 80: True
```

## Key Features:

1. **TreeNode Class**: Represents each node with value, left child, and right child
2. **BinarySearchTree Class**: Contains all BST operations
3. **Insert Method**: Adds new values maintaining BST property
4. **Search Method**: Finds values efficiently using BST property
5. **Traversal Methods**: In-order, pre-order, and post-order traversals
6. **Recursive Implementation**: Uses recursion for clean, readable code

## Time Complexity:
- **Search**: O(log n) average, O(n) worst case
- **Insert**: O(log n) average, O(n) worst case
- **Traversal**: O(n) for all traversals

This implementation demonstrates the fundamental principles of binary search trees in Visual Basic with proper encapsulation and recursive algorithms.

