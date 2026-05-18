# AVL Tree Balancing Algorithm in Visual Basic

Here's a complete implementation of an AVL tree with balancing operations in Visual Basic:

```vb
Public Class AVLNode
    Public Value As Integer
    Public Left As AVLNode
    Public Right As AVLNode
    Public Height As Integer
    
    Public Sub New(value As Integer)
        Me.Value = value
        Me.Height = 1
        Me.Left = Nothing
        Me.Right = Nothing
    End Sub
End Class

Public Class AVLTree
    Private Root As AVLNode
    
    Public Sub New()
        Root = Nothing
    End Sub
    
    ' Get height of node
    Private Function GetHeight(node As AVLNode) As Integer
        If node Is Nothing Then
            Return 0
        End If
        Return node.Height
    End Function
    
    ' Get balance factor
    Private Function GetBalance(node As AVLNode) As Integer
        If node Is Nothing Then
            Return 0
        End If
        Return GetHeight(node.Left) - GetHeight(node.Right)
    End Function
    
    ' Right rotate
    Private Function RotateRight(y As AVLNode) As AVLNode
        Dim x As AVLNode = y.Left
        Dim T2 As AVLNode = x.Right
        
        ' Perform rotation
        x.Right = y
        y.Left = T2
        
        ' Update heights
        y.Height = Math.Max(GetHeight(y.Left), GetHeight(y.Right)) + 1
        x.Height = Math.Max(GetHeight(x.Left), GetHeight(x.Right)) + 1
        
        ' Return new root
        Return x
    End Function
    
    ' Left rotate
    Private Function RotateLeft(x As AVLNode) As AVLNode
        Dim y As AVLNode = x.Right
        Dim T2 As AVLNode = y.Left
        
        ' Perform rotation
        y.Left = x
        x.Right = T2
        
        ' Update heights
        x.Height = Math.Max(GetHeight(x.Left), GetHeight(x.Right)) + 1
        y.Height = Math.Max(GetHeight(y.Left), GetHeight(y.Right)) + 1
        
        ' Return new root
        Return y
    End Function
    
    ' Insert a value
    Public Sub Insert(value As Integer)
        Root = InsertNode(Root, value)
    End Sub
    
    Private Function InsertNode(node As AVLNode, value As Integer) As AVLNode
        ' 1. Perform normal BST insertion
        If node Is Nothing Then
            Return New AVLNode(value)
        End If
        
        If value < node.Value Then
            node.Left = InsertNode(node.Left, value)
        ElseIf value > node.Value Then
            node.Right = InsertNode(node.Right, value)
        Else
            ' Duplicate values not allowed
            Return node
        End If
        
        ' 2. Update height of current node
        node.Height = Math.Max(GetHeight(node.Left), GetHeight(node.Right)) + 1
        
        ' 3. Get balance factor
        Dim balance As Integer = GetBalance(node)
        
        ' 4. Perform rotations if unbalanced
        
        ' Left Left Case
        If balance > 1 AndAlso value < node.Left.Value Then
            Return RotateRight(node)
        End If
        
        ' Right Right Case
        If balance < -1 AndAlso value > node.Right.Value Then
            Return RotateLeft(node)
        End If
        
        ' Left Right Case
        If balance > 1 AndAlso value > node.Left.Value Then
            node.Left = RotateLeft(node.Left)
            Return RotateRight(node)
        End If
        
        ' Right Left Case
        If balance < -1 AndAlso value < node.Right.Value Then
            node.Right = RotateRight(node.Right)
            Return RotateLeft(node)
        End If
        
        ' Return the (unchanged) node pointer
        Return node
    End Function
    
    ' In-order traversal to display tree
    Public Sub InOrderTraversal()
        Console.WriteLine("In-order traversal:")
        InOrder(Root)
        Console.WriteLine()
    End Sub
    
    Private Sub InOrder(node As AVLNode)
        If node Is Nothing Then
            Return
        End If
        
        InOrder(node.Left)
        Console.Write(node.Value & " ")
        InOrder(node.Right)
    End Sub
    
    ' Display tree structure
    Public Sub DisplayTree()
        Console.WriteLine("AVL Tree structure:")
        Display(Root, "", True)
        Console.WriteLine()
    End Sub
    
    Private Sub Display(node As AVLNode, prefix As String, isLast As Boolean)
        If node Is Nothing Then
            Return
        End If
        
        Console.Write(prefix)
        If isLast Then
            Console.Write("└── ")
            prefix &= "    "
        Else
            Console.Write("├── ")
            prefix &= "│   "
        End If
        
        Console.WriteLine(node.Value & " (h:" & node.Height & ")")
        
        If node.Left IsNot Nothing OrElse node.Right IsNot Nothing Then
            If node.Left IsNot Nothing Then
                Display(node.Left, prefix, node.Right Is Nothing)
            End If
            If node.Right IsNot Nothing Then
                Display(node.Right, prefix, True)
            End If
        End If
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        Dim avl As New AVLTree()
        
        Console.WriteLine("AVL Tree Balancing Example")
        Console.WriteLine("==========================")
        
        ' Insert values that will require balancing
        Dim values As Integer() = {10, 20, 30, 40, 50, 25}
        
        Console.WriteLine("Inserting values: " & String.Join(", ", values))
        
        For Each value As Integer In values
            avl.Insert(value)
            Console.WriteLine($"After inserting {value}:")
            avl.DisplayTree()
        Next
        
        ' Show final tree
        Console.WriteLine("Final AVL Tree:")
        avl.InOrderTraversal()
        
        ' Insert more values to test additional balancing
        Console.WriteLine(vbNewLine & "Inserting additional values: 15, 5, 35")
        avl.Insert(15)
        avl.Insert(5)
        avl.Insert(35)
        
        Console.WriteLine("Final tree structure:")
        avl.DisplayTree()
        avl.InOrderTraversal()
    End Sub
End Module
```

## Key Features of this AVL Implementation:

### 1. **Core AVL Operations:**
- **Height Calculation**: `GetHeight()` function to determine node height
- **Balance Factor**: `GetBalance()` to calculate balance factor
- **Rotation Operations**: 
  - `RotateLeft()` for left rotations
  - `RotateRight()` for right rotations
- **Insertion with Balancing**: Automatic rebalancing during insertion

### 2. **Balancing Cases:**
- **Left-Left Case**: Right rotation
- **Right-Right Case**: Left rotation  
- **Left-Right Case**: Left rotation on left child, then right rotation
- **Right-Left Case**: Right rotation on right child, then left rotation

### 3. **Visual Display:**
- `InOrderTraversal()` for sorted output
- `DisplayTree()` for visual tree structure representation

### 4. **Example Output:**
```
AVL Tree Balancing Example
==========================
Inserting values: 10, 20, 30, 40, 50, 25
After inserting 10:
└── 10 (h:1)

After inserting 20:
└── 10 (h:2)
    └── 20 (h:1)

After inserting 30:
└── 20 (h:2)
    ├── 10 (h:1)
    └── 30 (h:1)

After inserting 40:
└── 20 (h:3)
    ├── 10 (h:1)
    └── 30 (h:2)
        └── 40 (h:1)
```

This implementation demonstrates how AVL trees maintain balance through rotations after each insertion, ensuring O(log n) time complexity for all operations.

