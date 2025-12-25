# Skip List Search Algorithm in Visual Basic

```vb
Imports System
Imports System.Collections.Generic

Public Class SkipListNode
    Public Value As Integer
    Public Forward() As SkipListNode
    
    Public Sub New(value As Integer, level As Integer)
        Me.Value = value
        ReDim Forward(level)
    End Sub
End Class

Public Class SkipList
    Private head As SkipListNode
    Private maxLevel As Integer
    Private currentLevel As Integer
    Private random As Random
    
    Public Sub New(maxLevel As Integer)
        Me.maxLevel = maxLevel
        Me.currentLevel = 0
        Me.random = New Random()
        Me.head = New SkipListNode(Integer.MinValue, maxLevel)
    End Sub
    
    ' Search method
    Public Function Search(value As Integer) As Boolean
        Dim current As SkipListNode = head
        
        ' Start from the highest level and move down
        For i As Integer = currentLevel To 0 Step -1
            ' Move forward while the next node's value is less than target
            While current.Forward(i) IsNot Nothing AndAlso 
                  current.Forward(i).Value < value
                current = current.Forward(i)
            End While
        Next
        
        ' Move to the next node (this should be the node with the target value)
        current = current.Forward(0)
        
        ' Check if we found the value
        Return current IsNot Nothing AndAlso current.Value = value
    End Function
    
    ' Insert method (for completeness)
    Public Sub Insert(value As Integer)
        Dim update(maxLevel) As SkipListNode
        Dim current As SkipListNode = head
        
        ' Find the position where value should be inserted
        For i As Integer = currentLevel To 0 Step -1
            While current.Forward(i) IsNot Nothing AndAlso 
                  current.Forward(i).Value < value
                current = current.Forward(i)
            End While
            update(i) = current
        Next
        
        ' Create new node
        Dim newNode As SkipListNode = New SkipListNode(value, GetRandomLevel())
        
        ' Update level if necessary
        If newNode.Forward.Length > currentLevel Then
            currentLevel = newNode.Forward.Length - 1
        End If
        
        ' Insert the node
        For i As Integer = 0 To newNode.Forward.Length - 1
            newNode.Forward(i) = update(i).Forward(i)
            update(i).Forward(i) = newNode
        Next
    End Sub
    
    Private Function GetRandomLevel() As Integer
        Dim level As Integer = 0
        While random.NextDouble() < 0.5 AndAlso level < maxLevel
            level += 1
        End While
        Return level
    End Function
    
    ' Display the skip list (for visualization)
    Public Sub Display()
        Console.WriteLine("Skip List Structure:")
        For i As Integer = currentLevel To 0 Step -1
            Dim current As SkipListNode = head
            Console.Write("Level {0}: ", i)
            While current.Forward(i) IsNot Nothing
                Console.Write("{0} -> ", current.Forward(i).Value)
                current = current.Forward(i)
            End While
            Console.WriteLine("NIL")
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Create a skip list with max level 3
        Dim skipList As New SkipList(3)
        
        ' Insert some values
        Dim values As Integer() = {3, 6, 7, 9, 12, 19, 21, 25, 30}
        For Each value As Integer In values
            skipList.Insert(value)
        Next
        
        ' Display the skip list structure
        skipList.Display()
        Console.WriteLine()
        
        ' Search for values
        Console.WriteLine("Search Results:")
        Console.WriteLine("Search 7: {0}", skipList.Search(7))
        Console.WriteLine("Search 15: {0}", skipList.Search(15))
        Console.WriteLine("Search 25: {0}", skipList.Search(25))
        Console.WriteLine("Search 35: {0}", skipList.Search(35))
        
        Console.ReadLine()
    End Sub
End Module
```

## Search Algorithm Explanation

The skip list search algorithm works as follows:

1. **Start at the highest level** of the skip list
2. **Traverse forward** at the current level while the next node's value is less than the target
3. **Move down** to the next lower level when the target value is not found at the current level
4. **Continue until** reaching level 0
5. **Check if the target value** matches the value in the current node

## Time Complexity
- **Average case**: O(log n)
- **Worst case**: O(n)
- **Space complexity**: O(n log n)

## Key Features
- Uses multiple levels of linked lists for faster searching
- Probabilistic approach to maintain balance
- Efficient search, insertion, and deletion operations
- Visual representation of the skip list structure

