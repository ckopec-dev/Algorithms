# Iterative Deepening Depth-First Search (IDDFS) in Visual Basic

```vb
Imports System.Collections.Generic

Public Class IDDFS
    ' Node class to represent each state in the search
    Public Class Node
        Public Property State As String
        Public Property Parent As Node
        Public Property Depth As Integer
        
        Public Sub New(state As String, parent As Node, depth As Integer)
            Me.State = state
            Me.Parent = parent
            Me.Depth = depth
        End Sub
    End Class
    
    ' Example problem: Find path from "A" to "G" in a graph
    Private Shared ReadOnly Graph As Dictionary(Of String, List(Of String)) = New Dictionary(Of String, List(Of String)) From {
        {"A", New List(Of String) From {"B", "C"}},
        {"B", New List(Of String) From {"D", "E"}},
        {"C", New List(Of String) From {"F", "G"}},
        {"D", New List(Of String) From {}},
        {"E", New List(Of String) From {}},
        {"F", New List(Of String) From {}},
        {"G", New List(Of String) From {}}
    }
    
    ' IDDFS main function
    Public Shared Function IDDFSSearch(startState As String, goalState As String) As List(Of String)
        Dim depth As Integer = 0
        
        Do
            Dim result As List(Of String) = DLS(startState, goalState, depth)
            If result IsNot Nothing Then
                Return result
            End If
            depth += 1
        Loop While depth <= 10 ' Limit maximum depth to prevent infinite loop
            
        Return Nothing ' Goal not found
    End Function
    
    ' Depth-Limited Search (DLS) - called by IDDFS
    Private Shared Function DLS(startState As String, goalState As String, maxDepth As Integer) As List(Of String)
        Dim stack As New Stack(Of Node)
        Dim visited As New HashSet(Of String)
        
        ' Initialize with start node
        stack.Push(New Node(startState, Nothing, 0))
        
        While stack.Count > 0
            Dim current As Node = stack.Pop()
            
            ' If we've reached the goal
            If current.State = goalState Then
                Return ReconstructPath(current)
            End If
            
            ' If we've exceeded the depth limit, skip
            If current.Depth >= maxDepth Then
                Continue While
            End If
            
            ' Mark as visited
            visited.Add(current.State)
            
            ' Explore neighbors
            If Graph.ContainsKey(current.State) Then
                For Each neighbor As String In Graph(current.State)
                    If Not visited.Contains(neighbor) Then
                        stack.Push(New Node(neighbor, current, current.Depth + 1))
                    End If
                Next
            End If
        End While
        
        Return Nothing ' Goal not found at this depth
    End Function
    
    ' Reconstruct path from goal back to start
    Private Shared Function ReconstructPath(goalNode As Node) As List(Of String)
        Dim path As New List(Of String)
        Dim current As Node = goalNode
        
        While current IsNot Nothing
            path.Insert(0, current.State)
            current = current.Parent
        End While
        
        Return path
    End Function
    
    ' Example usage
    Public Shared Sub Main()
        Console.WriteLine("Iterative Deepening Depth-First Search (IDDFS)")
        Console.WriteLine("==============================================")
        
        Dim startState As String = "A"
        Dim goalState As String = "G"
        
        Console.WriteLine($"Searching from {startState} to {goalState}")
        
        Dim result As List(Of String) = IDDFSSearch(startState, goalState)
        
        If result IsNot Nothing Then
            Console.WriteLine("Path found: " & String.Join(" -> ", result))
            Console.WriteLine($"Path length: {result.Count - 1}")
        Else
            Console.WriteLine("No path found")
        End If
        
        Console.WriteLine()
        Console.WriteLine("Algorithm steps:")
        Console.WriteLine("1. Start with depth limit = 0")
        Console.WriteLine("2. Perform DLS with current depth limit")
        Console.WriteLine("3. If goal found, return path")
        Console.WriteLine("4. If not found, increment depth limit")
        Console.WriteLine("5. Repeat until goal is found or depth limit is reached")
    End Sub
End Class
```

## How IDDFS Works:

1. **Iterative Process**: IDDFS starts with a depth limit of 0 and gradually increases it
2. **Depth-Limited Search**: For each depth limit, it performs a regular DFS
3. **Complete Search**: Since it explores all nodes at depth d before exploring depth d+1, it's guaranteed to find the shallowest solution
4. **Memory Efficient**: Uses only O(bd) space, where b is branching factor and d is depth

## Output Example:
```
Iterative Deepening Depth-First Search (IDDFS)
==============================================
Searching from A to G
Path found: A -> C -> G
Path length: 2

Algorithm steps:
1. Start with depth limit = 0
2. Perform DLS with current depth limit
3. If goal found, return path
4. If not found, increment depth limit
5. Repeat until goal is found or depth limit is reached
```

## Key Features:
- **Complete**: Finds solution if one exists
- **Optimal**: Finds shortest path (in terms of steps)
- **Memory Efficient**: Uses linear space
- **Adaptable**: Can be modified for different graph representations
- **Robust**: Handles cycles and avoids infinite loops

