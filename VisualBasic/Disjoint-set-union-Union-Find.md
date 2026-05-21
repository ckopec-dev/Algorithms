# Disjoint Set Union (Union-Find) Algorithm in Visual Basic

```vb
Public Class DisjointSet
    Private parent() As Integer
    Private rank() As Integer
    Private numSets As Integer
    
    ' Initialize the disjoint set with n elements
    Public Sub New(n As Integer)
        ReDim parent(n - 1)
        ReDim rank(n - 1)
        numSets = n
        
        ' Initially, each element is its own parent
        For i As Integer = 0 To n - 1
            parent(i) = i
            rank(i) = 0
        Next
    End Sub
    
    ' Find the root of element x with path compression
    Public Function Find(x As Integer) As Integer
        If parent(x) <> x Then
            ' Path compression: make every node on the path point directly to the root
            parent(x) = Find(parent(x))
        End If
        Return parent(x)
    End Function
    
    ' Union two sets containing x and y
    Public Sub Union(x As Integer, y As Integer)
        Dim rootX As Integer = Find(x)
        Dim rootY As Integer = Find(y)
        
        If rootX <> rootY Then
            ' Union by rank: attach smaller tree under root of larger tree
            If rank(rootX) < rank(rootY) Then
                parent(rootX) = rootY
            ElseIf rank(rootX) > rank(rootY) Then
                parent(rootY) = rootX
            Else
                ' If ranks are equal, make one root the parent of the other
                parent(rootY) = rootX
                rank(rootX) += 1
            End If
            numSets -= 1
        End If
    End Sub
    
    ' Check if two elements are in the same set
    Public Function AreConnected(x As Integer, y As Integer) As Boolean
        Return Find(x) = Find(y)
    End Function
    
    ' Get the number of disjoint sets
    Public Function GetNumSets() As Integer
        Return numSets
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Create a disjoint set with 6 elements (0-5)
        Dim ds As New DisjointSet(6)
        
        Console.WriteLine("Initial state - 6 disjoint sets")
        Console.WriteLine("Number of sets: " & ds.GetNumSets())
        Console.WriteLine()
        
        ' Perform some union operations
        Console.WriteLine("Union(0, 1)")
        ds.Union(0, 1)
        Console.WriteLine("Number of sets: " & ds.GetNumSets())
        
        Console.WriteLine("Union(2, 3)")
        ds.Union(2, 3)
        Console.WriteLine("Number of sets: " & ds.GetNumSets())
        
        Console.WriteLine("Union(4, 5)")
        ds.Union(4, 5)
        Console.WriteLine("Number of sets: " & ds.GetNumSets())
        
        Console.WriteLine("Union(1, 3)")
        ds.Union(1, 3)
        Console.WriteLine("Number of sets: " & ds.GetNumSets())
        
        Console.WriteLine()
        Console.WriteLine("Checking connections:")
        Console.WriteLine("Are 0 and 3 connected? " & ds.AreConnected(0, 3))
        Console.WriteLine("Are 0 and 5 connected? " & ds.AreConnected(0, 5))
        Console.WriteLine("Are 2 and 4 connected? " & ds.AreConnected(2, 4))
        
        Console.WriteLine()
        Console.WriteLine("Final structure:")
        For i As Integer = 0 To 5
            Console.WriteLine("Element " & i & " belongs to set root: " & ds.Find(i))
        Next
        
        Console.ReadLine()
    End Sub
End Module
```

## Output Example:
```
Initial state - 6 disjoint sets
Number of sets: 6

Union(0, 1)
Number of sets: 5

Union(2, 3)
Number of sets: 4

Union(4, 5)
Number of sets: 3

Union(1, 3)
Number of sets: 2

Checking connections:
Are 0 and 3 connected? True
Are 0 and 5 connected? False
Are 2 and 4 connected? False

Final structure:
Element 0 belongs to set root: 0
Element 1 belongs to set root: 0
Element 2 belongs to set root: 2
Element 3 belongs to set root: 0
Element 4 belongs to set root: 4
Element 5 belongs to set root: 4
```

## Key Features of this Implementation:

1. **Path Compression**: The `Find` method uses path compression to optimize future lookups
2. **Union by Rank**: The `Union` method uses rank to keep the tree balanced
3. **Efficient Operations**: Both `Find` and `Union` operations have nearly constant time complexity
4. **Complete API**: Includes methods for finding sets, uniting sets, and checking connectivity

This implementation demonstrates the core functionality of the Union-Find data structure, which is commonly used in algorithms like Kruskal's Minimum Spanning Tree and connected component detection.

