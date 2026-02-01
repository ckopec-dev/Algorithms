# Union-Find Algorithm in Visual Basic

Here's a complete implementation of the Union-Find (Disjoint Set Union) algorithm in Visual Basic .NET:

```vb
Public Class UnionFind
    Private parent() As Integer
    Private rank() As Integer
    Private count As Integer

    ' Constructor to initialize the Union-Find structure
    Public Sub New(size As Integer)
        ReDim parent(size - 1)
        ReDim rank(size - 1)
        count = size
        
        ' Initialize each element to be its own parent
        For i As Integer = 0 To size - 1
            parent(i) = i
            rank(i) = 0
        Next
    End Sub

    ' Find operation with path compression
    Public Function Find(element As Integer) As Integer
        If parent(element) <> element Then
            ' Path compression: make every node point directly to root
            parent(element) = Find(parent(element))
        End If
        Return parent(element)
    End Function

    ' Union operation with union by rank
    Public Sub Union(element1 As Integer, element2 As Integer)
        Dim root1 As Integer = Find(element1)
        Dim root2 As Integer = Find(element2)

        If root1 <> root2 Then
            ' Union by rank: attach smaller tree under root of larger tree
            If rank(root1) < rank(root2) Then
                parent(root1) = root2
            ElseIf rank(root1) > rank(root2) Then
                parent(root2) = root1
            Else
                ' If ranks are equal, choose one as root and increment its rank
                parent(root2) = root1
                rank(root1) += 1
            End If
            count -= 1
        End If
    End Sub

    ' Get the number of connected components
    Public Function GetCount() As Integer
        Return count
    End Function

    ' Check if two elements are connected
    Public Function IsConnected(element1 As Integer, element2 As Integer) As Boolean
        Return Find(element1) = Find(element2)
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Create a Union-Find structure with 6 elements (0-5)
        Dim uf As New UnionFind(6)
        
        Console.WriteLine("Initial state - Number of components: " & uf.GetCount())
        
        ' Perform some union operations
        uf.Union(0, 1)
        Console.WriteLine("After union(0,1) - Number of components: " & uf.GetCount())
        
        uf.Union(2, 3)
        Console.WriteLine("After union(2,3) - Number of components: " & uf.GetCount())
        
        uf.Union(4, 5)
        Console.WriteLine("After union(4,5) - Number of components: " & uf.GetCount())
        
        uf.Union(1, 3)
        Console.WriteLine("After union(1,3) - Number of components: " & uf.GetCount())
        
        ' Check connections
        Console.WriteLine("Is 0 connected to 3? " & uf.IsConnected(0, 3))
        Console.WriteLine("Is 0 connected to 5? " & uf.IsConnected(0, 5))
        
        ' Find operations
        Console.WriteLine("Root of 0: " & uf.Find(0))
        Console.WriteLine("Root of 4: " & uf.Find(4))
        
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Output:
```
Initial state - Number of components: 6
After union(0,1) - Number of components: 5
After union(2,3) - Number of components: 4
After union(4,5) - Number of components: 3
After union(1,3) - Number of components: 2
Is 0 connected to 3? True
Is 0 connected to 5? False
Root of 0: 0
Root of 4: 4
Press any key to exit...
```

## Key Features:

1. **Path Compression**: The `Find` method uses path compression to optimize future operations
2. **Union by Rank**: The `Union` method uses union by rank to keep the tree balanced
3. **Efficient Operations**: Both `Find` and `Union` operations have nearly constant time complexity
4. **Component Counting**: Tracks the number of connected components

## Time Complexity:
- Find operation: O(α(n)) where α is the inverse Ackermann function (practically constant)
- Union operation: O(α(n))
- Space complexity: O(n)

This implementation is commonly used in graph algorithms, such as finding connected components or implementing Kruskal's algorithm for minimum spanning trees.

