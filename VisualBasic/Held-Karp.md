# Held-Karp Algorithm Implementation in Visual Basic

The Held-Karp algorithm is used to solve the Traveling Salesman Problem (TSP) using dynamic programming. Here's a complete implementation in Visual Basic:

```vb
Imports System
Imports System.Collections.Generic

Public Class HeldKarpTSP
    Private distances As Integer(,)
    Private n As Integer
    Private memo As Dictionary(Of String, Integer)
    
    Public Sub New(ByVal distanceMatrix As Integer(,))
        Me.distances = distanceMatrix
        Me.n = distanceMatrix.GetLength(0)
        Me.memo = New Dictionary(Of String, Integer)
    End Sub
    
    ' Main function to solve TSP using Held-Karp algorithm
    Public Function Solve() As Integer
        ' Start from city 0 and visit all other cities
        Return TSP(1, 0)
    End Function
    
    ' Recursive function with memoization
    Private Function TSP(ByVal mask As Integer, ByVal pos As Integer) As Integer
        ' Base case: if all cities are visited
        If mask = (1 << n) - 1 Then
            Return distances(pos, 0) ' Return to starting city
        End If
        
        ' Create key for memoization
        Dim key As String = mask.ToString() & "," & pos.ToString()
        
        ' Check if already computed
        If memo.ContainsKey(key) Then
            Return memo(key)
        End If
        
        Dim minCost As Integer = Integer.MaxValue
        Dim nextCity As Integer
        
        ' Try visiting each unvisited city
        For i As Integer = 0 To n - 1
            ' If city i is not visited
            If (mask And (1 << i)) = 0 Then
                Dim cost As Integer = distances(pos, i) + TSP(mask Or (1 << i), i)
                If cost < minCost Then
                    minCost = cost
                    nextCity = i
                End If
            End If
        Next
        
        memo(key) = minCost
        Return minCost
    End Function
    
    ' Function to get the actual path (not just cost)
    Public Function GetPath() As List(Of Integer)
        Dim path As New List(Of Integer)
        Dim mask As Integer = 1
        Dim pos As Integer = 0
        path.Add(0) ' Start from city 0
        
        ' Reconstruct the path
        While path.Count < n
            Dim minCost As Integer = Integer.MaxValue
            Dim nextCity As Integer = -1
            
            For i As Integer = 0 To n - 1
                If (mask And (1 << i)) = 0 Then
                    Dim cost As Integer = distances(pos, i)
                    If cost < minCost Then
                        minCost = cost
                        nextCity = i
                    End If
                End If
            Next
            
            If nextCity <> -1 Then
                mask = mask Or (1 << nextCity)
                pos = nextCity
                path.Add(pos)
            End If
        End While
        
        path.Add(0) ' Return to starting city
        Return path
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Example distance matrix (5 cities)
        Dim distanceMatrix(,) As Integer = {
            {0, 10, 15, 20, 25},
            {10, 0, 35, 25, 30},
            {15, 35, 0, 30, 20},
            {20, 25, 30, 0, 15},
            {25, 30, 20, 15, 0}
        }
        
        Console.WriteLine("Distance Matrix:")
        For i As Integer = 0 To 4
            For j As Integer = 0 To 4
                Console.Write(distanceMatrix(i, j).ToString().PadLeft(4))
            Next
            Console.WriteLine()
        Next
        
        ' Solve TSP using Held-Karp
        Dim tsp As New HeldKarpTSP(distanceMatrix)
        Dim minCost As Integer = tsp.Solve()
        
        Console.WriteLine(vbNewLine & "Minimum cost: " & minCost)
        
        ' Get the actual path
        Dim path As List(Of Integer) = tsp.GetPath()
        Console.Write("Optimal path: ")
        For i As Integer = 0 To path.Count - 1
            Console.Write(path(i))
            If i < path.Count - 1 Then
                Console.Write(" -> ")
            End If
        Next
        Console.WriteLine()
        
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## How the Algorithm Works

1. **Dynamic Programming Approach**: Uses bitmask representation to track which cities have been visited
2. **Memoization**: Stores computed results to avoid redundant calculations
3. **State Representation**: `mask` represents visited cities, `pos` represents current city
4. **Recurrence Relation**: For each state, try all unvisited cities and take the minimum cost

## Key Components

- **`TSP` function**: Recursive function with memoization
- **`memo` dictionary**: Stores computed results for reuse
- **Bitmask operations**: Efficient way to track visited cities
- **`GetPath` function**: Reconstructs the actual optimal path

## Time and Space Complexity

- **Time Complexity**: O(n² × 2ⁿ)
- **Space Complexity**: O(n × 2ⁿ)

This implementation efficiently solves the TSP for small to medium-sized problems (typically up to 20-25 cities).

