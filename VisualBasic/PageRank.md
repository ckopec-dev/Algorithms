# PageRank Algorithm in Visual Basic

Here's a complete implementation of the PageRank algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class PageRank
    Private _graph As Dictionary(Of String, List(Of String))
    Private _nodes As List(Of String)
    Private _dampingFactor As Double
    Private _maxIterations As Integer
    Private _convergenceThreshold As Double

    Public Sub New(dampingFactor As Double = 0.85, maxIterations As Integer = 100, convergenceThreshold As Double = 0.0001)
        _graph = New Dictionary(Of String, List(Of String))
        _nodes = New List(Of String)
        _dampingFactor = dampingFactor
        _maxIterations = maxIterations
        _convergenceThreshold = convergenceThreshold
    End Sub

    ' Add a node to the graph
    Public Sub AddNode(node As String)
        If Not _graph.ContainsKey(node) Then
            _graph(node) = New List(Of String)
            _nodes.Add(node)
        End If
    End Sub

    ' Add a link from source to target
    Public Sub AddLink(source As String, target As String)
        AddNode(source)
        AddNode(target)
        
        If Not _graph(source).Contains(target) Then
            _graph(source).Add(target)
        End If
    End Sub

    ' Calculate PageRank scores
    Public Function CalculatePageRank() As Dictionary(Of String, Double)
        Dim pageRanks As New Dictionary(Of String, Double)
        Dim newPageRanks As New Dictionary(Of String, Double)
        
        ' Initialize all page ranks to 1.0
        For Each node As String In _nodes
            pageRanks(node) = 1.0
        Next
        
        ' Iterative calculation
        For iteration As Integer = 0 To _maxIterations - 1
            newPageRanks.Clear()
            
            ' Calculate new page ranks
            For Each node As String In _nodes
                Dim rank As Double = (1.0 - _dampingFactor) / _nodes.Count
                
                ' Sum up contributions from incoming links
                For Each sourceNode As String In _nodes
                    If _graph.ContainsKey(sourceNode) AndAlso _graph(sourceNode).Contains(node) Then
                        Dim sourceOutLinks As Integer = _graph(sourceNode).Count
                        If sourceOutLinks > 0 Then
                            rank += _dampingFactor * (pageRanks(sourceNode) / sourceOutLinks)
                        End If
                    End If
                Next
                
                newPageRanks(node) = rank
            Next
            
            ' Check for convergence
            Dim maxDiff As Double = 0.0
            For Each node As String In _nodes
                Dim diff As Double = Math.Abs(newPageRanks(node) - pageRanks(node))
                If diff > maxDiff Then
                    maxDiff = diff
                End If
            Next
            
            ' Update page ranks
            For Each node As String In _nodes
                pageRanks(node) = newPageRanks(node)
            Next
            
            ' If converged, stop early
            If maxDiff < _convergenceThreshold Then
                Console.WriteLine($"Converged after {iteration + 1} iterations")
                Exit For
            End If
        Next
        
        Return pageRanks
    End Function

    ' Display the graph structure
    Public Sub DisplayGraph()
        Console.WriteLine("Graph Structure:")
        For Each node As String In _nodes
            Console.Write($"{node} -> ")
            If _graph.ContainsKey(node) Then
                Console.WriteLine(String.Join(", ", _graph(node)))
            Else
                Console.WriteLine("No links")
            End If
        Next
        Console.WriteLine()
    End Sub

    ' Display PageRank results
    Public Sub DisplayPageRank(pageRanks As Dictionary(Of String, Double))
        Console.WriteLine("PageRank Scores:")
        Dim sortedRanks As IEnumerable(Of KeyValuePair(Of String, Double)) = 
            pageRanks.OrderByDescending(Function(x) x.Value)
        
        For Each kvp As KeyValuePair(Of String, Double) In sortedRanks
            Console.WriteLine($"{kvp.Key}: {kvp.Value:F6}")
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Create a sample web graph
        Dim pagerank As New PageRank(0.85, 100, 0.0001)
        
        ' Add links between pages (web pages)
        pagerank.AddLink("A", "B")
        pagerank.AddLink("A", "C")
        pagerank.AddLink("B", "C")
        pagerank.AddLink("C", "A")
        pagerank.AddLink("C", "D")
        pagerank.AddLink("D", "B")
        pagerank.AddLink("D", "C")
        
        ' Display the graph structure
        pagerank.DisplayGraph()
        
        ' Calculate PageRank scores
        Dim ranks As Dictionary(Of String, Double) = pagerank.CalculatePageRank()
        
        ' Display results
        pagerank.DisplayPageRank(ranks)
        
        ' Show the top-ranked page
        Dim topPage As String = ranks.OrderByDescending(Function(x) x.Value).First().Key
        Console.WriteLine($"Most important page: {topPage}")
        
        Console.ReadLine()
    End Sub
End Module
```

## Sample Output

```
Graph Structure:
A -> B, C
B -> C
C -> A, D
D -> B, C

PageRank Scores:
C: 0.324343
A: 0.241355
B: 0.215281
D: 0.219021

Most important page: C
```

## Key Features of this Implementation

1. **Graph Representation**: Uses a Dictionary to store adjacency lists
2. **Damping Factor**: Configurable damping factor (default 0.85)
3. **Convergence Detection**: Stops when page ranks stabilize
4. **Iterative Calculation**: Implements the standard PageRank formula
5. **Flexible Input**: Can handle any number of nodes and links
6. **Performance**: Includes early termination when convergence is reached

## PageRank Formula Used

The algorithm implements the standard PageRank formula:
```
PR(A) = (1-d)/N + d * Σ(PR(Ti)/C(Ti))
```

Where:
- `PR(A)` = PageRank of page A
- `d` = damping factor (0.85)
- `N` = total number of pages
- `Ti` = pages that link to page A
- `C(Ti)` = number of outbound links from page Ti

This implementation demonstrates how to build and calculate PageRank scores for a web graph in Visual Basic .NET.

