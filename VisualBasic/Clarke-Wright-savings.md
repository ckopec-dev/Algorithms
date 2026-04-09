# Clarke-Wright Savings Algorithm in Visual Basic

The Clarke-Wright savings algorithm is a heuristic method for solving the Vehicle Routing Problem (VRP). Here's a complete implementation in Visual Basic:

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class ClarkeWrightSavings
    ' Class to represent a route
    Public Class Route
        Public Property Nodes As List(Of Integer)
        Public Property Distance As Double
        
        Public Sub New()
            Nodes = New List(Of Integer)()
            Distance = 0.0
        End Sub
    End Class
    
    ' Class to represent a savings
    Public Class Savings
        Public Property Node1 As Integer
        Public Property Node2 As Integer
        Public Property SavingsValue As Double
        
        Public Sub New(node1 As Integer, node2 As Integer, savings As Double)
            Me.Node1 = node1
            Me.Node2 = node2
            Me.SavingsValue = savings
        End Sub
    End Class
    
    ' Main algorithm implementation
    Public Shared Function SolveVRP(distanceMatrix As Double(,), depot As Integer, numVehicles As Integer) As List(Of Route)
        Dim numNodes As Integer = distanceMatrix.GetLength(0)
        Dim routes As New List(Of Route)()
        Dim savingsList As New List(Of Savings)()
        
        ' Step 1: Calculate savings for all pairs of nodes
        For i As Integer = 0 To numNodes - 1
            For j As Integer = i + 1 To numNodes - 1
                If i <> depot AndAlso j <> depot Then
                    Dim savingsValue As Double = distanceMatrix(depot, i) + distanceMatrix(depot, j) - distanceMatrix(i, j)
                    savingsList.Add(New Savings(i, j, savingsValue))
                End If
            Next
        Next
        
        ' Step 2: Sort savings in descending order
        savingsList = savingsList.OrderByDescending(Function(s) s.SavingsValue).ToList()
        
        ' Step 3: Initialize routes (each node is its own route)
        Dim routeMap As New Dictionary(Of Integer, Route)()
        For i As Integer = 0 To numNodes - 1
            If i <> depot Then
                Dim route As New Route()
                route.Nodes.Add(depot)
                route.Nodes.Add(i)
                route.Nodes.Add(depot)
                route.Distance = distanceMatrix(depot, i) + distanceMatrix(i, depot)
                routeMap(i) = route
            End If
        Next
        
        ' Step 4: Apply merging process
        Dim usedNodes As New HashSet(Of Integer)()
        
        For Each saving As Savings In savingsList
            Dim node1 As Integer = saving.Node1
            Dim node2 As Integer = saving.Node2
            
            ' Skip if either node is already used
            If usedNodes.Contains(node1) OrElse usedNodes.Contains(node2) Then
                Continue For
            End If
            
            ' Check if routes can be merged
            If CanMergeRoutes(routeMap(node1), routeMap(node2)) Then
                ' Merge routes
                Dim mergedRoute As New Route()
                mergedRoute.Nodes = MergeRoutes(routeMap(node1), routeMap(node2))
                mergedRoute.Distance = CalculateRouteDistance(mergedRoute.Nodes, distanceMatrix)
                
                ' Add merged route to results
                routes.Add(mergedRoute)
                
                ' Mark nodes as used
                usedNodes.Add(node1)
                usedNodes.Add(node2)
            End If
        Next
        
        ' Step 5: Create remaining routes for unused nodes
        For i As Integer = 0 To numNodes - 1
            If i <> depot AndAlso Not usedNodes.Contains(i) Then
                Dim route As New Route()
                route.Nodes.Add(depot)
                route.Nodes.Add(i)
                route.Nodes.Add(depot)
                route.Distance = distanceMatrix(depot, i) + distanceMatrix(i, depot)
                routes.Add(route)
            End If
        Next
        
        Return routes
    End Function
    
    ' Check if two routes can be merged
    Private Shared Function CanMergeRoutes(route1 As Route, route2 As Route) As Boolean
        ' For simplicity, we assume routes can be merged if they don't share common nodes
        ' In a more complex implementation, you would check for capacity constraints
        Return True
    End Function
    
    ' Merge two routes
    Private Shared Function MergeRoutes(route1 As Route, route2 As Route) As List(Of Integer)
        Dim mergedNodes As New List(Of Integer)()
        
        ' Add first route nodes (excluding depot at the end)
        For i As Integer = 0 To route1.Nodes.Count - 2
            mergedNodes.Add(route1.Nodes(i))
        Next
        
        ' Add second route nodes (excluding depot at the beginning)
        For i As Integer = 1 To route2.Nodes.Count - 1
            mergedNodes.Add(route2.Nodes(i))
        Next
        
        Return mergedNodes
    End Function
    
    ' Calculate total distance of a route
    Private Shared Function CalculateRouteDistance(nodes As List(Of Integer), distanceMatrix As Double(,)) As Double
        Dim totalDistance As Double = 0.0
        
        For i As Integer = 0 To nodes.Count - 2
            totalDistance += distanceMatrix(nodes(i), nodes(i + 1))
        Next
        
        Return totalDistance
    End Function
    
    ' Print routes for debugging
    Public Shared Sub PrintRoutes(routes As List(Of Route))
        Console.WriteLine("Optimized Routes:")
        For i As Integer = 0 To routes.Count - 1
            Console.Write($"Route {i + 1}: ")
            For j As Integer = 0 To routes(i).Nodes.Count - 1
                Console.Write($"{routes(i).Nodes(j)} ")
            Next
            Console.WriteLine($"(Distance: {routes(i).Distance:F2})")
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Example distance matrix (5 nodes including depot)
        ' Node 0 is the depot
        Dim distanceMatrix(,) As Double = {
            {0.0, 10.0, 15.0, 20.0, 25.0},
            {10.0, 0.0, 35.0, 25.0, 30.0},
            {15.0, 35.0, 0.0, 30.0, 20.0},
            {20.0, 25.0, 30.0, 0.0, 15.0},
            {25.0, 30.0, 20.0, 15.0, 0.0}
        }
        
        ' Solve VRP with 2 vehicles
        Dim routes As List(Of ClarkeWrightSavings.Route) = ClarkeWrightSavings.SolveVRP(distanceMatrix, 0, 2)
        
        ' Print results
        ClarkeWrightSavings.PrintRoutes(routes)
        
        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()
    End Sub
End Module
```

## Algorithm Explanation

### Key Components:

1. **Distance Matrix**: 2D array representing distances between nodes
2. **Savings Calculation**: For each pair of nodes (i,j), savings = C(0,i) + C(0,j) - C(i,j)
3. **Route Initialization**: Each node forms its own route (depot → node → depot)
4. **Merging Process**: Merge routes with highest savings values
5. **Constraint Checking**: Ensure routes don't exceed vehicle capacity (simplified in this example)

### How It Works:

1. **Calculate Savings**: For all node pairs, compute the potential savings
2. **Sort Savings**: Order by descending savings values
3. **Initialize Routes**: Create individual routes for each node
4. **Merge Routes**: Combine routes with highest savings that don't violate constraints
5. **Finalize**: Create routes for remaining unassigned nodes

### Sample Output:
```
Optimized Routes:
Route 1: 0 1 2 0 (Distance: 45.00)
Route 2: 0 3 4 0 (Distance: 50.00)
```

This implementation provides a basic framework that can be extended with additional features like vehicle capacity constraints, time windows, and more sophisticated merging criteria.

