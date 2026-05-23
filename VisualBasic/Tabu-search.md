# Tabu Search Algorithm in Visual Basic

Here's a complete implementation of a Tabu Search algorithm in Visual Basic for solving the Traveling Salesman Problem (TSP):

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Public Class TabuSearch
    Private tabuList As New List(Of String)
    Private tabuTenure As Integer
    Private maxIterations As Integer
    Private distanceMatrix()() As Double
    Private numberOfCities As Integer
    
    Public Sub New(distanceMatrix()() As Double, tabuTenure As Integer, maxIterations As Integer)
        Me.distanceMatrix = distanceMatrix
        Me.numberOfCities = distanceMatrix.GetLength(0)
        Me.tabuTenure = tabuTenure
        Me.maxIterations = maxIterations
    End Sub
    
    ' Generate initial solution (random tour)
    Private Function GenerateInitialSolution() As List(Of Integer)
        Dim solution As New List(Of Integer)
        For i As Integer = 0 To numberOfCities - 1
            solution.Add(i)
        Next
        
        ' Shuffle the solution
        Dim random As New Random()
        For i As Integer = solution.Count - 1 To 1 Step -1
            Dim j As Integer = random.Next(i + 1)
            Dim temp As Integer = solution(i)
            solution(i) = solution(j)
            solution(j) = temp
        Next
        
        Return solution
    End Function
    
    ' Calculate total distance of a tour
    Private Function CalculateTotalDistance(tour() As Integer) As Double
        Dim totalDistance As Double = 0
        For i As Integer = 0 To tour.Length - 2
            totalDistance += distanceMatrix(tour(i))(tour(i + 1))
        Next
        ' Return to starting city
        totalDistance += distanceMatrix(tour(tour.Length - 1))(tour(0))
        Return totalDistance
    End Function
    
    ' Generate neighborhood by swapping two cities
    Private Function GenerateNeighborhood(currentTour() As Integer) As List(Of Integer())
        Dim neighborhood As New List(Of Integer())
        
        For i As Integer = 0 To currentTour.Length - 1
            For j As Integer = i + 1 To currentTour.Length - 1
                ' Create a new tour by swapping cities at positions i and j
                Dim newTour As Integer() = currentTour.Clone()
                Dim temp As Integer = newTour(i)
                newTour(i) = newTour(j)
                newTour(j) = temp
                
                neighborhood.Add(newTour)
            Next
        Next
        
        Return neighborhood
    End Function
    
    ' Check if a move is in the tabu list
    Private Function IsTabu(move() As Integer) As Boolean
        Dim moveKey As String = String.Join("-", move)
        Return tabuList.Contains(moveKey)
    End Function
    
    ' Add move to tabu list
    Private Sub AddToTabu(move() As Integer)
        Dim moveKey As String = String.Join("-", move)
        tabuList.Add(moveKey)
        
        ' Remove oldest entry if tabu list exceeds tenure
        If tabuList.Count > tabuTenure Then
            tabuList.RemoveAt(0)
        End If
    End Sub
    
    ' Main Tabu Search algorithm
    Public Function Solve() As Tuple(Of List(Of Integer), Double)
        ' Initialize
        Dim currentSolution As List(Of Integer) = GenerateInitialSolution()
        Dim bestSolution As List(Of Integer) = currentSolution.Clone()
        Dim bestDistance As Double = CalculateTotalDistance(currentSolution.ToArray())
        Dim iteration As Integer = 0
        
        Do While iteration < maxIterations
            ' Generate neighborhood
            Dim neighborhood As List(Of Integer()) = GenerateNeighborhood(currentSolution.ToArray())
            
            Dim bestNeighbor As Integer() = Nothing
            Dim bestNeighborDistance As Double = Double.MaxValue
            Dim isBestTabu As Boolean = False
            
            ' Find best non-tabu neighbor
            For Each neighbor As Integer() In neighborhood
                Dim neighborDistance As Double = CalculateTotalDistance(neighbor)
                
                ' Check if neighbor is better than current best
                If neighborDistance < bestNeighborDistance Then
                    ' Check if move is tabu
                    If Not IsTabu(neighbor) Then
                        bestNeighbor = neighbor
                        bestNeighborDistance = neighborDistance
                        isBestTabu = False
                    ElseIf neighborDistance < bestDistance Then
                        ' Allow aspiration criterion - accept tabu move if it's better than best solution
                        bestNeighbor = neighbor
                        bestNeighborDistance = neighborDistance
                        isBestTabu = True
                    End If
                End If
            Next
            
            ' If no good neighbor found, continue to next iteration
            If bestNeighbor Is Nothing Then
                iteration += 1
                Continue Do
            End If
            
            ' Update current solution
            currentSolution = bestNeighbor.ToList()
            
            ' Update best solution if needed
            If bestNeighborDistance < bestDistance Then
                bestSolution = currentSolution.Clone()
                bestDistance = bestNeighborDistance
            End If
            
            ' Add move to tabu list
            AddToTabu(bestNeighbor)
            
            iteration += 1
        Loop
        
        Return New Tuple(Of List(Of Integer), Double)(bestSolution, bestDistance)
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Example distance matrix for 5 cities (0-4)
        Dim distanceMatrix(,) As Double = {
            {0, 10, 15, 20, 25},
            {10, 0, 35, 25, 30},
            {15, 35, 0, 30, 20},
            {20, 25, 30, 0, 15},
            {25, 30, 20, 15, 0}
        }
        
        ' Convert 2D array to jagged array for VB.NET
        Dim jaggedDistanceMatrix()() As Double = New Double(distanceMatrix.GetLength(0) - 1)() {}
        For i As Integer = 0 To distanceMatrix.GetLength(0) - 1
            jaggedDistanceMatrix(i) = New Double(distanceMatrix.GetLength(1) - 1) {}
            For j As Integer = 0 To distanceMatrix.GetLength(1) - 1
                jaggedDistanceMatrix(i)(j) = distanceMatrix(i, j)
            Next
        Next
        
        ' Create and run Tabu Search
        Dim tabuSearch As New TabuSearch(jaggedDistanceMatrix, 5, 1000)
        Dim result As Tuple(Of List(Of Integer), Double) = tabuSearch.Solve()
        
        ' Display results
        Console.WriteLine("Best Tour: " & String.Join(" -> ", result.Item1))
        Console.WriteLine("Total Distance: " & result.Item2.ToString("F2"))
        
        Console.ReadLine()
    End Sub
End Module
```

## Key Components of the Tabu Search Implementation:

### 1. **Tabu List Management**
- Maintains a list of recently visited solutions
- Implements tenure-based removal of entries
- Prevents cycling back to recent solutions

### 2. **Neighborhood Generation**
- Uses 2-opt swaps to generate neighboring solutions
- Each swap creates a new tour by exchanging two cities

### 3. **Tabu Criteria**
- Checks if a move is in the tabu list
- Implements aspiration criteria to accept better solutions

### 4. **Main Algorithm Flow**
1. Generate initial random solution
2. Generate neighborhood of current solution
3. Evaluate neighbors and select best non-tabu move
4. Update tabu list with selected move
5. Continue until maximum iterations reached

## Parameters:
- **Tabu Tenure**: Number of iterations a move stays in tabu list
- **Max Iterations**: Maximum number of iterations to run
- **Distance Matrix**: Pre-computed distances between cities

## Output:
The algorithm returns the best tour found and its total distance, demonstrating how Tabu Search escapes local optima through memory-based mechanisms.

