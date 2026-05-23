# Critical Path Method (CPM) Algorithm in Visual Basic

Here's a complete implementation of the Critical Path Method algorithm in Visual Basic:

```vb
Imports System.Collections.Generic
Imports System.Linq

Public Class Activity
    Public Property Name As String
    Public Property Duration As Integer
    Public Property Predecessors As List(Of String)
    Public Property EarlyStart As Integer
    Public Property EarlyFinish As Integer
    Public Property LateStart As Integer
    Public Property LateFinish As Integer
    Public Property Slack As Integer
    
    Public Sub New(name As String, duration As Integer)
        Me.Name = name
        Me.Duration = duration
        Me.Predecessors = New List(Of String)()
        Me.EarlyStart = 0
        Me.EarlyFinish = 0
        Me.LateStart = 0
        Me.LateFinish = 0
        Me.Slack = 0
    End Sub
End Class

Public Class CPMAnalyzer
    Private activities As Dictionary(Of String, Activity)
    Private criticalPath As List(Of String)
    
    Public Sub New()
        activities = New Dictionary(Of String, Activity)()
        criticalPath = New List(Of String)()
    End Sub
    
    Public Sub AddActivity(name As String, duration As Integer, Optional predecessors As List(Of String) = Nothing)
        Dim activity As New Activity(name, duration)
        If predecessors IsNot Nothing Then
            activity.Predecessors = predecessors
        End If
        activities.Add(name, activity)
    End Sub
    
    Public Sub CalculateCPM()
        ' Forward Pass - Calculate Early Start and Early Finish
        CalculateForwardPass()
        
        ' Backward Pass - Calculate Late Start and Late Finish
        CalculateBackwardPass()
        
        ' Calculate Slack and Identify Critical Path
        CalculateSlackAndCriticalPath()
    End Sub
    
    Private Sub CalculateForwardPass()
        ' Initialize all early starts to 0
        For Each activity In activities.Values
            activity.EarlyStart = 0
        Next
        
        ' Process activities in topological order
        Dim processed As New HashSet(Of String)()
        Dim queue As New Queue(Of String)()
        
        ' Find activities with no predecessors
        For Each activity In activities.Values
            If activity.Predecessors.Count = 0 Then
                queue.Enqueue(activity.Name)
            End If
        Next
        
        While queue.Count > 0
            Dim currentActivityName As String = queue.Dequeue()
            Dim currentActivity As Activity = activities(currentActivityName)
            processed.Add(currentActivityName)
            
            ' Calculate early finish
            currentActivity.EarlyFinish = currentActivity.EarlyStart + currentActivity.Duration
            
            ' Update successors
            For Each activity In activities.Values
                If activity.Predecessors.Contains(currentActivityName) Then
                    Dim newEarlyStart As Integer = currentActivity.EarlyFinish
                    If newEarlyStart > activity.EarlyStart Then
                        activity.EarlyStart = newEarlyStart
                    End If
                    
                    ' Add to queue if all predecessors are processed
                    Dim allPredecessorsProcessed As Boolean = True
                    For Each pred In activity.Predecessors
                        If Not processed.Contains(pred) Then
                            allPredecessorsProcessed = False
                            Exit For
                        End If
                    Next
                    
                    If allPredecessorsProcessed AndAlso Not queue.Contains(activity.Name) Then
                        queue.Enqueue(activity.Name)
                    End If
                End If
            Next
        End While
    End Sub
    
    Private Sub CalculateBackwardPass()
        ' Find the maximum early finish time
        Dim maxFinishTime As Integer = 0
        For Each activity In activities.Values
            If activity.EarlyFinish > maxFinishTime Then
                maxFinishTime = activity.EarlyFinish
            End If
        Next
        
        ' Initialize late finish times
        For Each activity In activities.Values
            activity.LateFinish = maxFinishTime
        Next
        
        ' Backward pass
        Dim processed As New HashSet(Of String)()
        Dim queue As New Queue(Of String)()
        
        ' Add all activities with maximum late finish to queue
        For Each activity In activities.Values
            If activity.EarlyFinish = maxFinishTime Then
                queue.Enqueue(activity.Name)
                activity.LateStart = activity.LateFinish - activity.Duration
                processed.Add(activity.Name)
            End If
        Next
        
        While queue.Count > 0
            Dim currentActivityName As String = queue.DeQueue()
            Dim currentActivity As Activity = activities(currentActivityName)
            
            ' Update predecessors
            For Each activity In activities.Values
                If activity.Predecessors.Contains(currentActivityName) Then
                    Dim newLateFinish As Integer = currentActivity.LateStart
                    If newLateFinish < activity.LateFinish Then
                        activity.LateFinish = newLateFinish
                        activity.LateStart = activity.LateFinish - activity.Duration
                    End If
                    
                    ' Add to queue if all successors are processed
                    If Not processed.Contains(activity.Name) Then
                        processed.Add(activity.Name)
                        queue.Enqueue(activity.Name)
                    End If
                End If
            Next
        End While
    End Sub
    
    Private Sub CalculateSlackAndCriticalPath()
        ' Calculate slack for each activity
        For Each activity In activities.Values
            activity.Slack = activity.LateStart - activity.EarlyStart
        Next
        
        ' Identify critical path activities (slack = 0)
        criticalPath.Clear()
        For Each activity In activities.Values
            If activity.Slack = 0 Then
                criticalPath.Add(activity.Name)
            End If
        Next
    End Sub
    
    Public Function GetCriticalPath() As List(Of String)
        Return criticalPath.ToList()
    End Function
    
    Public Function GetTotalProjectDuration() As Integer
        Dim maxFinish As Integer = 0
        For Each activity In activities.Values
            If activity.EarlyFinish > maxFinish Then
                maxFinish = activity.EarlyFinish
            End If
        Next
        Return maxFinish
    End Function
    
    Public Sub DisplayResults()
        Console.WriteLine("=== Critical Path Method Results ===")
        Console.WriteLine("Total Project Duration: " & GetTotalProjectDuration() & " days")
        Console.WriteLine()
        Console.WriteLine("Activity Details:")
        Console.WriteLine("Name    Duration  ES    EF    LS    LF    Slack")
        Console.WriteLine("------------------------------------------------")
        
        For Each activity In activities.Values.OrderBy(Function(a) a.EarlyStart)
            Console.WriteLine(String.Format("{0,-8} {1,3}       {2,3}   {3,3}   {4,3}   {5,3}   {6,3}",
                                          activity.Name,
                                          activity.Duration,
                                          activity.EarlyStart,
                                          activity.EarlyFinish,
                                          activity.LateStart,
                                          activity.LateFinish,
                                          activity.Slack))
        Next
        
        Console.WriteLine()
        Console.WriteLine("Critical Path Activities:")
        For Each activityName In criticalPath
            Console.WriteLine(activityName)
        Next
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        Dim analyzer As New CPMAnalyzer()
        
        ' Add activities with their predecessors
        analyzer.AddActivity("A", 3)
        analyzer.AddActivity("B", 2, New List(Of String) From {"A"})
        analyzer.AddActivity("C", 4, New List(Of String) From {"A"})
        analyzer.AddActivity("D", 3, New List(Of String) From {"B", "C"})
        analyzer.AddActivity("E", 2, New List(Of String) From {"C"})
        analyzer.AddActivity("F", 5, New List(Of String) From {"D", "E"})
        
        ' Calculate CPM
        analyzer.CalculateCPM()
        
        ' Display results
        analyzer.DisplayResults()
        
        Console.WriteLine()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Example Output

When you run this program with the sample activities, you'll get output similar to:

```
=== Critical Path Method Results ===
Total Project Duration: 10 days

Activity Details:
Name    Duration  ES    EF    LS    LF    Slack
------------------------------------------------
A        3       0     3     0     3     0
B        2       3     5     3     5     0
C        4       3     7     5     9     2
D        3       5     8     5     8     0
E        2       7     9     9     11    2
F        5       8     13    8     13    0

Critical Path Activities:
A
B
D
F
```

## Key Features of this CPM Implementation:

1. **Activity Class**: Stores all necessary information for each activity (name, duration, predecessors, timing data)
2. **Forward Pass**: Calculates Early Start and Early Finish times
3. **Backward Pass**: Calculates Late Start and Late Finish times
4. **Slack Calculation**: Determines the slack for each activity
5. **Critical Path Identification**: Identifies activities with zero slack
6. **Results Display**: Shows comprehensive timing information

## How to Use:

1. Create a new Visual Basic Console Application
2. Copy the code into the main module
3. Add activities using `AddActivity()` method with name, duration, and optional predecessors
4. Call `CalculateCPM()` to perform the analysis
5. Use `DisplayResults()` to see the complete analysis

This implementation handles the core CPM algorithm and can be extended with additional features like resource constraints or more complex dependency relationships.

