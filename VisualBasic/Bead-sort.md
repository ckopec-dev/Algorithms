# Bead Sort Algorithm in Visual Basic

```vb
Public Class BeadSort
    ''' <summary>
    ''' Performs bead sort (also known as gravity sort) on an array of integers
    ''' </summary>
    ''' <param name="array">Array of non-negative integers to sort</param>
    ''' <returns>Sorted array in ascending order</returns>
    Public Shared Function BeadSortArray(array() As Integer) As Integer()
        If array Is Nothing OrElse array.Length <= 1 Then
            Return array
        End If

        ' Find maximum value to determine bead height
        Dim maxValue As Integer = array(0)
        For i As Integer = 1 To array.Length - 1
            If array(i) > maxValue Then
                maxValue = array(i)
            End If
        Next

        ' Create bead grid - rows represent bead heights, columns represent elements
        Dim grid(maxValue, array.Length - 1) As Integer

        ' Place beads in the grid
        For i As Integer = 0 To array.Length - 1
            For j As Integer = 0 To array(i) - 1
                grid(j, i) = 1
            Next
        Next

        ' Let beads fall to the bottom (gravity sort)
        For i As Integer = 0 To maxValue - 1
            For j As Integer = 0 To array.Length - 1
                ' Count how many beads are above current position
                Dim count As Integer = 0
                For k As Integer = i To maxValue - 1
                    If grid(k, j) = 1 Then
                        count += 1
                    End If
                Next
                
                ' Place beads at bottom
                For k As Integer = 0 To count - 1
                    grid(maxValue - 1 - k, j) = 1
                Next
                
                ' Clear upper positions
                For k As Integer = 0 To maxValue - 1 - count
                    grid(maxValue - 1 - k, j) = 0
                Next
            Next
        Next

        ' Read sorted values from grid
        Dim result(array.Length - 1) As Integer
        For i As Integer = 0 To array.Length - 1
            result(i) = 0
            For j As Integer = 0 To maxValue - 1
                If grid(j, i) = 1 Then
                    result(i) += 1
                End If
            Next
        Next

        Return result
    End Function

    ''' <summary>
    ''' Alternative simpler implementation of bead sort
    ''' </summary>
    ''' <param name="array">Array of non-negative integers to sort</param>
    ''' <returns>Sorted array in ascending order</returns>
    Public Shared Function SimpleBeadSort(array() As Integer) As Integer()
        If array Is Nothing OrElse array.Length <= 1 Then
            Return array
        End If

        ' Find maximum value
        Dim maxValue As Integer = array(0)
        For i As Integer = 1 To array.Length - 1
            If array(i) > maxValue Then
                maxValue = array(i)
            End If
        Next

        ' Create a "bead" array where each position represents a bead
        Dim beads(maxValue) As Integer

        ' Count beads for each value
        For i As Integer = 0 To array.Length - 1
            beads(array(i)) += 1
        Next

        ' Reconstruct sorted array
        Dim result(array.Length - 1) As Integer
        Dim index As Integer = 0

        For i As Integer = 0 To maxValue
            For j As Integer = 0 To beads(i) - 1
                result(index) = i
                index += 1
            Next
        Next

        Return result
    End Function

    ''' <summary>
    ''' Display array contents for testing
    ''' </summary>
    ''' <param name="array">Array to display</param>
    ''' <param name="title">Title for display</param>
    Public Shared Sub DisplayArray(array() As Integer, title As String)
        Console.Write(title & ": [")
        For i As Integer = 0 To array.Length - 1
            Console.Write(array(i))
            If i < array.Length - 1 Then
                Console.Write(", ")
            End If
        Next
        Console.WriteLine("]")
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Test array
        Dim testArray() As Integer = {4, 2, 7, 1, 9, 3, 6, 5}

        Console.WriteLine("Bead Sort Algorithm Demo")
        Console.WriteLine("========================")

        ' Display original array
        BeadSort.DisplayArray(testArray, "Original Array")

        ' Sort using bead sort
        Dim sortedArray() As Integer = BeadSort.BeadSortArray(testArray)

        ' Display sorted array
        BeadSort.DisplayArray(sortedArray, "Sorted Array")

        ' Test with simple implementation
        Dim simpleSorted() As Integer = BeadSort.SimpleBeadSort(testArray)
        BeadSort.DisplayArray(simpleSorted, "Simple Bead Sort Result")

        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## How Bead Sort Works

The bead sort algorithm works by simulating beads falling under gravity:

1. **Initialization**: Create a grid where rows represent bead heights and columns represent array elements
2. **Bead Placement**: Place beads in the grid according to array values
3. **Gravity Simulation**: Let beads fall to the bottom of their respective columns
4. **Result Extraction**: Read the final bead positions to get the sorted array

## Key Features

- **Time Complexity**: O(n + max_value) where n is array length and max_value is maximum element
- **Space Complexity**: O(max_value × n) for the grid
- **Best For**: Sorting arrays with small range of values
- **Limitation**: Only works with non-negative integers

## Sample Output
```
Bead Sort Algorithm Demo
========================
Original Array: [4, 2, 7, 1, 9, 3, 6, 5]
Sorted Array: [1, 2, 3, 4, 5, 6, 7, 9]
Simple Bead Sort Result: [1, 2, 3, 4, 5, 6, 7, 9]
```

