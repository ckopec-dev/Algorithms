# Introsort Algorithm in Visual Basic

```vb
Public Class Introsort
    Private Shared Sub Introsort(ByVal arr() As Integer, ByVal low As Integer, ByVal high As Integer, ByVal maxDepth As Integer)
        If high - low <= 1 Then
            Return
        End If

        If maxDepth = 0 Then
            ' Use heapsort when max depth is reached
            HeapSort(arr, low, high)
            Return
        End If

        If high - low <= 16 Then
            ' Use insertion sort for small arrays
            InsertionSort(arr, low, high)
            Return
        End If

        ' Partition the array
        Dim pivotIndex As Integer = Partition(arr, low, high)
        
        ' Recursively sort both parts
        Introsort(arr, low, pivotIndex - 1, maxDepth - 1)
        Introsort(arr, pivotIndex + 1, high, maxDepth - 1)
    End Sub

    Private Shared Function Partition(ByVal arr() As Integer, ByVal low As Integer, ByVal high As Integer) As Integer
        Dim pivot As Integer = arr(high)
        Dim i As Integer = low - 1

        For j As Integer = low To high - 1
            If arr(j) <= pivot Then
                i += 1
                Swap(arr, i, j)
            End If
        Next

        Swap(arr, i + 1, high)
        Return i + 1
    End Function

    Private Shared Sub InsertionSort(ByVal arr() As Integer, ByVal low As Integer, ByVal high As Integer)
        For i As Integer = low + 1 To high
            Dim key As Integer = arr(i)
            Dim j As Integer = i - 1

            While j >= low AndAlso arr(j) > key
                arr(j + 1) = arr(j)
                j -= 1
            End While

            arr(j + 1) = key
        End While
    End Sub

    Private Shared Sub HeapSort(ByVal arr() As Integer, ByVal low As Integer, ByVal high As Integer)
        Dim n As Integer = high - low + 1
        Dim tempArr() As Integer = New Integer(n - 1) {}

        ' Copy array to temp array
        For i As Integer = 0 To n - 1
            tempArr(i) = arr(low + i)
        Next

        ' Build heap
        For i As Integer = n \ 2 - 1 To 0 Step -1
            Heapify(tempArr, n, i)
        Next

        ' Extract elements from heap one by one
        For i As Integer = n - 1 To 1 Step -1
            Swap(tempArr, 0, i)
            Heapify(tempArr, i, 0)
        Next

        ' Copy back to original array
        For i As Integer = 0 To n - 1
            arr(low + i) = tempArr(i)
        Next
    End Sub

    Private Shared Sub Heapify(ByVal arr() As Integer, ByVal n As Integer, ByVal i As Integer)
        Dim largest As Integer = i
        Dim left As Integer = 2 * i + 1
        Dim right As Integer = 2 * i + 2

        If left < n AndAlso arr(left) > arr(largest) Then
            largest = left
        End If

        If right < n AndAlso arr(right) > arr(largest) Then
            largest = right
        End If

        If largest <> i Then
            Swap(arr, i, largest)
            Heapify(arr, n, largest)
        End If
    End Sub

    Private Shared Sub Swap(ByVal arr() As Integer, ByVal i As Integer, ByVal j As Integer)
        Dim temp As Integer = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
    End Sub

    Public Shared Sub Sort(ByVal arr() As Integer)
        If arr Is Nothing OrElse arr.Length <= 1 Then
            Return
        End If

        Dim maxDepth As Integer = CInt(Math.Floor(2 * Math.Log(arr.Length) / Math.Log(2)))
        Introsort(arr, 0, arr.Length - 1, maxDepth)
    End Sub

    ' Helper method to print array
    Public Shared Sub PrintArray(ByVal arr() As Integer)
        For i As Integer = 0 To arr.Length - 1
            Console.Write(arr(i) & " ")
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        Dim arr() As Integer = {64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42}
        
        Console.WriteLine("Original array:")
        Introsort.PrintArray(arr)
        
        Introsort.Sort(arr)
        
        Console.WriteLine("Sorted array:")
        Introsort.PrintArray(arr)
        
        Console.ReadLine()
    End Sub
End Module
```

## Algorithm Explanation

The Introsort algorithm combines three sorting techniques:

1. **Quick Sort**: Used for the main sorting process
2. **Heap Sort**: Used when the recursion depth exceeds a threshold
3. **Insertion Sort**: Used for small subarrays (≤ 16 elements)

## Key Features

- **Time Complexity**: O(n log n) average and worst case
- **Space Complexity**: O(log n) due to recursion
- **Hybrid Approach**: Combines the best aspects of different sorting algorithms
- **Adaptive**: Switches between algorithms based on conditions

## How it Works

1. **Partitioning**: Uses quick sort partitioning technique
2. **Depth Tracking**: Keeps track of recursion depth to avoid worst-case scenarios
3. **Switching**: Switches to heap sort when depth exceeds threshold
4. **Small Arrays**: Uses insertion sort for small subarrays for efficiency

This implementation provides an efficient, hybrid sorting solution that maintains good performance across different input types.

