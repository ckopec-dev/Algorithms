# Bucket Sort Algorithm in Visual Basic

Here's a complete implementation of the Bucket Sort algorithm in Visual Basic:

```vb
Public Class BucketSort
    ' Bucket Sort implementation
    Public Shared Sub BucketSortArray(ByVal arr() As Double)
        If arr Is Nothing OrElse arr.Length <= 1 Then
            Return
        End If
        
        Dim n As Integer = arr.Length
        Dim buckets() As List(Of Double)
        ReDim buckets(n - 1)
        
        ' Initialize buckets
        For i As Integer = 0 To n - 1
            buckets(i) = New List(Of Double)
        Next
        
        ' Find minimum and maximum values
        Dim minVal As Double = arr(0)
        Dim maxVal As Double = arr(0)
        
        For i As Integer = 1 To arr.Length - 1
            If arr(i) < minVal Then
                minVal = arr(i)
            End If
            If arr(i) > maxVal Then
                maxVal = arr(i)
            End If
        Next
        
        ' Calculate range
        Dim range As Double = maxVal - minVal
        
        ' Distribute elements into buckets
        For i As Integer = 0 To arr.Length - 1
            If range = 0 Then
                buckets(0).Add(arr(i))
            Else
                Dim bucketIndex As Integer = CInt(Math.Floor((arr(i) - minVal) / range * (n - 1)))
                If bucketIndex >= n Then
                    bucketIndex = n - 1
                End If
                buckets(bucketIndex).Add(arr(i))
            End If
        Next
        
        ' Sort individual buckets and concatenate results
        Dim index As Integer = 0
        For i As Integer = 0 To n - 1
            If buckets(i).Count > 0 Then
                buckets(i).Sort() ' Using built-in sort for each bucket
                For Each value As Double In buckets(i)
                    arr(index) = value
                    index += 1
                Next
            End If
        Next
    End Sub
    
    ' Helper method to print array
    Public Shared Sub PrintArray(ByVal arr() As Double)
        For i As Integer = 0 To arr.Length - 1
            Console.Write(arr(i) & " ")
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Example 1: Sorting an array of doubles
        Dim numbers() As Double = {0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51}
        
        Console.WriteLine("Original array:")
        BucketSort.PrintArray(numbers)
        
        BucketSort.BucketSortArray(numbers)
        
        Console.WriteLine("Sorted array:")
        BucketSort.PrintArray(numbers)
        
        Console.WriteLine()
        
        ' Example 2: Another test case
        Dim numbers2() As Double = {0.897, 0.565, 0.656, 0.1234, 0.665, 0.3434}
        
        Console.WriteLine("Original array 2:")
        BucketSort.PrintArray(numbers2)
        
        BucketSort.BucketSortArray(numbers2)
        
        Console.WriteLine("Sorted array 2:")
        BucketSort.PrintArray(numbers2)
        
        Console.ReadKey()
    End Sub
End Module
```

## How the Algorithm Works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Buckets**: Create empty buckets (typically equal to array length)
3. **Distribute Elements**: Place each element into appropriate bucket based on its value
4. **Sort Buckets**: Sort each individual bucket (using built-in sort)
5. **Concatenate**: Combine all sorted buckets to get final sorted array

## Key Features:

- **Time Complexity**: O(n + k) average case, O(n²) worst case
- **Space Complexity**: O(n + k)
- **Stable**: Maintains relative order of equal elements
- **Best for**: Uniformly distributed data

## Sample Output:
```
Original array:
0.42 0.32 0.33 0.52 0.37 0.47 0.51 
Sorted array:
0.32 0.33 0.37 0.42 0.47 0.51 0.52 

Original array 2:
0.897 0.565 0.656 0.1234 0.665 0.3434 
Sorted array 2:
0.1234 0.3434 0.565 0.656 0.665 0.897 
```

This implementation handles edge cases like empty arrays and arrays with duplicate values, making it robust for practical use.

