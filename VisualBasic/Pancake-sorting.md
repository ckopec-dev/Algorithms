# Pancake Sorting Algorithm in Visual Basic

```vb
Public Class PancakeSorting
    ' Pancake sorting algorithm implementation
    Public Shared Sub PancakeSort(ByRef arr() As Integer)
        Dim n As Integer = arr.Length
        
        ' Start from the end and work backwards
        For i As Integer = n - 1 To 1 Step -1
            ' Find the index of the maximum element in arr[0..i]
            Dim maxIndex As Integer = FindMaxIndex(arr, i)
            
            ' If the maximum element is not already at the end
            If maxIndex <> i Then
                ' Flip the array up to maxIndex to bring max element to front
                Flip(arr, maxIndex)
                ' Flip the array up to i to bring max element to its correct position
                Flip(arr, i)
            End If
        Next
    End Sub
    
    ' Helper function to find index of maximum element in array up to index n
    Private Shared Function FindMaxIndex(arr() As Integer, n As Integer) As Integer
        Dim maxIndex As Integer = 0
        Dim maxValue As Integer = arr(0)
        
        For i As Integer = 1 To n
            If arr(i) > maxValue Then
                maxValue = arr(i)
                maxIndex = i
            End If
        Next
        
        Return maxIndex
    End Function
    
    ' Helper function to flip the array from index 0 to n
    Private Shared Sub Flip(arr() As Integer, n As Integer)
        Dim start As Integer = 0
        Dim endIdx As Integer = n
        
        While start < endIdx
            ' Swap elements
            Dim temp As Integer = arr(start)
            arr(start) = arr(endIdx)
            arr(endIdx) = temp
            
            start += 1
            endIdx -= 1
        End While
    End Sub
    
    ' Helper function to print array
    Public Shared Sub PrintArray(arr() As Integer)
        For i As Integer = 0 To arr.Length - 1
            Console.Write(arr(i) & " ")
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Example array to sort
        Dim arr() As Integer = {3, 2, 4, 1, 5}
        
        Console.WriteLine("Original array:")
        PancakeSorting.PrintArray(arr)
        
        ' Perform pancake sort
        PancakeSorting.PancakeSort(arr)
        
        Console.WriteLine("Sorted array:")
        PancakeSorting.PrintArray(arr)
        
        Console.ReadLine()
    End Sub
End Module
```

## How it works:

1. **Find Maximum**: For each position from the end to the beginning, find the maximum element in the unsorted portion
2. **Flip to Front**: If the maximum element is not already at the front, flip the array up to that element's position to bring it to the front
3. **Flip to Correct Position**: Flip the entire unsorted portion to move the maximum element to its correct position
4. **Repeat**: Continue until the entire array is sorted

## Example Output:
```
Original array:
3 2 4 1 5 
Sorted array:
1 2 3 4 5 
```

The algorithm performs at most 2n flips in the worst case, making it an efficient sorting algorithm with a time complexity of O(n²) and space complexity of O(1).

