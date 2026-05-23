# Quick Sort Algorithm in Visual Basic

Here's a complete implementation of the Quick Sort algorithm in Visual Basic .NET:

```vb
Public Class QuickSortExample
    
    ' Main QuickSort function
    Public Shared Sub QuickSort(ByRef arr() As Integer, ByVal low As Integer, ByVal high As Integer)
        If low < high Then
            ' Partition the array and get pivot index
            Dim pivotIndex As Integer = Partition(arr, low, high)
            
            ' Recursively sort elements before and after partition
            QuickSort(arr, low, pivotIndex - 1)
            QuickSort(arr, pivotIndex + 1, high)
        End If
    End Sub
    
    ' Partition function
    Private Shared Function Partition(ByRef arr() As Integer, ByVal low As Integer, ByVal high As Integer) As Integer
        ' Choose the rightmost element as pivot
        Dim pivot As Integer = arr(high)
        
        ' Index of smaller element (indicates right position of pivot)
        Dim i As Integer = low - 1
        
        For j As Integer = low To high - 1
            ' If current element is smaller than or equal to pivot
            If arr(j) <= pivot Then
                i += 1
                ' Swap elements
                Swap(arr, i, j)
            End If
        Next
        
        ' Swap the pivot element with the element at i+1
        Swap(arr, i + 1, high)
        
        ' Return the partition index
        Return i + 1
    End Function
    
    ' Helper function to swap two elements
    Private Shared Sub Swap(ByRef arr() As Integer, ByVal i As Integer, ByVal j As Integer)
        Dim temp As Integer = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
    End Sub
    
    ' Example usage
    Public Shared Sub Main()
        Dim numbers() As Integer = {64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42}
        
        Console.WriteLine("Original array:")
        PrintArray(numbers)
        
        ' Sort the array
        QuickSort(numbers, 0, numbers.Length - 1)
        
        Console.WriteLine("Sorted array:")
        PrintArray(numbers)
        
        Console.ReadLine()
    End Sub
    
    ' Helper function to print array
    Private Shared Sub PrintArray(ByRef arr() As Integer)
        For i As Integer = 0 To arr.Length - 1
            Console.Write(arr(i) & " ")
        Next
        Console.WriteLine()
    End Sub
End Class
```

## How it works:

1. **Partition Function**: 
   - Selects the last element as pivot
   - Rearranges array so elements smaller than pivot come before it
   - Elements greater than pivot come after it
   - Returns the final position of pivot

2. **QuickSort Function**:
   - Recursively applies the sorting process
   - Sorts elements before and after the pivot
   - Continues until subarrays have only one element

## Sample Output:
```
Original array:
64 34 25 12 22 11 90 88 76 50 42 
Sorted array:
11 12 22 25 34 42 50 64 76 88 90 
```

## Key Features:
- **In-place sorting** (requires only O(log n) extra space)
- **Time complexity**: O(n log n) average case, O(n²) worst case
- **Space complexity**: O(log n)
- **Not stable** (doesn't preserve relative order of equal elements)

