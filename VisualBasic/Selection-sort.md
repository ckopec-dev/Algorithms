# Selection Sort Algorithm in Visual Basic

Here's an implementation of the Selection Sort algorithm in Visual Basic:

```vb
Module SelectionSort
    Sub Main()
        ' Example array to sort
        Dim numbers() As Integer = {64, 34, 25, 12, 22, 11, 90}
        
        Console.WriteLine("Original array:")
        PrintArray(numbers)
        
        ' Perform selection sort
        SelectionSortArray(numbers)
        
        Console.WriteLine("Sorted array:")
        PrintArray(numbers)
        
        Console.ReadLine()
    End Sub
    
    ' Selection Sort Algorithm
    Sub SelectionSortArray(ByRef arr() As Integer)
        Dim n As Integer = arr.Length
        Dim i As Integer, j As Integer, minIndex As Integer
        Dim temp As Integer
        
        ' Traverse through all array elements
        For i = 0 To n - 2
            ' Find the minimum element in remaining unsorted array
            minIndex = i
            
            For j = i + 1 To n - 1
                If arr(j) < arr(minIndex) Then
                    minIndex = j
                End If
            Next
            
            ' Swap the found minimum element with the first element
            If minIndex <> i Then
                temp = arr(i)
                arr(i) = arr(minIndex)
                arr(minIndex) = temp
            End If
            
            ' Optional: Show step by step process
            Console.WriteLine("Step " & (i + 1) & ": " & String.Join(", ", arr))
        Next
    End Sub
    
    ' Helper function to print array
    Sub PrintArray(arr() As Integer)
        Console.WriteLine(String.Join(", ", arr))
    End Sub
End Module
```

## How it works:

1. **Find Minimum**: In each iteration, find the smallest element in the unsorted portion
2. **Swap**: Place this minimum element at the beginning of the unsorted portion
3. **Repeat**: Continue until the entire array is sorted

## Sample Output:
```
Original array:
64, 34, 25, 12, 22, 11, 90
Step 1: 11, 34, 25, 12, 22, 64, 90
Step 2: 11, 12, 25, 34, 22, 64, 90
Step 3: 11, 12, 22, 34, 25, 64, 90
Step 4: 11, 12, 22, 25, 34, 64, 90
Step 5: 11, 12, 22, 25, 34, 64, 90
Step 6: 11, 12, 22, 25, 34, 64, 90
Sorted array:
11, 12, 22, 25, 34, 64, 90
```

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- O(1) - sorts in-place

The algorithm is simple but inefficient for large datasets due to its quadratic time complexity.

