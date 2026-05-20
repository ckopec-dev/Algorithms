# Comb Sort Algorithm in Visual Basic

Here's an implementation of the Comb Sort algorithm in Visual Basic:

```vb
Public Class CombSort
    ' Comb Sort algorithm implementation
    Public Shared Sub CombSortArray(ByRef arr() As Integer)
        Dim gap As Integer = arr.Length
        Dim shrinkFactor As Double = 1.3
        Dim sorted As Boolean = False
        
        While Not sorted
            ' Calculate new gap
            gap = CInt(Math.Floor(gap / shrinkFactor))
            
            ' If gap becomes 1, we're doing a final bubble sort
            If gap <= 1 Then
                gap = 1
                sorted = True
            End If
            
            ' Compare elements with current gap
            For i As Integer = 0 To arr.Length - gap - 1
                If arr(i) > arr(i + gap) Then
                    ' Swap elements
                    Dim temp As Integer = arr(i)
                    arr(i) = arr(i + gap)
                    arr(i + gap) = temp
                    sorted = False
                End If
            Next
        End While
    End Sub
    
    ' Helper method to display array
    Public Shared Sub DisplayArray(arr() As Integer)
        For i As Integer = 0 To arr.Length - 1
            Console.Write(arr(i) & " ")
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Create test array
        Dim numbers() As Integer = {64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42}
        
        Console.WriteLine("Original array:")
        CombSort.DisplayArray(numbers)
        
        ' Sort the array
        CombSort.CombSortArray(numbers)
        
        Console.WriteLine("Sorted array:")
        CombSort.DisplayArray(numbers)
        
        Console.ReadLine()
    End Sub
End Module
```

## How Comb Sort Works:

1. **Initialize**: Start with a gap equal to the array length
2. **Shrink**: Reduce the gap by a shrink factor (typically 1.3)
3. **Compare**: Compare elements separated by the gap
4. **Swap**: If elements are out of order, swap them
5. **Repeat**: Continue until gap becomes 1 and no swaps occur

## Key Features:

- **Time Complexity**: O(n²) worst case, O(n log n) average case
- **Space Complexity**: O(1) - sorts in place
- **Improvement**: Better than bubble sort due to the gap shrinking
- **Stability**: Not stable (doesn't preserve relative order of equal elements)

## Output:
```
Original array:
64 34 25 12 22 11 90 88 76 50 42 
Sorted array:
11 12 22 25 34 42 50 64 76 88 90 
```

