# Odd-Even Sort Algorithm in Visual Basic

Here's an implementation of the Odd-Even Sort (also known as Brick Sort) algorithm in Visual Basic:

```vb
Public Class OddEvenSort
    ' Odd-Even Sort Algorithm
    Public Shared Sub OddEvenSort(ByRef array() As Integer)
        Dim n As Integer = array.Length
        Dim isSorted As Boolean = False
        Dim i As Integer
        
        While Not isSorted
            isSorted = True
            
            ' Perform odd phase
            For i = 1 To n - 2 Step 2
                If array(i) > array(i + 1) Then
                    ' Swap elements
                    Dim temp As Integer = array(i)
                    array(i) = array(i + 1)
                    array(i + 1) = temp
                    isSorted = False
                End If
            Next
            
            ' Perform even phase
            For i = 0 To n - 2 Step 2
                If array(i) > array(i + 1) Then
                    ' Swap elements
                    Dim temp As Integer = array(i)
                    array(i) = array(i + 1)
                    array(i + 1) = temp
                    isSorted = False
                End If
            Next
        End While
    End Sub
    
    ' Helper method to display array
    Public Shared Sub DisplayArray(array() As Integer)
        For i As Integer = 0 To array.Length - 1
            Console.Write(array(i) & " ")
        Next
        Console.WriteLine()
    End Sub
End Class

' Example usage
Module Program
    Sub Main()
        ' Test array
        Dim testArray() As Integer = {64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42}
        
        Console.WriteLine("Original array:")
        OddEvenSort.DisplayArray(testArray)
        
        ' Sort the array
        OddEvenSort.OddEvenSort(testArray)
        
        Console.WriteLine("Sorted array:")
        OddEvenSort.DisplayArray(testArray)
        
        Console.ReadLine()
    End Sub
End Module
```

## How it works:

1. **Odd-Even Sort** is a variation of Bubble Sort that works in two phases:
   - **Odd phase**: Compare and swap elements at odd indices (1, 3, 5, ...)
   - **Even phase**: Compare and swap elements at even indices (0, 2, 4, ...)

2. **Process**:
   - Continue alternating between odd and even phases
   - After each phase, check if any swaps occurred
   - If no swaps occurred in a complete pass, the array is sorted
   - Repeat until the array is completely sorted

## Example Output:
```
Original array:
64 34 25 12 22 11 90 88 76 50 42 
Sorted array:
11 12 22 25 34 42 50 64 76 88 90 
```

## Time Complexity:
- **Best case**: O(n) - when array is already sorted
- **Average case**: O(n²)
- **Worst case**: O(n²)

## Space Complexity:
- O(1) - sorts in place, only uses a constant amount of extra memory

This algorithm is particularly useful for parallel processing because the odd and even phases can be executed simultaneously on different processors.

