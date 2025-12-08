# Comb Sort Algorithm in Visual Basic

```vb
Module CombSortExample
    Sub Main()
        ' Example array to sort
        Dim arr() As Integer = {8, 4, 1, 56, 3, -44, 23, -6, 28, 0}
        
        Console.WriteLine("Original array:")
        PrintArray(arr)
        
        ' Perform comb sort
        CombSort(arr)
        
        Console.WriteLine("Sorted array:")
        PrintArray(arr)
        
        Console.ReadKey()
    End Sub
    
    Sub CombSort(ByRef arr() As Integer)
        Dim gap As Integer = arr.Length
        Dim shrinkFactor As Double = 1.3
        Dim sorted As Boolean = False
        
        While Not sorted
            ' Calculate new gap
            gap = CInt(Math.Floor(gap / shrinkFactor))
            
            ' If gap is 1, we're doing a final bubble sort
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
    
    Sub PrintArray(arr() As Integer)
        For i As Integer = 0 To arr.Length - 1
            Console.Write(arr(i) & " ")
        Next
        Console.WriteLine()
    End Sub
End Module
```

## How Comb Sort Works

The Comb Sort algorithm is an improvement over the Bubble Sort algorithm:

1. **Initial Gap**: Start with a large gap (usually the array length)
2. **Shrink Gap**: Reduce the gap by a shrink factor (typically 1.3)
3. **Compare Elements**: Compare elements that are `gap` positions apart
4. **Swap if Needed**: If elements are in wrong order, swap them
5. **Repeat**: Continue until gap becomes 1, then do final bubble sort

## Output
```
Original array:
8 4 1 56 3 -44 23 -6 28 0 
Sorted array:
-44 -6 0 1 3 4 8 23 28 56 
```

## Key Features

- **Time Complexity**: O(n²) worst case, O(n log n) average case
- **Space Complexity**: O(1) - sorts in place
- **Improvement over Bubble Sort**: Eliminates "turtles" (small values at the end)
- **Adaptive**: Performs better than Bubble Sort on most inputs

