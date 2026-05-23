# Cocktail Shaker Sort in Visual Basic

```vb
Module CocktailShakerSort
    Sub Main()
        ' Example array to sort
        Dim arr() As Integer = {64, 34, 25, 12, 22, 11, 90, 5}
        
        Console.WriteLine("Original array:")
        PrintArray(arr)
        
        ' Perform cocktail shaker sort
        CocktailShakerSortAlgorithm(arr)
        
        Console.WriteLine("Sorted array:")
        PrintArray(arr)
        
        Console.ReadKey()
    End Sub
    
    Sub CocktailShakerSortAlgorithm(ByRef arr() As Integer)
        Dim n As Integer = arr.Length
        Dim left As Integer = 0
        Dim right As Integer = n - 1
        Dim swapped As Boolean = True
        
        While swapped
            swapped = False
            
            ' Forward pass (left to right)
            For i As Integer = left To right - 1
                If arr(i) > arr(i + 1) Then
                    ' Swap elements
                    Dim temp As Integer = arr(i)
                    arr(i) = arr(i + 1)
                    arr(i + 1) = temp
                    swapped = True
                End If
            Next
            
            ' If no swapping occurred, array is sorted
            If Not swapped Then Exit While
            
            ' Move the right boundary
            right -= 1
            
            ' Backward pass (right to left)
            For i As Integer = right To left + 1 Step -1
                If arr(i) < arr(i - 1) Then
                    ' Swap elements
                    Dim temp As Integer = arr(i)
                    arr(i) = arr(i - 1)
                    arr(i - 1) = temp
                    swapped = True
                End If
            Next
            
            ' Move the left boundary
            left += 1
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

## How it works:

1. **Bidirectional Sorting**: Unlike regular bubble sort, cocktail shaker sort alternates between forward and backward passes
2. **Forward Pass**: Moves the largest element to the right end
3. **Backward Pass**: Moves the smallest element to the left end
4. **Boundary Adjustment**: After each pass, the boundaries are adjusted since elements are already in their correct positions
5. **Optimization**: The algorithm stops early if no swaps occur in a complete pass

## Output:
```
Original array:
64 34 25 12 22 11 90 5 
Sorted array:
5 11 12 22 25 34 64 90 
```

This implementation demonstrates the cocktail shaker sort algorithm's ability to sort arrays efficiently by moving elements in both directions during each iteration.

