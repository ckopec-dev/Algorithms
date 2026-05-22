# Ternary Search Algorithm in Visual Basic

Here's an implementation of the ternary search algorithm in Visual Basic .NET:

```vb
Module TernarySearchExample
    ' Ternary search function that searches for a target value in a sorted array
    ' Returns the index of the target element if found, otherwise returns -1
    Function TernarySearch(arr() As Integer, target As Integer) As Integer
        Dim left As Integer = 0
        Dim right As Integer = arr.Length - 1
        
        While left <= right
            ' Calculate the two mid points
            Dim mid1 As Integer = left + (right - left) \ 3
            Dim mid2 As Integer = right - (right - left) \ 3
            
            ' If target is at either mid point, return the index
            If arr(mid1) = target Then
                Return mid1
            End If
            
            If arr(mid2) = target Then
                Return mid2
            End If
            
            ' Determine which segment to search next
            If target < arr(mid1) Then
                ' Target is in the left segment
                right = mid1 - 1
            ElseIf target > arr(mid2) Then
                ' Target is in the right segment
                left = mid2 + 1
            Else
                ' Target is in the middle segment
                left = mid1 + 1
                right = mid2 - 1
            End If
        End While
        
        ' Target not found
        Return -1
    End Function
    
    ' Main subroutine to demonstrate the ternary search
    Sub Main()
        ' Example sorted array
        Dim numbers() As Integer = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25}
        
        Console.WriteLine("Sorted Array: " & String.Join(", ", numbers))
        Console.WriteLine()
        
        ' Test cases
        Dim testValues() As Integer = {7, 1, 25, 10, 26}
        
        For Each target As Integer In testValues
            Dim result As Integer = TernarySearch(numbers, target)
            
            If result <> -1 Then
                Console.WriteLine($"Target {target} found at index {result}")
            Else
                Console.WriteLine($"Target {target} not found in the array")
            End If
        Next
        
        Console.WriteLine()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## How the Algorithm Works:

1. **Divide**: The array is divided into three parts using two mid points (`mid1` and `mid2`)
2. **Compare**: The target is compared with both mid points
3. **Conquer**: Based on the comparison, the search space is reduced to one of three segments:
   - Left segment (if target < mid1)
   - Middle segment (if mid1 < target < mid2)
   - Right segment (if target > mid2)
4. **Repeat**: The process continues until the target is found or the search space is exhausted

## Time and Space Complexity:

- **Time Complexity**: O(log₃ n) - Each iteration reduces the search space by 2/3
- **Space Complexity**: O(1) - Uses constant extra space

## Output Example:
```
Sorted Array: 1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25

Target 7 found at index 3
Target 1 found at index 0
Target 25 found at index 12
Target 10 not found in the array
Target 26 not found in the array

Press any key to exit...
```

