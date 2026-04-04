# Counting Sort Algorithm in Visual Basic

Here's a complete implementation of the Counting Sort algorithm in Visual Basic:

```vb
Module CountingSort
    Sub Main()
        ' Example usage
        Dim arr() As Integer = {4, 2, 2, 8, 3, 3, 1}
        
        Console.WriteLine("Original array:")
        PrintArray(arr)
        
        ' Perform counting sort
        CountingSortArray(arr)
        
        Console.WriteLine("Sorted array:")
        PrintArray(arr)
        
        Console.ReadLine()
    End Sub
    
    Sub CountingSortArray(ByRef arr() As Integer)
        If arr Is Nothing OrElse arr.Length = 0 Then
            Return
        End If
        
        ' Find the maximum value to determine range
        Dim maxValue As Integer = arr(0)
        For i As Integer = 1 To arr.Length - 1
            If arr(i) > maxValue Then
                maxValue = arr(i)
            End If
        Next
        
        ' Create counting array
        Dim count(maxValue) As Integer
        
        ' Count occurrences of each element
        For i As Integer = 0 To arr.Length - 1
            count(arr(i)) += 1
        Next
        
        ' Reconstruct the sorted array
        Dim index As Integer = 0
        For i As Integer = 0 To maxValue
            While count(i) > 0
                arr(index) = i
                index += 1
                count(i) -= 1
            End While
        Next
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

1. **Find Maximum Value**: First, we determine the maximum value in the input array to know the size of the counting array needed.

2. **Count Elements**: Create a counting array where each index represents a value from the input array, and the value at each index represents how many times that value appears.

3. **Reconstruct Array**: Go through the counting array and reconstruct the sorted input array by placing elements in their correct positions.

## Sample Output:
```
Original array:
4 2 2 8 3 3 1 
Sorted array:
1 2 2 3 3 4 8 
```

## Time and Space Complexity:
- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the counting array

## Key Features:
- Works efficiently for small range of integers
- Stable sorting algorithm
- Non-comparative sorting method
- Best when the range of possible values is not significantly greater than the number of elements

