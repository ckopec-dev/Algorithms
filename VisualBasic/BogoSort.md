```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Module BogoSortExample
    ' BogoSort algorithm implementation
    Sub BogoSort(ByRef array() As Integer)
        Dim random As New Random()
        
        While Not IsSorted(array)
            ' Shuffle the array randomly
            For i As Integer = 0 To array.Length - 1
                Dim j As Integer = random.Next(0, array.Length)
                Dim temp As Integer = array(i)
                array(i) = array(j)
                array(j) = temp
            Next
        End While
    End Sub
    
    ' Helper function to check if array is sorted
    Function IsSorted(array() As Integer) As Boolean
        For i As Integer = 0 To array.Length - 2
            If array(i) > array(i + 1) Then
                Return False
            End If
        Next
        Return True
    End Function
    
    ' Helper function to print array
    Sub PrintArray(array() As Integer)
        Console.Write("[")
        For i As Integer = 0 To array.Length - 1
            Console.Write(array(i))
            If i < array.Length - 1 Then
                Console.Write(", ")
            End If
        Next
        Console.WriteLine("]")
    End Sub
    
    ' Main program
    Sub Main()
        ' Example usage
        Dim testArray() As Integer = {3, 1, 4, 1, 5, 9, 2, 6}
        
        Console.WriteLine("Original array:")
        PrintArray(testArray)
        
        Console.WriteLine("Sorting...")
        BogoSort(testArray)
        
        Console.WriteLine("Sorted array:")
        PrintArray(testArray)
        
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

This Visual Basic implementation of BogoSort demonstrates:

1. **BogoSort Algorithm**: Repeatedly shuffles the array randomly until it becomes sorted
2. **Helper Functions**: 
   - `IsSorted()` - checks if the array is in ascending order
   - `PrintArray()` - displays the array elements
3. **Main Logic**: The sorting process continues in a while loop until `IsSorted()` returns true
4. **Random Shuffling**: Uses `Random.Next()` to generate random indices for swapping elements

**Note**: BogoSort has an average time complexity of O(n!) and should only be used for educational purposes, as it's extremely inefficient for practical applications.

