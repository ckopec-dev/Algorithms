# Heap Sort Algorithm in Visual Basic

Here's an implementation of the Heap Sort algorithm in Visual Basic:

```vb
Module HeapSort
    Sub Main()
        ' Example array to sort
        Dim arr() As Integer = {64, 34, 25, 12, 22, 11, 90, 5}
        
        Console.WriteLine("Original array:")
        PrintArray(arr)
        
        ' Perform heap sort
        HeapSortArray(arr)
        
        Console.WriteLine("Sorted array:")
        PrintArray(arr)
        
        Console.ReadLine()
    End Sub
    
    ' Main heap sort function
    Sub HeapSortArray(ByRef arr() As Integer)
        Dim n As Integer = arr.Length
        
        ' Build max heap
        For i As Integer = n \ 2 - 1 To 0 Step -1
            Heapify(arr, n, i)
        Next
        
        ' Extract elements from heap one by one
        For i As Integer = n - 1 To 1 Step -1
            ' Move current root to end
            Dim temp As Integer = arr(0)
            arr(0) = arr(i)
            arr(i) = temp
            
            ' Call heapify on the reduced heap
            Heapify(arr, i, 0)
        Next
    End Sub
    
    ' Heapify function to maintain heap property
    Sub Heapify(ByRef arr() As Integer, ByVal n As Integer, ByVal i As Integer)
        Dim largest As Integer = i        ' Initialize largest as root
        Dim left As Integer = 2 * i + 1   ' Left child
        Dim right As Integer = 2 * i + 2  ' Right child
        
        ' If left child exists and is greater than root
        If left < n AndAlso arr(left) > arr(largest) Then
            largest = left
        End If
        
        ' If right child exists and is greater than largest so far
        If right < n AndAlso arr(right) > arr(largest) Then
            largest = right
        End If
        
        ' If largest is not root
        If largest <> i Then
            ' Swap root with largest
            Dim temp As Integer = arr(i)
            arr(i) = arr(largest)
            arr(largest) = temp
            
            ' Recursively heapify the affected sub-tree
            Heapify(arr, n, largest)
        End If
    End Sub
    
    ' Helper function to print array
    Sub PrintArray(ByRef arr() As Integer)
        For i As Integer = 0 To arr.Length - 1
            Console.Write(arr(i) & " ")
        Next
        Console.WriteLine()
    End Sub
End Module
```

## How it works:

1. **Build Max Heap**: Convert the array into a max heap by calling `Heapify` on all non-leaf nodes starting from the last non-leaf node.

2. **Extract Elements**: Repeatedly extract the maximum element (root) and place it at the end of the array, then restore the heap property.

3. **Heapify**: Maintains the heap property by ensuring that each parent node is greater than or equal to its children.

## Output:
```
Original array:
64 34 25 12 22 11 90 5 
Sorted array:
5 11 12 22 25 34 64 90 
```

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

The algorithm works by first building a max heap from the input array, then repeatedly extracting the maximum element and rebuilding the heap until all elements are sorted.

