## Insertion Sort in Visual Basic (VB.NET)

Below is a simple implementation of the **Insertion Sort** algorithm that sorts an array of integers in ascending order. The code is written for a console application, but the core `InsertionSort` routine can be used in any VB.NET project (WinForms, WPF, etc.).

```vb
Imports System

Module Program

    '--- Entry point -------------------------------------------------
    Sub Main()
        Dim numbers() As Integer = {5, 2, 9, 1, 5, 6}

        Console.WriteLine("Unsorted array: " & String.Join(", ", numbers))
        InsertionSort(numbers)
        Console.WriteLine("Sorted array:   " & String.Join(", ", numbers))
    End Sub

    '--- Insertion Sort ------------------------------------------------
    ''' <summary>
    ''' Sorts an integer array in place using the insertion‑sort algorithm.
    ''' </summary>
    ''' <param name="arr">The array to be sorted.</param>
    Private Sub InsertionSort(ByVal arr() As Integer)
        ' Loop from the second element (index 1) to the end
        For i As Integer = 1 To arr.Length - 1
            Dim key As Integer = arr(i)          ' Element to be inserted
            Dim j As Integer = i - 1             ' Index of the element before key

            ' Move elements of arr[0..i-1] that are greater than key
            ' one position to the right
            While j >= 0 AndAlso arr(j) > key
                arr(j + 1) = arr(j)
                j -= 1
            End While

            ' Place the key after the element just smaller than it
            arr(j + 1) = key
        Next
    End Sub

End Module
```

### How it works
1. **Outer loop (`i`)** picks each element (`key`) starting from the second position.
2. **Inner loop (`While`)** shifts all larger elements in the sorted portion (`arr[0..i‑1]`) one slot to the right.
3. When the correct spot is found, `key` is placed there (`arr(j + 1) = key`).
4. After processing all elements, the entire array is sorted.

### Sample output
```
Unsorted array: 5, 2, 9, 1, 5, 6
Sorted array:   1, 2, 5, 5, 6, 9
```

Feel free to replace the `Integer` type with `Double`, `String`, or any comparable type—just adjust the comparison (`>`) accordingly. Happy coding!