# Longest Increasing Subsequence (LIS) Algorithm in Visual Basic

Here's a complete implementation of the LIS algorithm in Visual Basic with explanation:

```vb
Module LISAlgorithm
    Sub Main()
        ' Example array
        Dim arr() As Integer = {10, 22, 9, 33, 21, 50, 41, 60, 80}
        
        Console.WriteLine("Array: " & String.Join(", ", arr))
        
        ' Find LIS
        Dim result As Integer = FindLIS(arr)
        Console.WriteLine("Length of Longest Increasing Subsequence: " & result)
        
        ' Show the actual subsequence
        Dim subsequence As List(Of Integer) = GetLIS(arr)
        Console.WriteLine("Longest Increasing Subsequence: " & String.Join(", ", subsequence))
        
        Console.ReadLine()
    End Sub
    
    ' Function to find the length of LIS
    Function FindLIS(arr() As Integer) As Integer
        If arr.Length = 0 Then Return 0
        
        ' Create DP array where dp(i) represents length of LIS ending at index i
        Dim dp(arr.Length - 1) As Integer
        
        ' Initialize all positions with 1 (each element forms a subsequence of length 1)
        For i As Integer = 0 To arr.Length - 1
            dp(i) = 1
        Next
        
        ' Fill dp array using dynamic programming
        For i As Integer = 1 To arr.Length - 1
            For j As Integer = 0 To i - 1
                If arr(i) > arr(j) AndAlso dp(i) < dp(j) + 1 Then
                    dp(i) = dp(j) + 1
                End If
            Next
        Next
        
        ' Find maximum value in dp array
        Dim maxLength As Integer = dp(0)
        For i As Integer = 1 To arr.Length - 1
            If dp(i) > maxLength Then
                maxLength = dp(i)
            End If
        Next
        
        Return maxLength
    End Function
    
    ' Function to get the actual LIS subsequence
    Function GetLIS(arr() As Integer) As List(Of Integer)
        If arr.Length = 0 Then Return New List(Of Integer)()
        
        ' Create DP array
        Dim dp(arr.Length - 1) As Integer
        Dim parent(arr.Length - 1) As Integer
        
        ' Initialize arrays
        For i As Integer = 0 To arr.Length - 1
            dp(i) = 1
            parent(i) = -1
        Next
        
        ' Fill dp and parent arrays
        For i As Integer = 1 To arr.Length - 1
            For j As Integer = 0 To i - 1
                If arr(i) > arr(j) AndAlso dp(i) < dp(j) + 1 Then
                    dp(i) = dp(j) + 1
                    parent(i) = j
                End If
            Next
        Next
        
        ' Find the index with maximum LIS length
        Dim maxIndex As Integer = 0
        For i As Integer = 1 To arr.Length - 1
            If dp(i) > dp(maxIndex) Then
                maxIndex = i
            End If
        Next
        
        ' Reconstruct the LIS
        Dim lis As New List(Of Integer)()
        Dim currentIndex As Integer = maxIndex
        
        While currentIndex <> -1
            lis.Add(arr(currentIndex))
            currentIndex = parent(currentIndex)
        End While
        
        ' Reverse to get correct order
        lis.Reverse()
        Return lis
    End Function
End Module
```

## Output:
```
Array: 10, 22, 9, 33, 21, 50, 41, 60, 80
Length of Longest Increasing Subsequence: 6
Longest Increasing Subsequence: 10, 22, 33, 50, 60, 80
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: Uses a `dp` array where `dp[i]` represents the length of the longest increasing subsequence ending at index `i`.

2. **Initialization**: Each element forms a subsequence of length 1, so `dp[i] = 1` for all `i`.

3. **Fill DP Array**: For each element, check all previous elements. If current element is greater than a previous element, update `dp[i]` if it gives a longer subsequence.

4. **Reconstruction**: To get the actual subsequence, maintain a `parent` array to track the path and reconstruct the sequence.

## Time Complexity: O(n²)
## Space Complexity: O(n)

This implementation finds both the length of the longest increasing subsequence and the actual subsequence itself.

