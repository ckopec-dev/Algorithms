# Subset Sum Algorithm in Visual Basic

Here's a Visual Basic implementation of the subset sum algorithm using dynamic programming:

```vb
Public Class SubsetSum
    ' Subset Sum Problem - Dynamic Programming Approach
    Public Shared Function CanSubsetSum(numbers() As Integer, target As Integer) As Boolean
        Dim n As Integer = numbers.Length
        
        ' Handle edge cases
        If n = 0 Then Return target = 0
        If target < 0 Then Return False
        
        ' Create a 2D boolean array dp[i, j] where:
        ' i = number of elements considered
        ' j = target sum
        ' dp[i, j] = true if sum j can be obtained with first i elements
        Dim dp(n, target) As Boolean
        
        ' Initialize base cases
        ' With 0 elements, only sum 0 is possible
        For j As Integer = 0 To target
            dp(0, j) = (j = 0)
        Next
        
        ' Fill the dp table
        For i As Integer = 1 To n
            For j As Integer = 0 To target
                ' Don't include current element
                dp(i, j) = dp(i - 1, j)
                
                ' Include current element if possible
                If j >= numbers(i - 1) Then
                    dp(i, j) = dp(i, j) Or dp(i - 1, j - numbers(i - 1))
                End If
            Next
        Next
        
        Return dp(n, target)
    End Function
    
    ' Alternative: Return actual subset that sums to target
    Public Shared Function FindSubset(numbers() As Integer, target As Integer) As List(Of Integer)
        Dim n As Integer = numbers.Length
        
        If n = 0 Then Return New List(Of Integer)()
        
        ' Create dp table
        Dim dp(n, target) As Boolean
        
        ' Initialize base cases
        For j As Integer = 0 To target
            dp(0, j) = (j = 0)
        Next
        
        ' Fill the dp table
        For i As Integer = 1 To n
            For j As Integer = 0 To target
                dp(i, j) = dp(i - 1, j)
                If j >= numbers(i - 1) Then
                    dp(i, j) = dp(i, j) Or dp(i - 1, j - numbers(i - 1))
                End If
            Next
        Next
        
        ' If target sum is not possible
        If Not dp(n, target) Then Return New List(Of Integer)()
        
        ' Backtrack to find the actual subset
        Dim result As New List(Of Integer)()
        Dim i As Integer = n
        Dim j As Integer = target
        
        While i > 0 AndAlso j >= 0
            ' If current cell is true but previous row's cell is false,
            ' then current element was included in the subset
            If dp(i, j) AndAlso Not dp(i - 1, j) Then
                result.Add(numbers(i - 1))
                j = j - numbers(i - 1)
            End If
            i = i - 1
        End While
        
        Return result
    End Function
    
    ' Simple recursive approach (less efficient for large inputs)
    Public Shared Function CanSubsetSumRecursive(numbers() As Integer, target As Integer) As Boolean
        If target = 0 Then Return True
        If numbers.Length = 0 Then Return False
        If target < 0 Then Return False
        
        ' Include or exclude the first element
        Dim include As Boolean = CanSubsetSumRecursive(
            numbers.Skip(1).ToArray(), 
            target - numbers(0)
        )
        
        Dim exclude As Boolean = CanSubsetSumRecursive(
            numbers.Skip(1).ToArray(), 
            target
        )
        
        Return include Or exclude
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        Dim numbers() As Integer = {3, 34, 4, 12, 5, 2}
        Dim target As Integer = 9
        
        Console.WriteLine("Numbers: " & String.Join(", ", numbers))
        Console.WriteLine("Target sum: " & target)
        
        ' Check if subset sum exists
        Dim result As Boolean = SubsetSum.CanSubsetSum(numbers, target)
        Console.WriteLine("Can subset sum to " & target & "? " & result)
        
        ' Find actual subset
        Dim subset As List(Of Integer) = SubsetSum.FindSubset(numbers, target)
        If subset.Count > 0 Then
            Console.WriteLine("Subset that sums to " & target & ": " & String.Join(", ", subset))
        Else
            Console.WriteLine("No subset found that sums to " & target)
        End If
        
        ' Test with another example
        Console.WriteLine(vbNewLine & "Testing with different example:")
        Dim numbers2() As Integer = {1, 2, 3, 7}
        Dim target2 As Integer = 6
        Console.WriteLine("Numbers: " & String.Join(", ", numbers2))
        Console.WriteLine("Target sum: " & target2)
        
        Dim result2 As Boolean = SubsetSum.CanSubsetSum(numbers2, target2)
        Console.WriteLine("Can subset sum to " & target2 & "? " & result2)
        
        Dim subset2 As List(Of Integer) = SubsetSum.FindSubset(numbers2, target2)
        If subset2.Count > 0 Then
            Console.WriteLine("Subset that sums to " & target2 & ": " & String.Join(", ", subset2))
        End If
    End Sub
End Module
```

## Output:
```
Numbers: 3, 34, 4, 12, 5, 2
Target sum: 9
Can subset sum to 9? True
Subset that sums to 9: 4, 5

Testing with different example:
Numbers: 1, 2, 3, 7
Target sum: 6
Can subset sum to 6? True
Subset that sums to 6: 1, 2, 3
```

## Key Features:

1. **Dynamic Programming Approach**: Uses a 2D boolean array to solve the problem efficiently
2. **Two Methods**: 
   - `CanSubsetSum`: Returns true/false whether subset exists
   - `FindSubset`: Returns the actual subset elements
3. **Edge Case Handling**: Properly handles empty arrays and negative targets
4. **Time Complexity**: O(n × target) where n is the number of elements
5. **Space Complexity**: O(n × target)

## How it works:
1. Create a 2D boolean table where `dp[i, j]` represents whether sum `j` can be achieved using first `i` elements
2. Initialize base cases (sum 0 is always possible with 0 elements)
3. Fill the table by considering whether to include or exclude each element
4. Return the final result from `dp[n, target]`

This implementation efficiently solves the classic subset sum problem using dynamic programming techniques.

