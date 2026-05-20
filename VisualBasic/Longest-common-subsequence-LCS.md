# Longest Common Subsequence (LCS) Algorithm in Visual Basic

Here's a complete implementation of the LCS algorithm in Visual Basic with example usage:

```vb
Module LCSModule
    ' Function to find the length of Longest Common Subsequence
    Function LCSLength(str1 As String, str2 As String) As Integer
        Dim m As Integer = str1.Length
        Dim n As Integer = str2.Length
        
        ' Create a 2D array to store lengths of LCS
        Dim dp(m, n) As Integer
        
        ' Initialize the first row and column
        For i As Integer = 0 To m
            dp(i, 0) = 0
        Next
        
        For j As Integer = 0 To n
            dp(0, j) = 0
        Next
        
        ' Fill the dp table
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                If str1(i - 1) = str2(j - 1) Then
                    dp(i, j) = dp(i - 1, j - 1) + 1
                Else
                    dp(i, j) = Math.Max(dp(i - 1, j), dp(i, j - 1))
                End If
            Next
        Next
        
        Return dp(m, n)
    End Function
    
    ' Function to find the actual LCS string
    Function LCSString(str1 As String, str2 As String) As String
        Dim m As Integer = str1.Length
        Dim n As Integer = str2.Length
        
        ' Create a 2D array to store lengths of LCS
        Dim dp(m, n) As Integer
        
        ' Initialize the first row and column
        For i As Integer = 0 To m
            dp(i, 0) = 0
        Next
        
        For j As Integer = 0 To n
            dp(0, j) = 0
        Next
        
        ' Fill the dp table
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                If str1(i - 1) = str2(j - 1) Then
                    dp(i, j) = dp(i - 1, j - 1) + 1
                Else
                    dp(i, j) = Math.Max(dp(i - 1, j), dp(i, j - 1))
                End If
            Next
        Next
        
        ' Backtrack to find the actual LCS string
        Dim result As New System.Text.StringBuilder()
        Dim i As Integer = m
        Dim j As Integer = n
        
        While i > 0 AndAlso j > 0
            If str1(i - 1) = str2(j - 1) Then
                result.Insert(0, str1(i - 1))
                i -= 1
                j -= 1
            ElseIf dp(i - 1, j) > dp(i, j - 1) Then
                i -= 1
            Else
                j -= 1
            End If
        End While
        
        Return result.ToString()
    End Function
    
    ' Main subroutine to demonstrate the LCS algorithm
    Sub Main()
        ' Example 1
        Dim str1 As String = "ABCDGH"
        Dim str2 As String = "AEDFHR"
        
        Console.WriteLine("Example 1:")
        Console.WriteLine("String 1: " & str1)
        Console.WriteLine("String 2: " & str2)
        Console.WriteLine("Length of LCS: " & LCSLength(str1, str2))
        Console.WriteLine("LCS String: " & LCSString(str1, str2))
        Console.WriteLine()
        
        ' Example 2
        str1 = "AGGTAB"
        str2 = "GXTXAYB"
        
        Console.WriteLine("Example 2:")
        Console.WriteLine("String 1: " & str1)
        Console.WriteLine("String 2: " & str2)
        Console.WriteLine("Length of LCS: " & LCSLength(str1, str2))
        Console.WriteLine("LCS String: " & LCSString(str1, str2))
        Console.WriteLine()
        
        ' Example 3
        str1 = "ABCD"
        str2 = "EFGH"
        
        Console.WriteLine("Example 3:")
        Console.WriteLine("String 1: " & str1)
        Console.WriteLine("String 2: " & str2)
        Console.WriteLine("Length of LCS: " & LCSLength(str1, str2))
        Console.WriteLine("LCS String: " & LCSString(str1, str2))
        Console.WriteLine()
        
        ' Example 4 - Same strings
        str1 = "HELLO"
        str2 = "HELLO"
        
        Console.WriteLine("Example 4:")
        Console.WriteLine("String 1: " & str1)
        Console.WriteLine("String 2: " & str2)
        Console.WriteLine("Length of LCS: " & LCSLength(str1, str2))
        Console.WriteLine("LCS String: " & LCSString(str1, str2))
        
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## Output:
```
Example 1:
String 1: ABCDGH
String 2: AEDFHR
Length of LCS: 3
LCS String: ADH

Example 2:
String 1: AGGTAB
String 2: GXTXAYB
Length of LCS: 4
LCS String: GTAB

Example 3:
String 1: ABCD
String 2: EFGH
Length of LCS: 0
LCS String: 

Example 4:
String 1: HELLO
String 2: HELLO
Length of LCS: 5
LCS String: HELLO

Press any key to exit...
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: Uses a 2D table `dp` where `dp[i,j]` represents the length of LCS for the first `i` characters of `str1` and first `j` characters of `str2`.

2. **Base Case**: First row and column are initialized to 0 since LCS with empty string is 0.

3. **Recurrence Relation**:
   - If characters match: `dp[i,j] = dp[i-1,j-1] + 1`
   - If characters don't match: `dp[i,j] = max(dp[i-1,j], dp[i,j-1])`

4. **Backtracking**: To reconstruct the actual LCS string, we trace back through the DP table from `dp[m,n]` to `dp[0,0]`.

## Time Complexity: O(m×n)
## Space Complexity: O(m×n)

Where `m` and `n` are the lengths of the two input strings.

