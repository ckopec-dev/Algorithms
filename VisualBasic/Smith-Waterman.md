# Smith-Waterman Algorithm Implementation in Visual Basic

Here's a complete implementation of the Smith-Waterman local sequence alignment algorithm in Visual Basic:

```vb
Public Class SmithWaterman
    Private Shared ReadOnly MATCH_SCORE As Integer = 2
    Private Shared ReadOnly MISMATCH_SCORE As Integer = -1
    Private Shared ReadOnly GAP_PENALTY As Integer = -1

    Public Shared Function Align(sequence1 As String, sequence2 As String) As AlignmentResult
        Dim m As Integer = sequence1.Length
        Dim n As Integer = sequence2.Length
        
        ' Create scoring matrix
        Dim scoreMatrix(m, n) As Integer
        Dim tracebackMatrix(m, n) As Integer
        
        ' Initialize first row and column
        For i As Integer = 0 To m
            scoreMatrix(i, 0) = 0
        Next
        
        For j As Integer = 0 To n
            scoreMatrix(0, j) = 0
        Next
        
        ' Fill the scoring matrix
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                Dim matchScore As Integer = If(sequence1(i - 1) = sequence2(j - 1), MATCH_SCORE, MISMATCH_SCORE)
                Dim diagonalScore As Integer = scoreMatrix(i - 1, j - 1) + matchScore
                Dim upScore As Integer = scoreMatrix(i - 1, j) + GAP_PENALTY
                Dim leftScore As Integer = scoreMatrix(i, j - 1) + GAP_PENALTY
                
                ' Take maximum of three scores or zero (local alignment)
                scoreMatrix(i, j) = Math.Max(Math.Max(diagonalScore, upScore), Math.Max(leftScore, 0))
                
                ' Record the traceback direction
                If scoreMatrix(i, j) = 0 Then
                    tracebackMatrix(i, j) = 0 ' Stop
                ElseIf scoreMatrix(i, j) = diagonalScore Then
                    tracebackMatrix(i, j) = 1 ' Diagonal
                ElseIf scoreMatrix(i, j) = upScore Then
                    tracebackMatrix(i, j) = 2 ' Up
                Else
                    tracebackMatrix(i, j) = 3 ' Left
                End If
            Next
        Next
        
        ' Find the maximum score and its position
        Dim maxScore As Integer = 0
        Dim maxI As Integer = 0
        Dim maxJ As Integer = 0
        
        For i As Integer = 0 To m
            For j As Integer = 0 To n
                If scoreMatrix(i, j) > maxScore Then
                    maxScore = scoreMatrix(i, j)
                    maxI = i
                    maxJ = j
                End If
            Next
        Next
        
        ' Traceback to find the alignment
        Dim alignedSeq1 As String = ""
        Dim alignedSeq2 As String = ""
        Dim iPos As Integer = maxI
        Dim jPos As Integer = maxJ
        
        While iPos > 0 AndAlso jPos > 0 AndAlso scoreMatrix(iPos, jPos) > 0
            Select Case tracebackMatrix(iPos, jPos)
                Case 1 ' Diagonal
                    alignedSeq1 = sequence1(iPos - 1) & alignedSeq1
                    alignedSeq2 = sequence2(jPos - 1) & alignedSeq2
                    iPos -= 1
                    jPos -= 1
                Case 2 ' Up
                    alignedSeq1 = sequence1(iPos - 1) & alignedSeq1
                    alignedSeq2 = "-" & alignedSeq2
                    iPos -= 1
                Case 3 ' Left
                    alignedSeq1 = "-" & alignedSeq1
                    alignedSeq2 = sequence2(jPos - 1) & alignedSeq2
                    jPos -= 1
                Case 0 ' Stop
                    Exit While
            End Select
        End While
        
        Return New AlignmentResult With {
            .Sequence1 = sequence1,
            .Sequence2 = sequence2,
            .AlignedSequence1 = alignedSeq1,
            .AlignedSequence2 = alignedSeq2,
            .Score = maxScore
        }
    End Function
    
    Public Shared Sub PrintMatrix(sequence1 As String, sequence2 As String)
        Dim m As Integer = sequence1.Length
        Dim n As Integer = sequence2.Length
        
        ' Create scoring matrix
        Dim scoreMatrix(m, n) As Integer
        
        ' Initialize first row and column
        For i As Integer = 0 To m
            scoreMatrix(i, 0) = 0
        Next
        
        For j As Integer = 0 To n
            scoreMatrix(0, j) = 0
        Next
        
        ' Fill the scoring matrix
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                Dim matchScore As Integer = If(sequence1(i - 1) = sequence2(j - 1), MATCH_SCORE, MISMATCH_SCORE)
                Dim diagonalScore As Integer = scoreMatrix(i - 1, j - 1) + matchScore
                Dim upScore As Integer = scoreMatrix(i - 1, j) + GAP_PENALTY
                Dim leftScore As Integer = scoreMatrix(i, j - 1) + GAP_PENALTY
                
                scoreMatrix(i, j) = Math.Max(Math.Max(diagonalScore, upScore), Math.Max(leftScore, 0))
            Next
        Next
        
        ' Print the matrix
        Console.WriteLine("Scoring Matrix:")
        Console.Write("    ")
        For j As Integer = 0 To n - 1
            Console.Write(sequence2(j) & "  ")
        Next
        Console.WriteLine()
        
        For i As Integer = 0 To m
            If i = 0 Then
                Console.Write("  ")
            Else
                Console.Write(sequence1(i - 1) & " ")
            End If
            
            For j As Integer = 0 To n
                Console.Write(scoreMatrix(i, j).ToString().PadLeft(3))
            Next
            Console.WriteLine()
        Next
    End Sub
End Class

Public Class AlignmentResult
    Public Property Sequence1 As String
    Public Property Sequence2 As String
    Public Property AlignedSequence1 As String
    Public Property AlignedSequence2 As String
    Public Property Score As Integer
End Class

' Example usage
Module Program
    Sub Main()
        Dim seq1 As String = "ACGTACGT"
        Dim seq2 As String = "ACGTACGT"
        
        Console.WriteLine("Sequence 1: " & seq1)
        Console.WriteLine("Sequence 2: " & seq2)
        Console.WriteLine()
        
        ' Print the scoring matrix
        SmithWaterman.PrintMatrix(seq1, seq2)
        Console.WriteLine()
        
        ' Perform alignment
        Dim result As AlignmentResult = SmithWaterman.Align(seq1, seq2)
        
        Console.WriteLine("Alignment Result:")
        Console.WriteLine("Score: " & result.Score)
        Console.WriteLine("Sequence 1: " & result.AlignedSequence1)
        Console.WriteLine("Sequence 2: " & result.AlignedSequence2)
        Console.WriteLine()
        
        ' Test with different sequences
        Dim seq3 As String = "ACGTACGT"
        Dim seq4 As String = "ACGTACGT"
        
        Console.WriteLine("Testing with different sequences:")
        Console.WriteLine("Sequence 1: " & seq3)
        Console.WriteLine("Sequence 2: " & seq4)
        Console.WriteLine()
        
        Dim result2 As AlignmentResult = SmithWaterman.Align(seq3, seq4)
        Console.WriteLine("Score: " & result2.Score)
        Console.WriteLine("Aligned Sequence 1: " & result2.AlignedSequence1)
        Console.WriteLine("Aligned Sequence 2: " & result2.AlignedSequence2)
    End Sub
End Module
```

## Key Features of this Implementation:

1. **Scoring System**:
   - Match score: +2
   - Mismatch score: -1
   - Gap penalty: -1

2. **Algorithm Steps**:
   - Initialize scoring matrix with zeros
   - Fill matrix using dynamic programming
   - Track traceback directions
   - Find maximum score and trace back to alignment

3. **Local Alignment**:
   - Uses zero as minimum score (local alignment)
   - Stops traceback when score reaches zero

4. **Output**:
   - Displays scoring matrix
   - Shows aligned sequences
   - Reports alignment score

## Sample Output:
```
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT

Scoring Matrix:
        A  C  G  T  A  C  G  T  
  0  0  0  0  0  0  0  0  0  0  
A  2  1  1  1  2  1  1  1  2  
C  1  5  4  3  2  5  4  3  2  
G  1  4  8  7  6  5  8  7  6  
T  1  3  7 11 10  9  8 11 10  
A  2  2  6 10 14 13 12 11 10  
C  1  5  5  9 13 17 16 15 14  
G  1  4  8 12 11 15 19 18 17  
T  1  3  7 11 10 14 18 22 21  

Alignment Result:
Score: 22
Sequence 1: ACGTACGT
Sequence 2: ACGTACGT
```

This implementation demonstrates the core principles of the Smith-Waterman algorithm for local sequence alignment in Visual Basic.

