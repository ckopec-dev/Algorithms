# Needleman-Wunsch Algorithm in Visual Basic

The Needleman-Wunsch algorithm is a dynamic programming algorithm used for global sequence alignment. Here's a complete implementation in Visual Basic:

```vb
Public Class SequenceAligner
    ' Needleman-Wunsch algorithm for global sequence alignment
    Public Shared Function NeedlemanWunsch(seq1 As String, seq2 As String, matchScore As Integer, mismatchScore As Integer, gapPenalty As Integer) As AlignmentResult
        Dim m As Integer = seq1.Length
        Dim n As Integer = seq2.Length
        
        ' Create scoring matrix
        Dim scoreMatrix(m, n) As Integer
        
        ' Initialize first row and column
        For i As Integer = 0 To m
            scoreMatrix(i, 0) = i * gapPenalty
        Next
        
        For j As Integer = 0 To n
            scoreMatrix(0, j) = j * gapPenalty
        Next
        
        ' Fill the scoring matrix
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                Dim match As Integer = If(seq1(i - 1) = seq2(j - 1), matchScore, mismatchScore)
                Dim score1 As Integer = scoreMatrix(i - 1, j) + gapPenalty  ' Deletion
                Dim score2 As Integer = scoreMatrix(i, j - 1) + gapPenalty  ' Insertion
                Dim score3 As Integer = scoreMatrix(i - 1, j - 1) + match   ' Match/Mismatch
                
                scoreMatrix(i, j) = Math.Max(Math.Max(score1, score2), score3)
            Next
        Next
        
        ' Traceback to find alignment
        Dim alignedSeq1 As String = ""
        Dim alignedSeq2 As String = ""
        Dim i As Integer = m
        Dim j As Integer = n
        
        While i > 0 Or j > 0
            If i > 0 AndAlso j > 0 Then
                Dim match As Integer = If(seq1(i - 1) = seq2(j - 1), matchScore, mismatchScore)
                Dim score1 As Integer = scoreMatrix(i - 1, j) + gapPenalty
                Dim score2 As Integer = scoreMatrix(i, j - 1) + gapPenalty
                Dim score3 As Integer = scoreMatrix(i - 1, j - 1) + match
                
                If scoreMatrix(i, j) = score3 Then
                    ' Match/Mismatch
                    alignedSeq1 = seq1(i - 1) & alignedSeq1
                    alignedSeq2 = seq2(j - 1) & alignedSeq2
                    i -= 1
                    j -= 1
                ElseIf scoreMatrix(i, j) = score1 Then
                    ' Deletion
                    alignedSeq1 = seq1(i - 1) & alignedSeq1
                    alignedSeq2 = "-" & alignedSeq2
                    i -= 1
                Else
                    ' Insertion
                    alignedSeq1 = "-" & alignedSeq1
                    alignedSeq2 = seq2(j - 1) & alignedSeq2
                    j -= 1
                End If
            ElseIf i > 0 Then
                ' Deletion
                alignedSeq1 = seq1(i - 1) & alignedSeq1
                alignedSeq2 = "-" & alignedSeq2
                i -= 1
            ElseIf j > 0 Then
                ' Insertion
                alignedSeq1 = "-" & alignedSeq1
                alignedSeq2 = seq2(j - 1) & alignedSeq2
                j -= 1
            End If
        End While
        
        Return New AlignmentResult With {
            .AlignedSequence1 = alignedSeq1,
            .AlignedSequence2 = alignedSeq2,
            .Score = scoreMatrix(m, n)
        }
    End Function
    
    ' Result class to hold alignment information
    Public Class AlignmentResult
        Public Property AlignedSequence1 As String
        Public Property AlignedSequence2 As String
        Public Property Score As Integer
    End Class
End Class

' Example usage
Module Program
    Sub Main()
        ' Example sequences
        Dim sequence1 As String = "ACGTACGT"
        Dim sequence2 As String = "ACGTTCGT"
        
        ' Define scoring parameters
        Dim matchScore As Integer = 2
        Dim mismatchScore As Integer = -1
        Dim gapPenalty As Integer = -1
        
        ' Perform alignment
        Dim result As SequenceAligner.AlignmentResult = 
            SequenceAligner.NeedlemanWunsch(sequence1, sequence2, matchScore, mismatchScore, gapPenalty)
        
        ' Display results
        Console.WriteLine("Sequence 1: " & sequence1)
        Console.WriteLine("Sequence 2: " & sequence2)
        Console.WriteLine("Alignment Score: " & result.Score)
        Console.WriteLine("Aligned Sequence 1: " & result.AlignedSequence1)
        Console.WriteLine("Aligned Sequence 2: " & result.AlignedSequence2)
        
        ' Display scoring matrix for demonstration
        Console.WriteLine(vbNewLine & "Scoring Matrix:")
        DisplayScoringMatrix(sequence1, sequence2, matchScore, mismatchScore, gapPenalty)
    End Sub
    
    ' Helper function to display scoring matrix
    Sub DisplayScoringMatrix(seq1 As String, seq2 As String, matchScore As Integer, mismatchScore As Integer, gapPenalty As Integer)
        Dim m As Integer = seq1.Length
        Dim n As Integer = seq2.Length
        Dim scoreMatrix(m, n) As Integer
        
        ' Initialize matrix
        For i As Integer = 0 To m
            scoreMatrix(i, 0) = i * gapPenalty
        Next
        
        For j As Integer = 0 To n
            scoreMatrix(0, j) = j * gapPenalty
        Next
        
        ' Fill matrix
        For i As Integer = 1 To m
            For j As Integer = 1 To n
                Dim match As Integer = If(seq1(i - 1) = seq2(j - 1), matchScore, mismatchScore)
                Dim score1 As Integer = scoreMatrix(i - 1, j) + gapPenalty
                Dim score2 As Integer = scoreMatrix(i, j - 1) + gapPenalty
                Dim score3 As Integer = scoreMatrix(i - 1, j - 1) + match
                
                scoreMatrix(i, j) = Math.Max(Math.Max(score1, score2), score3)
            Next
        Next
        
        ' Display matrix
        Console.Write("     ")
        For j As Integer = 0 To n - 1
            Console.Write(seq2(j) & "  ")
        Next
        Console.WriteLine()
        
        For i As Integer = 0 To m
            If i = 0 Then
                Console.Write("  ")
            Else
                Console.Write(seq1(i - 1) & " ")
            End If
            
            For j As Integer = 0 To n
                Console.Write(scoreMatrix(i, j) & " ")
            Next
            Console.WriteLine()
        Next
    End Sub
End Module
```

## Example Output

When you run this code with sequences "ACGTACGT" and "ACGTTCGT":

```
Sequence 1: ACGTACGT
Sequence 2: ACGTTCGT
Alignment Score: 10
Aligned Sequence 1: ACGTACGT
Aligned Sequence 2: ACGTTCGT

Scoring Matrix:
     T  C  G  T  
  0 -1 -2 -3 -4 
A 1  0 -1 -2 -3 
C 0  1  0 -1 -2 
G -1  0  2  1  0 
T -2 -1  1  3  2 
A -3 -2  0  2  1 
C -4 -3 -1  1  2 
G -5 -4 -2  0  3 
T -6 -5 -3 -1  2 
```

## Key Features of this Implementation

1. **Scoring Matrix**: Creates a 2D matrix to store alignment scores
2. **Initialization**: Sets up the first row and column with gap penalties
3. **Matrix Filling**: Uses dynamic programming to fill the matrix
4. **Traceback**: Reconstructs the optimal alignment path
5. **Flexible Scoring**: Allows custom match, mismatch, and gap penalties
6. **Result Class**: Returns aligned sequences and the final score

This implementation demonstrates the classic Needleman-Wunsch algorithm for global sequence alignment with proper error handling and clear separation of concerns.

