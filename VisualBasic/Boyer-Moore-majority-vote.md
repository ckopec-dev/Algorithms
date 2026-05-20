# Boyer-Moore Majority Vote Algorithm in Visual Basic

```vb
Public Class BoyerMooreMajorityVote
    ''' <summary>
    ''' Finds the majority element in an array using Boyer-Moore Majority Vote Algorithm
    ''' </summary>
    ''' <param name="nums">Array of integers</param>
    ''' <returns>The majority element if exists, otherwise -1</returns>
    Public Shared Function FindMajorityElement(nums() As Integer) As Integer
        If nums Is Nothing OrElse nums.Length = 0 Then
            Return -1
        End If
        
        ' Phase 1: Find candidate
        Dim candidate As Integer = nums(0)
        Dim count As Integer = 1
        
        For i As Integer = 1 To nums.Length - 1
            If nums(i) = candidate Then
                count += 1
            Else
                count -= 1
                If count = 0 Then
                    candidate = nums(i)
                    count = 1
                End If
            End If
        Next
        
        ' Phase 2: Verify candidate is actually majority
        count = 0
        For Each num As Integer In nums
            If num = candidate Then
                count += 1
            End If
        Next
        
        ' Return candidate if it appears more than n/2 times
        If count > nums.Length \ 2 Then
            Return candidate
        Else
            Return -1
        End If
    End Function
    
    ''' <summary>
    ''' Alternative implementation that returns all elements that appear more than n/3 times
    ''' </summary>
    ''' <param name="nums">Array of integers</param>
    ''' <returns>List of majority elements</returns>
    Public Shared Function FindMajorityElements(nums() As Integer) As List(Of Integer)
        Dim result As New List(Of Integer)
        
        If nums Is Nothing OrElse nums.Length = 0 Then
            Return result
        End If
        
        ' For finding elements that appear more than n/3 times
        Dim candidate1 As Integer = 0
        Dim candidate2 As Integer = 0
        Dim count1 As Integer = 0
        Dim count2 As Integer = 0
        
        ' Phase 1: Find candidates
        For Each num As Integer In nums
            If count1 > 0 AndAlso num = candidate1 Then
                count1 += 1
            ElseIf count2 > 0 AndAlso num = candidate2 Then
                count2 += 1
            ElseIf count1 = 0 Then
                candidate1 = num
                count1 = 1
            ElseIf count2 = 0 Then
                candidate2 = num
                count2 = 1
            Else
                ' Decrease counts for all candidates
                count1 -= 1
                count2 -= 1
            End If
        Next
        
        ' Phase 2: Verify candidates
        count1 = 0
        count2 = 0
        For Each num As Integer In nums
            If num = candidate1 Then
                count1 += 1
            ElseIf num = candidate2 Then
                count2 += 1
            End If
        Next
        
        If count1 > nums.Length \ 3 Then
            result.Add(candidate1)
        End If
        
        If count2 > nums.Length \ 3 Then
            result.Add(candidate2)
        End If
        
        Return result
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Example 1: Find majority element
        Dim nums1() As Integer = {3, 2, 3}
        Dim majority1 As Integer = BoyerMooreMajorityVote.FindMajorityElement(nums1)
        Console.WriteLine($"Majority element in {String.Join(", ", nums1)}: {majority1}")
        
        ' Example 2: Find majority element
        Dim nums2() As Integer = {2, 2, 1, 1, 1, 2, 2}
        Dim majority2 As Integer = BoyerMooreMajorityVote.FindMajorityElement(nums2)
        Console.WriteLine($"Majority element in {String.Join(", ", nums2)}: {majority2}")
        
        ' Example 3: Find elements appearing more than n/3 times
        Dim nums3() As Integer = {1, 1, 1, 3, 3, 2, 2, 2}
        Dim majority3 As List(Of Integer) = BoyerMooreMajorityVote.FindMajorityElements(nums3)
        Console.WriteLine($"Elements appearing more than n/3 in {String.Join(", ", nums3)}: [{String.Join(", ", majority3)}]")
        
        Console.ReadKey()
    End Sub
End Module
```

## Algorithm Explanation

**Time Complexity:** O(n)  
**Space Complexity:** O(1)

### How it works:

1. **Phase 1 - Finding Candidate:**
   - Initialize first element as candidate with count 1
   - For each subsequent element:
     - If same as candidate, increment count
     - If different, decrement count
     - If count reaches 0, replace candidate with current element

2. **Phase 2 - Verification:**
   - Count actual occurrences of the candidate
   - Return candidate only if it appears more than n/2 times

### Example Output:
```
Majority element in 3, 2, 3: 3
Majority element in 2, 2, 1, 1, 1, 2, 2: 2
Elements appearing more than n/3 in 1, 1, 1, 3, 3, 2, 2, 2: [1, 2]
```

