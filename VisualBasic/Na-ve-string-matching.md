Here is a compact, easy‑to‑read implementation of the **Naïve (brute‑force) string‑matching** algorithm written in **Visual Basic .NET** (VB.NET).  
It searches for all occurrences of a pattern `Pat` inside a text `Txt` and returns a list of starting indices (0‑based) where the pattern matches.

```vbnet
'=====================================================================
'  Naïve String Matching (Brute‑Force) – VB.NET
'=====================================================================
'  Input :  Txt – the text to be searched
'           Pat – the pattern to look for
'  Output:  List<Integer> containing the start positions of each match
'=====================================================================
Public Function NaiveStringMatch(ByVal Txt As String, ByVal Pat As String) _
                                As List(Of Integer)

    Dim matches As New List(Of Integer)()

    ' Guard against empty pattern – by convention we return an empty list
    If String.IsNullOrEmpty(Pat) Then Return matches

    Dim n As Integer = Txt.Length
    Dim m As Integer = Pat.Length

    ' No possible match if pattern longer than text
    If m > n Then Return matches

    ' Slide the pattern over the text one character at a time
    For i As Integer = 0 To n - m
        Dim j As Integer = 0

        ' Compare characters of Pat and the current window of Txt
        While j < m AndAlso Txt(i + j) = Pat(j)
            j += 1
        End While

        ' If we reached the end of the pattern, we found a match
        If j = m Then
            matches.Add(i)
        End If
    Next

    Return matches
End Function
```

### How to use it

```vbnet
Sub Main()
    Dim text    As String = "ababcabcabababd"
    Dim pattern As String = "ababd"

    Dim result As List(Of Integer) = NaiveStringMatch(text, pattern)

    Console.WriteLine("Pattern found at positions: " & String.Join(", ", result))
    ' Output: Pattern found at positions: 10
End Sub
```

**Explanation of the core loop**

| Step | Description |
|------|-------------|
| `For i = 0 To n‑m` | Positions where the pattern could start (the left‑most index of the current window). |
| `While j < m AndAlso Txt(i + j) = Pat(j)` | Character‑by‑character comparison; stops early on a mismatch. |
| `If j = m Then …` | All `m` characters matched → record the start index `i`. |

The algorithm runs in **O((n‑m+1)·m)** time in the worst case (e.g., `Txt = "aaaaa…a"` and `Pat = "aaab"`), which is why it’s called *naïve*—but it is simple, deterministic, and works perfectly for small inputs or educational purposes.