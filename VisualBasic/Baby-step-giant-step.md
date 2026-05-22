# Baby-step Giant-step Algorithm in Visual Basic

Here's a complete implementation of the Baby-step Giant-step algorithm in Visual Basic to solve the discrete logarithm problem:

```vb
Module BabyStepGiantStep
    ' Baby-step Giant-step algorithm to solve discrete logarithm problem
    ' Find x such that (g^x) ≡ h (mod p)
    Public Function BabyStepGiantStep(g As Long, h As Long, p As Long) As Long
        Dim m As Long
        Dim i As Long, j As Long
        Dim x As Long
        Dim lookup As Dictionary(Of Long, Long)
        Dim result As Long
        
        ' Calculate m = ceil(sqrt(p))
        m = CLng(Math.Ceiling(Math.Sqrt(p)))
        
        ' Initialize dictionary for baby steps
        lookup = New Dictionary(Of Long, Long)
        
        ' Baby steps: store g^j mod p for j = 0, 1, 2, ..., m-1
        x = 1
        For j = 0 To m - 1
            If lookup.ContainsKey(x) = False Then
                lookup.Add(x, j)
            End If
            x = (x * g) Mod p
        Next
        
        ' Calculate g^(-m) mod p
        Dim gInv As Long = ModInverse(g, p)
        Dim gNegM As Long = PowerMod(gInv, m, p)
        
        ' Giant steps: check h * (g^(-m))^i mod p
        x = h
        For i = 0 To m - 1
            If lookup.ContainsKey(x) Then
                result = i * m + lookup(x)
                Return result
            End If
            x = (x * gNegM) Mod p
        Next
        
        ' No solution found
        Return -1
    End Function
    
    ' Modular exponentiation: (base^exp) mod modulus
    Private Function PowerMod(base As Long, exp As Long, modulus As Long) As Long
        Dim result As Long = 1
        base = base Mod modulus
        
        While exp > 0
            If exp Mod 2 = 1 Then
                result = (result * base) Mod modulus
            End If
            exp = exp \ 2
            base = (base * base) Mod modulus
        End While
        
        Return result
    End Function
    
    ' Extended Euclidean Algorithm to find modular inverse
    Private Function ModInverse(a As Long, m As Long) As Long
        Dim gcd As Long = GCD(a, m)
        If gcd <> 1 Then
            Throw New ArgumentException("Modular inverse does not exist")
        End If
        
        Dim result As Long = ExtendedGCD(a, m).Item1
        Return (result + m) Mod m
    End Function
    
    ' Extended GCD to find coefficients for modular inverse
    Private Function ExtendedGCD(a As Long, b As Long) As Tuple(Of Long, Long)
        If b = 0 Then
            Return New Tuple(Of Long, Long)(1, 0)
        End If
        
        Dim result As Tuple(Of Long, Long) = ExtendedGCD(b, a Mod b)
        Dim x As Long = result.Item2
        Dim y As Long = result.Item1 - (a \ b) * result.Item2
        
        Return New Tuple(Of Long, Long)(x, y)
    End Function
    
    ' Greatest Common Divisor
    Private Function GCD(a As Long, b As Long) As Long
        While b <> 0
            Dim temp As Long = b
            b = a Mod b
            a = temp
        End While
        Return a
    End Function
    
    ' Example usage
    Sub Main()
        ' Example: Find x such that 3^x ≡ 13 (mod 17)
        ' This means we want to solve: 3^x ≡ 13 (mod 17)
        Dim g As Long = 3
        Dim h As Long = 13
        Dim p As Long = 17
        
        Console.WriteLine("Solving: " & g & "^x ≡ " & h & " (mod " & p & ")")
        
        Dim result As Long = BabyStepGiantStep(g, h, p)
        
        If result <> -1 Then
            Console.WriteLine("Solution found: x = " & result)
            
            ' Verify the result
            Dim verification As Long = PowerMod(g, result, p)
            Console.WriteLine("Verification: " & g & "^" & result & " ≡ " & verification & " (mod " & p & ")")
        Else
            Console.WriteLine("No solution found")
        End If
        
        Console.WriteLine()
        
        ' Another example: Find x such that 2^x ≡ 11 (mod 13)
        g = 2
        h = 11
        p = 13
        
        Console.WriteLine("Solving: " & g & "^x ≡ " & h & " (mod " & p & ")")
        
        result = BabyStepGiantStep(g, h, p)
        
        If result <> -1 Then
            Console.WriteLine("Solution found: x = " & result)
            
            ' Verify the result
            Dim verification As Long = PowerMod(g, result, p)
            Console.WriteLine("Verification: " & g & "^" & result & " ≡ " & verification & " (mod " & p & ")")
        Else
            Console.WriteLine("No solution found")
        End If
        
        Console.ReadLine()
    End Sub
End Module
```

## How the Algorithm Works:

1. **Baby Steps**: Store values of g^j mod p for j = 0, 1, 2, ..., ⌈√p⌉-1 in a dictionary
2. **Giant Steps**: Check values of h × (g^(-⌈√p⌉))^i mod p for i = 0, 1, 2, ..., ⌈√p⌉-1
3. **Collision Detection**: If a value from giant steps matches a value from baby steps, we've found the solution
4. **Solution**: x = i × ⌈√p⌉ + j where the collision occurs

## Example Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 ≡ 13 (mod 17)

Solving: 2^x ≡ 11 (mod 13)
Solution found: x = 7
Verification: 2^7 ≡ 11 (mod 13)
```

## Key Features:
- Handles modular arithmetic correctly
- Uses dictionary for efficient lookup
- Includes modular inverse calculation
- Provides verification of results
- Works with any valid prime modulus
- Time complexity: O(√p)

