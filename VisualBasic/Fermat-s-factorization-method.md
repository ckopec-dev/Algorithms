# Fermat's Factorization Method in Visual Basic

```vb
Module FermatFactorization
    Sub Main()
        Console.WriteLine("Fermat's Factorization Method")
        Console.WriteLine("============================")
        
        ' Example with number 5959
        Dim number As Long = 5959
        Dim factors() As Long = Factorize(number)
        
        Console.WriteLine($"Factoring {number}")
        Console.WriteLine($"Factors: {factors(0)} and {factors(1)}")
        Console.WriteLine($"Verification: {factors(0)} × {factors(1)} = {factors(0) * factors(1)}")
        
        Console.WriteLine()
        
        ' Another example
        number = 1343
        factors = Factorize(number)
        
        Console.WriteLine($"Factoring {number}")
        Console.WriteLine($"Factors: {factors(0)} and {factors(1)}")
        Console.WriteLine($"Verification: {factors(0)} × {factors(1)} = {factors(0) * factors(1)}")
        
        Console.ReadLine()
    End Sub
    
    Function Factorize(n As Long) As Long()
        Dim factors(1) As Long
        
        ' Find the smallest integer greater than or equal to sqrt(n)
        Dim a As Long = CLng(Math.Ceiling(Math.Sqrt(n)))
        Dim b2 As Long = a * a - n
        
        ' Continue until b² is a perfect square
        Do While Not IsPerfectSquare(b2)
            a += 1
            b2 = a * a - n
        Loop
        
        ' Calculate the factors
        Dim b As Long = CLng(Math.Sqrt(b2))
        factors(0) = a - b
        factors(1) = a + b
        
        Return factors
    End Function
    
    Function IsPerfectSquare(num As Long) As Boolean
        If num < 0 Then Return False
        If num = 0 Then Return True
        
        Dim sqrtNum As Long = CLng(Math.Sqrt(num))
        Return sqrtNum * sqrtNum = num
    End Function
End Module
```

## How the Algorithm Works:

1. **Start with the number** to be factored (n)
2. **Find the smallest integer** a such that a² ≥ n
3. **Calculate b² = a² - n**
4. **Check if b² is a perfect square**
5. **If yes**: The factors are (a-b) and (a+b)
6. **If no**: Increment a and repeat steps 3-5

## Example Output:
```
Fermat's Factorization Method
============================
Factoring 5959
Factors: 59 and 101
Verification: 59 × 101 = 5959

Factoring 1343
Factors: 17 and 79
Verification: 17 × 79 = 1343
```

## Key Features:
- Uses `Math.Ceiling` to find the starting value of 'a'
- Implements `IsPerfectSquare` function to check if a number is a perfect square
- Returns an array containing the two factors
- Handles edge cases like perfect squares
- Includes verification of results

The algorithm is most efficient when the two factors are close to each other, making it particularly useful for numbers that are products of nearly equal factors.

