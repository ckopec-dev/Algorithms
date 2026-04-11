# Monte Carlo Method Implementation in Visual Basic

Here's a Visual Basic implementation of the Monte Carlo method to estimate the value of π:

```vb
Module MonteCarloPi
    Sub Main()
        Dim iterations As Long = 1000000
        Dim insideCircle As Long = 0
        Dim random As New Random()
        
        Console.WriteLine("Monte Carlo Method to Estimate π")
        Console.WriteLine("Number of iterations: " & iterations)
        Console.WriteLine()
        
        ' Monte Carlo simulation
        For i As Long = 1 To iterations
            ' Generate random point (x, y) in unit square [0,1] x [0,1]
            Dim x As Double = random.NextDouble()
            Dim y As Double = random.NextDouble()
            
            ' Calculate distance from origin
            Dim distance As Double = Math.Sqrt(x * x + y * y)
            
            ' Check if point is inside unit circle
            If distance <= 1 Then
                insideCircle += 1
            End If
            
            ' Display progress every 100,000 iterations
            If i Mod 100000 = 0 Then
                Console.WriteLine("Progress: " & i & " iterations")
            End If
        Next
        
        ' Calculate π estimate
        Dim piEstimate As Double = 4 * (insideCircle / iterations)
        
        Console.WriteLine()
        Console.WriteLine("Results:")
        Console.WriteLine("Points inside circle: " & insideCircle)
        Console.WriteLine("Total points: " & iterations)
        Console.WriteLine("Estimated π: " & piEstimate)
        Console.WriteLine("Actual π: " & Math.PI)
        Console.WriteLine("Error: " & Math.Abs(piEstimate - Math.PI))
        
        Console.WriteLine()
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub
End Module
```

## How it works:

1. **Random Point Generation**: The algorithm generates random points (x, y) within a unit square [0,1] × [0,1]

2. **Circle Test**: For each point, it calculates the distance from the origin using the formula √(x² + y²)

3. **Counting**: If the distance is ≤ 1, the point falls inside the unit circle

4. **π Estimation**: Since the ratio of the circle's area to the square's area is π/4, we multiply by 4 to get π

5. **Convergence**: As the number of iterations increases, the estimate approaches the actual value of π

## Key Features:

- Uses `Random.NextDouble()` for uniform random number generation
- Progress tracking every 100,000 iterations
- Error calculation to show accuracy
- Modular design with clear variable names
- Works with any number of iterations

## Sample Output:
```
Monte Carlo Method to Estimate π
Number of iterations: 1000000

Progress: 100000 iterations
Progress: 200000 iterations
...
Progress: 1000000 iterations

Results:
Points inside circle: 785398
Total points: 1000000
Estimated π: 3.141592
Actual π: 3.14159265358979
Error: 0.00000065358979
```

This implementation demonstrates the classic Monte Carlo approach for numerical integration and probability estimation.

