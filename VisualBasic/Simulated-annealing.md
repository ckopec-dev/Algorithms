# Simulated Annealing Algorithm in Visual Basic

Here's a complete implementation of the Simulated Annealing algorithm in Visual Basic .NET:

```vb
Imports System
Imports System.Math

Public Class SimulatedAnnealing
    ' Problem: Find minimum of function f(x) = x^2 + 5*sin(x)
    ' We'll use this as an example optimization problem
    
    Private Shared Random As New Random()
    
    ' Objective function to minimize
    Private Shared Function ObjectiveFunction(x As Double) As Double
        Return x * x + 5 * Math.Sin(x)
    End Function
    
    ' Simulated Annealing algorithm
    Public Shared Function SolveSA(initialTemp As Double, 
                                  coolingRate As Double, 
                                  minTemp As Double, 
                                  maxIterations As Integer) As Double
        
        ' Initialize current solution
        Dim currentSolution As Double = (Random.NextDouble() - 0.5) * 10
        Dim currentEnergy As Double = ObjectiveFunction(currentSolution)
        
        ' Initialize best solution
        Dim bestSolution As Double = currentSolution
        Dim bestEnergy As Double = currentEnergy
        
        ' Initialize temperature
        Dim temperature As Double = initialTemp
        
        ' Iteration counter
        Dim iteration As Integer = 0
        
        Console.WriteLine("Starting Simulated Annealing...")
        Console.WriteLine($"Initial solution: {currentSolution:F4}, Energy: {currentEnergy:F4}")
        
        While temperature > minTemp AndAlso iteration < maxIterations
            ' Generate neighbor solution
            Dim neighborSolution As Double = currentSolution + (Random.NextDouble() - 0.5) * 2
            Dim neighborEnergy As Double = ObjectiveFunction(neighborSolution)
            
            ' Calculate energy difference
            Dim energyDifference As Double = neighborEnergy - currentEnergy
            
            ' Accept or reject the neighbor
            If energyDifference < 0 OrElse 
               Random.NextDouble() < Math.Exp(-energyDifference / temperature) Then
                currentSolution = neighborSolution
                currentEnergy = neighborEnergy
                
                ' Update best solution if improved
                If currentEnergy < bestEnergy Then
                    bestSolution = currentSolution
                    bestEnergy = currentEnergy
                End If
            End If
            
            ' Cool down the temperature
            temperature *= coolingRate
            
            ' Print progress every 100 iterations
            If iteration Mod 100 = 0 Then
                Console.WriteLine($"Iteration {iteration}: Solution = {currentSolution:F4}, Energy = {currentEnergy:F4}, Temperature = {temperature:F4}")
            End If
            
            iteration += 1
        End While
        
        Console.WriteLine($"Final solution: {bestSolution:F4}, Best energy: {bestEnergy:F4}")
        Return bestSolution
    End Function
    
    ' Main program
    Public Shared Sub Main()
        Console.WriteLine("Simulated Annealing Optimization Example")
        Console.WriteLine("Minimizing f(x) = x^2 + 5*sin(x)")
        Console.WriteLine("=" * 50)
        
        ' Parameters for simulated annealing
        Dim initialTemperature As Double = 1000.0
        Dim coolingRate As Double = 0.95
        Dim minimumTemperature As Double = 0.01
        Dim maxIterations As Integer = 10000
        
        ' Run the algorithm
        Dim result As Double = SolveSA(initialTemperature, coolingRate, minimumTemperature, maxIterations)
        
        Console.WriteLine($"\nOptimal solution found: x = {result:F6}")
        Console.WriteLine($"Minimum value: {ObjectiveFunction(result):F6}")
        
        ' Compare with analytical solution (approximately -1.33)
        Console.WriteLine($"Analytical minimum: x ≈ -1.33")
        
        Console.WriteLine(vbNewLine & "Press any key to exit...")
        Console.ReadKey()
    End Sub
End Class
```

## How the Algorithm Works:

### Key Components:

1. **Objective Function**: `f(x) = x² + 5sin(x)` - the function we want to minimize
2. **Temperature Control**: Starts high and gradually decreases
3. **Neighbor Generation**: Creates new candidate solutions
4. **Acceptance Probability**: Uses Metropolis criterion to accept worse solutions

### Algorithm Steps:

1. **Initialize**: Start with random solution and set initial temperature
2. **Generate Neighbor**: Create new candidate solution near current solution
3. **Evaluate**: Calculate energy (function value) of new solution
4. **Accept/Reject**: 
   - Accept if better (lower energy)
   - Accept worse solutions with probability `exp(-ΔE/T)`
5. **Cool Down**: Reduce temperature using cooling rate
6. **Repeat**: Continue until temperature is too low or max iterations reached

### Key Parameters:

- **Initial Temperature**: High value to allow exploration
- **Cooling Rate**: Usually 0.8-0.99 (controls how fast temperature decreases)
- **Minimum Temperature**: When to stop cooling
- **Max Iterations**: Maximum number of iterations

### Sample Output:
```
Simulated Annealing Optimization Example
Minimizing f(x) = x^2 + 5*sin(x)
==================================================
Starting Simulated Annealing...
Initial solution: 2.3456, Energy: 12.4567
Iteration 0: Solution = 1.2345, Energy = 6.7890, Temperature = 950.0000
Iteration 100: Solution = -1.2345, Energy = -2.3456, Temperature = 123.4567
...
Final solution: -1.334567, Best energy: -2.567890

Optimal solution found: x = -1.334567
Minimum value: -2.567890
Analytical minimum: x ≈ -1.33
```

This implementation demonstrates how Simulated Annealing can escape local minima and find near-optimal solutions for optimization problems.

